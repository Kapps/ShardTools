/// Provides a single producer single consumer asynchronous range.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.AsyncRange;
import std.exception;
import ShardTools.SignaledTask;
import std.parallelism;
import std.math;
import std.array;
import ShardTools.Buffer;
import ShardTools.BufferPool;
import std.traits;
import std.range;
import std.algorithm;
import std.datetime;
import core.time;
import ShardTools.Untyped;
import ShardTools.ExceptionTools;

/// Indicates the status of a producer in an AsyncRange.
enum ProducerStatus {
	/// More data is available to be produced.
	more = 0,
	/// No more data is available after this production is consumed.
	complete = 1
}

/// An alias to the delegate used to notify that a producer has finished generating data.
alias void delegate(ProducerStatus status, size_t elementsProduced) ProducerCompletionCallback;
/// An alias to the delegate used to notify that a consumer has finished consuming data.
alias void delegate() ConsumerCompletionCallback;

/// Provides an asynchronous buffered input source that can be consumed
/// by a single consumer, completing an AsyncAction for when the range is depleted.
///
/// To produce elements, the $(D producer) delegate is called with a buffer to write elements
/// to, and a callback to invoke once all elements have been written. The buffer to write to
/// has a fixed capacity, The producer then returns whether more data is available.
/// 
/// To begin consuming elements, $(D consume) is invoked with a delegate to be called once sufficient
/// data is avilable to be consumed. Once all data is consumed, the callback passed to the consumer
/// delegate should be invoked to indicate that the consumer is available for more data.
/// 
/// In both situations, the callback to signal completion must be invoked exactly once. In any other
/// situation, data corruption or deadlocks will occur. This class does not attempt to check for such errors.
/// Neither the producer nor consumer should perform blocking operations. Any such operations must be
/// performed in a background thread or handled asynchronously in a different fashion. At least one
/// element must be produced or consumed in either situation. The first parameter to both callbacks
/// is a user-defined state that is unused by the range except to pass in to the callbacks. It is allowed
/// to be different for both callbacks.
/// 
/// This action may be aborted. In such a scenario, all that will occur is no more calls will be
/// made to the producer or consumer. It is expected that if special handling is required that the caller
/// will provide it using the $(D NotifyOnComplete) callback.
/// 
/// At the moment, the implementation of this class is subpar because the producer is not requested
/// to generate further data until the consumer completes, causing a delay if the producer requires
/// an asynchronous operation. In the future this will likely be changed to continue generating data
/// while the consumer is consuming.
class AsyncRange(T) : AsyncAction {

	/// An alias to the delegate used for consuming data.
	alias void delegate(Untyped consumerState, T[] data, ConsumerCompletionCallback callback) ConsumerDelegate;
	/// An alias to the delegate used for producing data.
	alias void delegate(Untyped producerState, RangeBuffer!T buffer, ProducerCompletionCallback callback) ProducerDelegate;

	static if(!is(T == class) && !is(T == interface))
		enum EstimatedElementSize = T.sizeof;
	else
		enum EstimatedElementSize = 128;

	/// Creates a new AsyncRange with the given delegate used to produce or consume elements.
	this(ProducerDelegate producer, Untyped producerState, ConsumerDelegate consumer, Untyped consumerState, size_t bufferSize = max(1, 4096 / EstimatedElementSize)) {
		this._bufferSize = bufferSize;
		this._producer = producer;
		this._consumer = consumer;
		this._producerState = producerState;
		this._consumerState = consumerState;
	}

protected:

	/// Allocates buffers and notifies the producer to begin generating data.
	override void PerformStart() {
		auto buf = BufferPool.Global.Acquire(_bufferSize * T.sizeof);
		this._buffer = new RangeBuffer!T(buf);
		_producer(_producerState, _buffer, &onProducerComplete);
	}

	/// Stops notifying the producer or consumer of any new data.
	override void PerformAbort() {
		this._isAborted = true;
		// If we get aborted, we have to wait for the next producer or consumer to complete before releasing our buffer.
	}

	/// Called when the producer finishes produces data.
	void onProducerComplete(ProducerStatus status, size_t elementsProduced) {
		if(!_isAborted) {
			if(status == ProducerStatus.complete)
				_waitToComplete = true;
			if(elementsProduced == 0 && status == ProducerStatus.more)
				Abort(Untyped(new InvalidOperationException("Received no data, yet the producer indicated more data was available.")));
			else if(elementsProduced) {
				T[] elements = _buffer.buffer[0 .. elementsProduced];
				_consumer(_consumerState, elements, &onConsumerComplete);
			}
		} else {
			enforce(_buffer !is null);
			BufferPool.Global.Release(_buffer._buffer);
		}
	}

	/// Called when the consumer finishes consuming data.
	void onConsumerComplete() {
		if(!_isAborted) {
			if(_waitToComplete)
				NotifyComplete(CompletionType.Successful, Untyped.init);
			_producer(_producerState, _buffer, &onProducerComplete);
		} else {
			enforce(_buffer !is null);
			BufferPool.Global.Release(_buffer._buffer);
		}
	}

	/// Releases the buffer used for the 
	override void OnComplete(CompletionType Status) {
		if(!_isAborted) {
			enforce(_buffer);
			BufferPool.Global.Release(_buffer._buffer);
		}
	}

private:
	RangeBuffer!T _buffer;
	size_t _bufferSize;
	ConsumerDelegate _consumer;
	ProducerDelegate _producer;
	Untyped _producerState;
	Untyped _consumerState;
	bool _isAborted = false;
	bool _waitToComplete = false;
}

/// Simple AsyncRange example to lazily copy between an array in the background.
/// Under normal circumstances your data would not be in memory like this, but 
/// rather loaded and stored asynchronously. This makes for a simpler example however.
unittest {
	enum size = 1_000_000;
	int[] elements = iota(0, size).array;
	int[] copy = new int[size];
	size_t offset = 0;
	size_t copyOffset = 0;
	
	auto producer = (Untyped _, RangeBuffer!int buffer, ProducerCompletionCallback callback) {
		size_t count = min(buffer.capacity, size - offset);
		int[] curr = elements[offset .. offset + count];
		buffer.buffer[0..count] = curr[];
		offset += count;
		callback(offset >= size ? ProducerStatus.complete : ProducerStatus.more, count);
	};
	
	auto consumer = (Untyped _, int[] elements, ConsumerCompletionCallback callback) {
		copy[copyOffset .. copyOffset + elements.length] = elements[];
		copyOffset += elements.length;
		callback();
	};
	
	auto range = new AsyncRange!int(producer, Untyped.init, consumer, Untyped.init);
	range.Start();
	range.WaitForCompletion(10.seconds);
	assert(elements == copy);
}

/// Provides a buffer for a producer to write elements to.
final class RangeBuffer(T) {

	/// Returns the number of elements that can be stored within the buffer.
	@property size_t capacity() {
		return _buffer.Capacity;
	}

	/// Returns a location in memory to write elements to.
	@property T* buffer() {
		return cast(T*)_buffer.FullData.ptr;
	}

	/// Creates a new RangeBuffer wrapping the given Buffer.
	/// The buffer will not automatically be released back to the BufferPool.
	this(Buffer buffer) {
		enforce(buffer && buffer.Capacity > 0);
		this._buffer = buffer;
	}

private:
	Buffer _buffer;
}