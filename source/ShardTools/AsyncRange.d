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
import ShardTools.Udas;
import core.atomic;

/// Indicates the status of a producer in an AsyncRange.
enum ProducerStatus {
	/// More data is available to be produced.
	more = 0,
	/// No more data is available after this production is consumed.
	complete = 1
}

/// An alias to the delegate used to notify that a producer has finished generating data.
alias void delegate(ProducerStatus status, void[] elementsProduced) ProducerCompletionCallback;
/// An alias to the delegate used to notify that a consumer has finished consuming data.
alias void delegate() ConsumerCompletionCallback;

/// Provides an asynchronous buffered input source that can be consumed
/// by a single consumer, completing an AsyncAction for when the range is depleted.
///
/// To produce elements, the $(D producer) delegate is called with a callback containing elements
/// that have been produced. The actual amount of elements to produce is up to the caller. 
/// The $(D producer) delegate returns whether more data is available to be produced.
/// The elements produced are allowed to be stored indefinitely by the consumer.
/// 
/// To begin consuming elements, $(D consume) is invoked with a delegate to be called once sufficient
/// data is avilable to be consumed. Once all data is consumed, the callback passed to the consumer
/// delegate should be invoked to indicate that the consumer is available for more data.
/// 
/// In both situations, the callback to signal completion must be invoked exactly once. In any other
/// situation, data corruption or deadlocks will occur. This class does not attempt to check for such errors.
/// Neither the producer nor consumer should perform blocking operations. Any such operations must be
/// performed in a background thread or handled asynchronously in a different fashion. At least one
/// element must be produced or consumed in either situation unless ProducerStatus is complete. 
/// The first parameter to both callbacks is a user-defined state that is unused by the 
/// range except to pass in to the callbacks. It is allowed to be different for both callbacks.
/// 
/// This action may be aborted. In such a scenario, all that will occur is no more calls will be
/// made to the producer or consumer. It is expected that if special handling is required that the caller
/// will provide it using the $(D NotifyOnComplete) callback.
class AsyncRange(T) : AsyncAction {

	/// An alias to the delegate used for consuming data.
	alias void delegate(Untyped consumerState, T[] data, ProducerStatus status, ConsumerCompletionCallback callback) ConsumerDelegate;
	/// An alias to the delegate used for producing data.
	alias void delegate(Untyped producerState, ProducerCompletionCallback callback) ProducerDelegate;

	/// Creates a new AsyncRange with the given delegate used to produce or consume elements.
	this(ProducerDelegate producer, Untyped producerState, ConsumerDelegate consumer, Untyped consumerState) {
		this._producer = producer;
		this._consumer = consumer;
		this._producerState = producerState;
		this._consumerState = consumerState;
	}

protected:

	/// Allocates buffers and notifies the producer to begin generating data.
	override void PerformStart() {
		_producer(_producerState, &onProducerComplete);
	}

	/// Stops notifying the producer or consumer of any new data.
	override void PerformAbort() {
		this._isAborted = true;
	}

	/// Called when the producer finishes produces data.
	void onProducerComplete(ProducerStatus status, void[] elementsProduced) {
		T[] elements = cast(T[])elementsProduced;
		if(!_isAborted) {
			if(status == ProducerStatus.complete)
				_waitToComplete = true;
			if(elements.length == 0 && status == ProducerStatus.more)
				Abort(Untyped(new InvalidOperationException("Received no data, yet the producer indicated more data was available.")));
			else {
				_consumer(_consumerState, elements, status, &onConsumerComplete);
			}
		}
	}

	/// Called when the consumer finishes consuming data.
	void onConsumerComplete() {
		if(!_isAborted) {
			if(_waitToComplete)
				NotifyComplete(CompletionType.Successful, Untyped.init);
			else {
				// HACK: Every X consumes, we'll invoke the producer on a TaskPool thread.
				// This is because otherwise we risk a stack overflow if the consumer and producer both complete in the same method.
				if((atomicOp!"+="(_consumesComplete, 1) % 25) == 0)
					taskPool.put(task(_producer, _producerState, &onProducerComplete));
				else
					_producer(_producerState, &onProducerComplete);
			}
		}
	}

private:
	ConsumerDelegate _consumer;
	ProducerDelegate _producer;
	Untyped _producerState;
	Untyped _consumerState;
	bool _isAborted = false;
	bool _waitToComplete = false;
	shared size_t _consumesComplete = 0;
}

/// Simple AsyncRange example to lazily copy between an array in the background.
/// Under normal circumstances your data would not be in memory like this, but 
/// rather loaded and stored asynchronously. This makes for a simpler example however.
@name("Basic Usage")
unittest {
	enum size = 1_000_000;
	int[] elements = iota(0, size).array;
	int[] copy = new int[size];
	size_t offset = 0;
	size_t copyOffset = 0;
	
	auto producer = (Untyped _, ProducerCompletionCallback callback) {
		size_t count = min(1024, size - offset);
		int[] curr = elements[offset .. offset + count];
		offset += count;
		callback(offset >= size ? ProducerStatus.complete : ProducerStatus.more, curr);
	};
	
	auto consumer = (Untyped _, int[] elements, ProducerStatus status, ConsumerCompletionCallback callback) {
		copy[copyOffset .. copyOffset + elements.length] = elements[];
		copyOffset += elements.length;
		callback();
	};
	
	auto range = new AsyncRange!int(producer, Untyped.init, consumer, Untyped.init);
	range.Start();
	range.WaitForCompletion(10.seconds);
	assert(elements == copy);
}