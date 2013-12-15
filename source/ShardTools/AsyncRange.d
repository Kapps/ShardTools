/// Provides a single producer single consumer asynchronous range.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.AsyncRange;
import std.exception;
import ShardTools.SignaledTask;
import std.parallelism;

/// Provides an asynchronous buffered input range that can be consumed
/// by a single consumer, returning an AsyncAction for when the range is depleted.
/// $(RED This module is not yet implemented.)
class AsyncRange(T) {

	/// Creates a new AsyncRange with the given delegate used to produce
	/// elements to consume. Elements are produced lazily, usually
	/// in multiples of $(D bufferSize).
	this(T delegate() producer, size_t bufferSize = 32, ThreadPool workerPool = threadPool) {
		enforce(bufferSize > 0);
		this.producer = producer;
		this.buffer.capacity = bufferSize;
		this.pool = workerPool;
	}

	/// Begins consuming elements from this range with the given delegate, returning 
	/// an AsyncAction that completes once all elements have been consumed.
	AsyncAction consume(void delegate(T) dg) {
		enforce(dg);
		enforce(task is null);
		task = new SignaledTask();
		assert(0, "Not implemented.");
	}


private:
	T[] buffer;
	size_t bufferSize;
	ThreadPool pool;
	void delegate(T) consumer;
	T delegate() producer;
	SignaledTask task;
}

