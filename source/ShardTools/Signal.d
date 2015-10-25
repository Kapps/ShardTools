/// An alternate implementation of signal-slots that requires no GC presence and allows for tag parameters.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2015 Ognjen Ivkovic
module ShardTools.Signal;
import std.typecons;
import ShardTools.Untyped;
//import std.experimental.allocator.mallocator;
import core.stdc.stdlib;
import core.memory;
import core.exception : onOutOfMemoryError;
import std.exception;
import std.algorithm;
import std.bitmanip;
import ShardTools.SpinLock;

/// An alternate implementation of signal-slots that requires no GC presence and allows for tag parameters.
/// This structure is thread-safe.
struct SignalImpl(Args...) {

	/// A delegate for the callback invoked for a slot.
	alias SignalCallback = void delegate(void* tag, Args args);

	/// Signals do not support copying nor assignment.
	@disable this(this);

	~this() {
		//Mallocator.instance.deallocate(slots);
		free(slots.ptr);
	}

	/// Attaches the given callback to this signal with the specified tag.
	/// The tag is passed in to the callback when emitted.
	/// The same callback is not allowed to be attached multiple times, even with a different tag.
	void attach(void* tag, SignalCallback callback) {
		sync.lock();
		scope(exit)
			sync.unlock();
		assert(!slots[0..length].any!(c=>c.callback == callback));
		Slot slot = {
			callback = cast(SignalCallback)callback,
			tag = cast(void*)tag
		};
		if(length >= slots.length) {
			if(slots.ptr)
				GC.removeRange(slots.ptr);
			/+void[] dat = cast(void[])slots;
			if(!Mallocator.instance.reallocate(dat, slots.length + 2))
				onOutOfMemoryError();
			slots = cast(Slot[])dat;+/
			auto newSize = (slots.length + 2) * Slot.sizeof;
			slots = cast(Slot[])(realloc(slots.ptr, newSize)[0..newSize]);
			GC.addRange(slots.ptr, slots.length * Slot.sizeof, typeid(Slot[]));
		}
		slots[length] = slot;
		length++;
	}

	void attach(void* tag, void delegate() callback) {

	}

	/// Removes the given slot from this signal.
	/// Returns the tag associated with the given callback.
	void* detach(SignalCallback callback) {
		sync.lock();
		scope(exit)
			sync.unlock();
		size_t index = slots[0..length].countUntil!(c=>c.callback == callback);
		if(index >= length)
			return null;
		void* tag = slots[index].tag;
		slots[index] = slots[length - 1];
		slots[length - 1] = Slot.init;
		length--;
		return tag;
	}

	/// Emits the callback on all slots using the given arguments.
	/// The order the callbacks are invoked in may change between calls.
	void emit(Args args) {
		sync.lock();
		scope(exit)
			sync.unlock();
		slots[0..length].each!(slot => slot.callback(slot.tag, args));
	}


private:
	Slot[] slots;
	size_t length;
	SlimSpinLock sync;

	struct Slot {
		void delegate(void*, Args) callback;
		void* tag;
	}
}

/// Basic Signal example
unittest {
	int lastVal = 0;
	string lastStr = "";
	Signal!(int, string) signal;
	// Can attach a basic callback.
	signal.SignalCallback callback = (tag, i, s) {
		assert(*(cast(int*)tag) == lastVal);
		lastVal = i;
		lastStr = s;
	};
	signal.attach(&lastVal, callback);
	// Emit the signal and observe our arguments change.
	assert(lastVal == 0);
	assert(lastStr == "");
	signal.emit(2, "test");
	assert(lastVal == 2);
	assert(lastStr == "test");
	signal.emit(4, "abc");
	assert(lastVal == 4 && lastStr == "abc");
	// Detaching returns the original tag.
	auto tag = signal.detach(callback);
	assert(tag && *cast(int*)tag == 4);
	// Emitting after detaching all signals has no observable effect.
	signal.emit(1, "a");
	assert(lastVal == 4 && lastStr == "abc");
	// Reattaching and attaching a second signal invokes both.
	signal.attach(&lastVal, callback);
	// While ordering is not guaranteed, for the purpose of demonstration we will assume it is.
	// This is not a safe assumption, as ordering changes between adding and removing signals.
	signal.SignalCallback cb2 = (tag, i, s) {
		assert(*(cast(int*)tag) == lastVal);
		lastVal *= i;
		lastStr ~= s;
	};
	signal.attach(&lastVal, cb2);
	signal.emit(10, "ad");
	assert(lastVal == 100 && lastStr == "adad");

}

alias Signal(Args...) = std.typecons.RefCounted!(SignalImpl!(Args));