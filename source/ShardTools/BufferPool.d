/// Provides a BufferPool for efficient means of pooling buffers in a thread-safe way.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.BufferPool;
private import std.stdio;
private import ShardTools.ConcurrentStack;
private import std.algorithm;
private import std.exception;
private import ShardTools.Stack;
private import std.math;
private import ShardTools.LinkedList;
private import core.atomic;
public import ShardTools.Buffer;
private import ShardTools.SortedList;
public import std.outbuffer;
private import std.container;
import ShardTools.Udas;

/// Provides access to a pool of reuseable buffers.
/// All methods in this class are O(1), thread-safe, and lock-free.
final class BufferPool  {

// TODO: Allow support for Paged buffers. Maybe. Maybe not. Probably not. Maybe a constructor that takes alignment though, or allocator.
// TODO: Try to implement self-load-balancing. Maybe have a balance checker in another thread that checks every few seconds.
//		 Then, when it sees something has too many or too little, it pops some buffers, splits and combines them, and pushes them back in.
//       A thread polling is a bad idea, it should actually be every X calls perform a check, and then maybe dispatch a task to clean it up.

// TODO: Ideally this class shouldn't be needed at all. Instead, usinga Buffer should handle this internally.
// That way we don't have to manually release memory.

// TODO: Rewrite the acquire / release methods to not be terrible.
// It rather defeats the purpose at the moment since ConcurrentStack allocates.
// Instead, allocate an extra header along with the memory allocated for the buffer when needed.
// Essentially, allocate __traits(classInstanceSize, Buffer) + BufferHeader.sizeof + RoundedCapacity,
// then pass in from RoundedCapacity to end for array, emplace buffer at __traits spot, and initialize
// struct containing next pointer at BufferHeader.
// Now we have just one allocation if empty (unless we go over Capacity), and to get when not empty none.

public:

	shared static this() {		
		// TODO: A more sane default determined by amount of available memory.
		_Global = new BufferPool(1024 * 1024 * 64, 1024 * 1024 * 2); // 64 megabytes max by default seems safe, with 2 megabytes max per buffer.
	}

	/// Initializes a new instance of the BufferPool object.	
	this(size_t MaxSize, size_t MaxIndividualSize) {
		this._MaxSize = MaxSize;
		this._MaxIndividualSize = MaxIndividualSize;
		foreach(Index; 0 .. NumBuffers)
			Buffers[Index] = new BufferCollection();		
	}

	/// Gets a global BufferPool to be used.
	@property static BufferPool Global() {
		return _Global;
	}

	/// Gets or sets the maximum size, in bytes, of the buffers that the pool holds. If buffers are resized past this size, they get split upon release.
	/// Changes will not take effect immediately, but the pool will balance itself out eventually.
	/// It is possible for this pool to go over the maximum size in certain situations, mostly from race conditions. It will still balance itself out, so this is generally acceptable.	
	@property size_t MaxSize() const {		
		return _MaxSize;
	}

	/// Ditto
	@property void MaxSize(size_t Value) {
		_MaxSize = Value;
	}

	/// Gets or sets the maximum size of each individual buffer in the pool.
	/// To prevent the entire pool being taken up from one buffer, buffers past this size being released will be ignored.
	@property size_t MaxIndividualSize() const {
		return _MaxIndividualSize;
	}

	/// Ditto
	@property void MaxIndividualSize(size_t Value) {
		_MaxIndividualSize = Value;
	}

	/// Gets the total number of bytes currently being stored.
	/// This is the combined size of all the buffers in the pool that are not currently in use.
	@property size_t BytesStored() const {
		return CurrentSize;
	}

	/// Acquires a buffer from the pool, or creates a new buffer if necessary.
	/// Params:
	/// 	NumBytes = The minimum number of bytes that the buffer should contain.
	Buffer Acquire(size_t NumBytes) {							
		size_t Index = IndexForBuffer(NumBytes);
		assert(Index >= 0 && Index < 32);				
		// If we don't have a buffer of the exact size needed, try a couple sizes above first.
		// If even then, try one or two sizes below with a resize.
		// Note that this leads to buffers gradually growing larger when callers request a smaller size than they use. This is generally fine. It is the large buffers that take the most time after all.				
		size_t Max = min(NumBuffers, Index + 30);			
		for(size_t i = Index; i < Max; i++) {				
			Buffer buff = Buffers[i].Pop();
			if(buff) {				
				OnBufferRemoved(buff);
				return buff;
			}
		}
		ptrdiff_t Min = max(0, cast(ptrdiff_t)(Index - 20));
		for(ptrdiff_t i = Index - 1; i >= Min; i--) {				
			Buffer buff = Buffers[i].Pop();				
			if(buff !is null) {				
				OnBufferRemoved(buff); // We want this to happen before reserve.				
				buff.Reserve(NumBytes);
				return buff;
			}
		}				
		return new Buffer(NumBytes);
	}

	/// Releases the specified buffer, putting it back into the pool if the pool is not full.
	/// If the buffer is too large to fit, it shall be discarded because it is not possible to slice the buffer without maintaining a reference to the whole data.	
	/// Params:
	/// 	Buffer = The buffer to release in to the pool.
	/// 	ClearOldData = Whether to zero out the old data in the buffer.
	void Release(Buffer Buffer, bool ClearOldData = false) {		
		// Race conditions are fine for this check.		
		if(CurrentSize + Buffer.Capacity > _MaxSize || Buffer.Capacity > _MaxIndividualSize) {			
			return;
		}

		enforce(!Buffer.IsDisposed);
		size_t Index = IndexForBuffer(Buffer.Capacity);
		BufferCollection Collection = Buffers[Index];
		Buffer.Reuse(ClearOldData);

		OnBufferAdded(Buffer);							
		Collection.Push(Buffer);			
	}
	
private:
	static __gshared BufferPool _Global;

	size_t _MaxSize;
	size_t _MaxIndividualSize;
	shared size_t CurrentSize;	
	shared size_t StoredBuffers;	

	pure static size_t IndexForBuffer(size_t NumBytes) {
		if(NumBytes == 0)
			return 0;
		return cast(size_t)ceil(log2(NumBytes));
	}

	void OnBufferRemoved(Buffer buff) {				
		atomicOp!("-=", size_t, size_t)(CurrentSize, buff.Capacity);
		atomicOp!("-=", size_t, size_t)(StoredBuffers, cast(size_t)1);		
	}

	void OnBufferAdded(Buffer buff) {				
		atomicOp!("+=", size_t, size_t)(CurrentSize, buff.Capacity);
		atomicOp!("+=", size_t, size_t)(StoredBuffers, cast(size_t)1);		
	}

	// The way this works is that our Buffers are guaranteed to have a capacity that's a power of two.
	// This means we have a fixed number of buffers known at compile time.
	// So, when requesting a buffer, we can get the collection of buffers associated with it in O(1) time and lock-free by using the next power of two.
	// Then, we make that collection be O(1) and lock-free too, and thus the entire Acquire and Release methods are O(1) and lock-free.
	// Things like max size checks are done in a way that allows race conditions. This is fine, it doesn't need to be exact.
	
	alias ConcurrentStack!Buffer BufferCollection;		
	enum size_t NumBuffers = size_t.sizeof * 8;	

	// While we don't necessarily need all of these buffers, we'd have some complications with MaxSize being set if we just calculated the amount needed at runtime.
	// It's not much overhead anyways considering you usally only have one buffer pool.
	BufferCollection[NumBuffers] Buffers;	

	@name("Index Calculations")
	unittest {
		assert(IndexForBuffer(0) == 0);
		assert(IndexForBuffer(2) == 1);
		assert(IndexForBuffer(3) == 2);
		assert(IndexForBuffer(pow(2, 16) - 1) == 16);
		assert(IndexForBuffer(pow(2, 16)) == 16);
		assert(IndexForBuffer(pow(2, 16) + 1) == 17);
	}
}