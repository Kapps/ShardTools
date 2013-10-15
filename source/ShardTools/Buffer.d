module ShardTools.Buffer;
private import std.algorithm;
private import std.traits;
private import std.array;
private import std.exception;
private import core.memory;
private import std.math;
private import std.c.stdlib;
private import std.c.string;


/// Provides a reusable buffer of memory designed for efficient writes.
final class Buffer  {
	
	// TODO: Allow support for creating the array by using pages of memory.
	
public:
	/// Initializes a new instance of the Buffer object.
	this(size_t InitialSize = 64) {
		Reserve(InitialSize);
	}
	
	private this(ubyte[] Existing) {
		this._Data = Existing;		
		this._Position = _Data.length;
	}
	
	/// Returns the number of bytes written to the buffer.	
	/// This resets to zero when Reuse is called, even though old data may still remain.
	@property size_t Count() const {
		return _Position;
	}
	
	/// Gets a reference to the underlying data.
	/// This value should not be stored. It is not guaranteed that this slice refers to the same data after any further operations on the Buffer.
	@property ubyte[] Data() {
		CheckDisposed();
		return _Data[0 .. _Position];
	}
	
	/// Ditto
	@property const(ubyte)[] Data() const {
		CheckDisposed();
		return cast(const)_Data[0 .. _Position];
	}
	
	/// Indicates whether the Buffer has been disposed of due to a Split.
	@property bool IsDisposed() const {
		return _Disposed;
	}
	
	/// Gets a reference to all of the underlying data.
	/// This operation is unsafe and should only be used if you intend to manually write to the buffer.
	/// It is important to never directly read from the buffer, as it contains uninitialized data.
	@property ubyte[] FullData() {
		CheckDisposed();
		return _Data;
	}
	
	/// Gets the maximum number of bytes this Buffer is capable of storing prior to needing a resize.
	/// This is guaranteed to be a power of two.
	@property size_t Capacity() {
		return _Data.length;
	}
	
	/// Advances the position by the given number of bytes without actually writing any data.
	/// This operation is unsafe and should only be used after manually writing to FullData.
	/// Improper use (such as skipping past unread bytes) will result in uninitialized data.
	/// Params:
	/// 	Bytes = The number of bytes to skip ahead.
	void AdvancePosition(size_t Bytes) {
		CheckDisposed();
		enforce(_Position + Bytes <= _Data.length);
		_Position += Bytes;
	}
	
	/// Ensures that there is at least Bytes bytes remaining in the buffer.
	/// Params:
	/// 	Bytes = The number of bytes to reserve.
	void Reserve(size_t Bytes) {
		CheckDisposed();
		if(_Position + Bytes <= _Data.length)
			return;
		float L2 = log2 (_Position + Bytes);
		if(L2 != cast(int)L2)
			L2 = (cast(int)L2) + 1;		
		// Can leave it unininitialized because we don't allow access to the underlying data that hasn't been written to yet.
		ubyte[] NewData = uninitializedArray!(ubyte[])(1 << cast(int)L2);		
		GC.clrAttr(NewData.ptr, GC.BlkAttr.NO_SCAN);		
		memcpy(NewData.ptr, _Data.ptr, _Position);
		this._Data = NewData;
	} unittest {
		Buffer b = new Buffer(4);
		ubyte* Last = b._Data.ptr;
		assert(b._Data.length == 4);
		b.Reserve(3);
		b.Write(new ubyte[3]);
		assert(b._Data.ptr == Last);
		assert(b._Data.length == 4);		
		b.Reserve(1);
		b.Write(new ubyte[1]);
		assert(b._Data.ptr == Last);
		assert(b._Data.length == 4);
		b.Write(new ubyte[1]);
		b.Reserve(1);				
		assert(b._Data.ptr != Last);
		assert(b._Data.length == 8);
		assert(b._Position == 5);
		Last = b._Data.ptr;
		
		assert(b.Data == new ubyte[5]);
	}
	
	/// Resuses the buffer by setting the position back to zero, and optionally zeroing out the old data.
	void Reuse(bool ClearOldData) {
		CheckDisposed();
		if(ClearOldData)
			memset(_Data.ptr, 0, _Position);
		_Position = 0;		
	} unittest {
		Buffer b = new Buffer();
		b.Write(uninitializedArray!(ubyte[])(4096));
		assert(b._Position == 4096);
		b.Reuse(true);
		assert(b._Position == 0);
		assert(b._Data[0..4096] == new ubyte[4096]);
	}
	
	/// Creates a new Buffer that writes to the data passed in, positioned at the end of the array.
	/// To go back to the beginning and overwrite the existing data, call Reuse on the resulting Buffer.
	static Buffer FromExistingData(ubyte[] Data) {
		return new Buffer(Data);
	}
	
	/// Splits this Buffer into as many buffers as can hold, of size ChunkSize.
	/// If the current size of the buffer is not a multiple of ChunkSize, one more buffer is returned to hold the remaining data.
	/// This Buffer is no longer allowed to be used afterwards.
	/// Params:
	/// 	ChunkSize = The number of bytes each buffer has allocated to it.
	/// 	ClearData = If true, the newly created buffers have their contents zeroed out.
	Buffer[] Split(size_t ChunkSize, bool ClearData) {
		CheckDisposed();
		if(ClearData)
			memset(_Data.ptr, 0, _Position);		
		_Disposed = true;
		size_t BytesRead = 0;
		size_t NumBuffers = _Position / ChunkSize;
		if(_Position % ChunkSize != 0)
			NumBuffers++;
		Buffer[] Result = new Buffer[NumBuffers];
		size_t Index = 0;
		while(BytesRead < _Position) {
			size_t NextChunkSize = min(ChunkSize, _Position - BytesRead);
			Buffer NextBuffer = Buffer.FromExistingData(this._Data[BytesRead .. BytesRead + NextChunkSize]);
			Result[Index++] = NextBuffer;
			BytesRead += NextChunkSize;
		}
		return Result;
	} unittest {
		Buffer FirstBuffer = new Buffer(8192);
		FirstBuffer.Write(new ubyte[8192]);
		Buffer[] Split = FirstBuffer.Split(2048, false);
		assert(Split.length == 4);
		assert(Split[0].Data.ptr == FirstBuffer._Data.ptr);
		assert(Split[3].Data.ptr == FirstBuffer._Data.ptr + (2048 * 3));
		foreach(Buffer Buff; Split)
			assert(Buff.Data.length == 2048);		
		
		Buffer Second = new Buffer(8193);
		Second.Write(new ubyte[8193]);
		Split = Second.Split(2048, false);
		assert(Split.length == 5);
		assert(Split[0].Data.ptr == Second._Data.ptr);
		assert(Split[4].Data.ptr == Second._Data.ptr + 8192);
		foreach(Buffer buff; Split[0..4])
			assert(buff.Data.length == 2048);		
		assert(Split[4].Data.length == 1);		
	}
	
	/// Writes a single value of type T into the buffer.
	/// Params:
	/// 	T = The type of the value to write.
	/// 	Value = The value to write.
	void Write(T)(T Value) if(!is(T == class) && !is(T == interface) && !isArray!T) {
		CheckDisposed();
		Reserve(T.sizeof);
		*(cast(T*)&this._Data[_Position]) = Value;
		_Position += T.sizeof;
	}
	
	/// Writes the given array into the buffer, with no prefix nor terminator.
	/// Params:
	/// 	T = The type of the values in the array.
	/// 	Value = The array to write.
	void Write(T)(in T[] Value) if(!is(T == class) && !is(T == struct) && !isArray!T) {
		CheckDisposed();
		if(Value.length == 0)
			return;
		Reserve(T.sizeof * Value.length);
		memcpy(&this._Data[_Position], Value.ptr, cast(uint)Value.length * T.sizeof);
		_Position += T.sizeof * Value.length;
	}
	
	/// Writes the given array in to the buffer, with an unsigned integer indicating length.
	/// Params:
	///		T = The type of the values in the array.
	///		Value = The array to write.
	void WritePrefixed(T)(in T[] Value) if(!is(T == class) && !is(T == struct) && !isArray!T) {
		CheckDisposed();
		size_t TotalSize = T.sizeof * Value.length + uint.sizeof;
		assert(TotalSize < uint.max);
		Reserve(TotalSize);
		void* BasePtr = &this._Data[_Position];
		*(cast(uint*)BasePtr) = cast(uint)Value.length;
		memcpy(BasePtr + uint.sizeof, Value.ptr, cast(uint)Value.length * T.sizeof); 
		_Position += TotalSize;
	}
	
	unittest {
		Buffer buff = new Buffer(4);
		ubyte* OrigPtr = buff._Data.ptr;
		buff.Write(4);
		assert(buff._Position == 4);
		assert(*(cast(int*)&buff._Data[0]) == 4);
		assert(OrigPtr == buff._Data.ptr);
		buff.Write('c');
		assert(OrigPtr != buff._Data.ptr);
		assert(buff._Position == 5);
		assert(buff._Data[4] == 'c');
		
		buff.Write("Test");
		assert(buff._Position == 9);
		assert(buff._Data[5 .. 9] == "Test");
		
		buff.WritePrefixed("Other");
		assert(buff._Position == 18);
		assert(*(cast(uint*)&buff._Data[9]) == 5);
		assert(buff._Data[13..18] == "Other");
	}
	
private:
	bool _Disposed;
	ubyte[] _Data;	
	size_t _Position;
	
	void CheckDisposed() const {
		enforce(!_Disposed, "Operations are not allowed on a disposed buffer.");
	}
}