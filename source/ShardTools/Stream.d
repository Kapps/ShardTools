/// Provides a one-way buffered Stream for IO operations.
/// $(RED This module has been largely replaced by DataSources and will not be completed.)
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Stream;

version(None) {
private import std.traits;
private import std.conv;
public import ShardTools.DataTransformer;
private import core.memory;
private import ShardTools.DataTransformerCollection;
private import std.exception;
private import std.algorithm;
import std.c.string;
import ShardTools.ExceptionTools;

// TODO: Either implement or delete this. It's not currently in use anywhere, and was always disabled.
// Probably delete; InputSource and OutputSource replace streams.

mixin(MakeException("StreamException", "An error has occurred in the stream."));

/// Determines how to perform a seek.
enum SeekMode {
	/// Offset from the start of the Stream, or absolute position.
	Start = 0,
	/// Offset from the current position.
	Current = 1,
	/// Offset from the end of the Stream.
	End = 2
}

/// Determines the type of a stream.
enum StreamMode {
	Read = 1,
	Write = 2,
	ReadWrite = Read | Write
}

/// Provides access to a Stream to read and/or write from.
/// A stream is responsible only for managing the actual reading and writing of the buffered data.
/// For modifying the data as it's written, a Stream may have zero or more DataTransformers.
/// These transformers may be chained together in a priority queue, allowing for more complex transforms.
/// Details:
///		When a Write is requested, it gets added to an internal buffer.
/// 	Then, when a Read or Seek is requested, PerformWrite is called causing the transformed buffer to be written (but not flushed).
/// 	Most implementations will likely flush the buffer anyways however.
///		When a Read is requested, BufferSize bytes are requested to be read from the derived Stream.
///		If the Stream is only capable of Reading, and not Writing, the entire buffer is transformed at this point.
/// 	Otherwise, it's not safe to transform the buffer because we can't put back bytes nor untransform, and it is necessary to if a Write is requested, as it may overwrite.
/// 	If this is the case, we transform per-read. Which is inefficient, but much easier than the alternative implementation (which may be put in later, but probably not).
/// 	Another issue is the user may pass in a buffer not large enough to contain the transformed data. Then we get to buffering in another buffer and complications.
/// 	For now, solve that by just completely not supporting reads and writes combined.
/// Bugs:
///		ReadWrite Mode is not yet supported. Only Read or Write.
@disable abstract class Stream  {

public:
	/// Initializes a new instance of the Stream object.
	/// Params:
	/// 	Mode = Whether this stream supports reads, writes, or both. Note that supporting both reads and writes results in less efficient reads at this time.
	this(StreamMode Mode) {
		if(Mode == StreamMode.ReadWrite)
			throw new StreamException("ReadWrite modes not yet supported.");
		_ReadBuffer = new StreamBuffer(0);
		_WriteBuffer = new StreamBuffer(Mode == StreamMode.Write ? BufferSize : 0);
		_Mode = Mode;
	}

	/// Gets a value indicating whether this Stream supports reads.
	/// Inheriting:
	/// 	When inheriting, this value must remain constant.
	@property final bool CanRead() const {
		return (Mode & StreamMode.Read) != 0;
	}

	/// Gets a value indicating whether this Stream supports writes.
	@property final bool CanWrite() const {
		return (Mode & StreamMode.Write) != 0;
	}

	/// Gets the types of operations this Stream supports.
	/// Inheriting:
	/// 	When inheriting, this value must remain constant.
	@property StreamMode Mode() const {
		return _Mode;
	}

	/// Gets a value indicating whether this Stream supports seeks.
	@property abstract bool CanSeek();

	/// Returns the length, in bytes, of this Stream.
	@property abstract size_t Length();

	/// Gets the position of this Stream.
	@property abstract size_t Position();	

	/// Returns the number of bytes left in the Stream.
	@property size_t Available() {
		return Length - Position;
	}	

	/// Releases any resources used by this Stream.
	void Close() { 
		Flush();
	}	

	/// Writes the given data to the Stream, or throws if CanWrite is false.
	/// Params:
	/// 	Data = The data to write.
	/// 	Length = The number of bytes to write.
	void Write(void* Data, size_t Length) {
		WriteBuffer.Write(cast(ubyte*)Data, Length);
	}

	/+ /// Writes the given value to the stream.
	/// Params:
	/// 	T = The type of the value. Must be a primitive or struct.
	/// 	Value = The value to write.
	void Write(T)(T Value) if(!is(T == class) && !is(T == interface) && !isPointer!(T)) {
		Write(&Value, T.sizeof);
	}

	/// Ditto
	void Write(T)(T[] Value) if(!is(T == class) && !is(T == interface) && !isPointer(T)) {
		Write(Value.ptr, T.sizeof * Value.length);
	}+/

	/// Writes the given value to the stream, prefixed by an integer (NOT size_t) length.	
	/// Params:
	/// 	T = The type of the value. Must be a primitive or struct.
	/// 	Value = The value to write.
	void WritePrefixed(T)(T[] Value) if(!is(T == class) && !is(T == interface)) {
		Write(cast(int)Value.length);
		Write(Value);
	}

	/// Writes the given array to the stream, followed by a single T.init.
	/// Params:
	/// 	T = The type of the value. Must be a primitive or struct.
	/// 	Value = The value to write.
	void WriteNullTerminated(T)(T[] Value) if(!is(T == class) && !is(T == interface)) {
		Write(Value);
		Write(T.init);
	}

	/// Flushes this Stream, applying all buffered writes.
	/// Inheriting:
	/// 	The base implementation should be called first to write the buffer.
	void Flush() {
		if(WriteBuffer.Position == 0)
			return;
		//ubyte[] Buffer = WriteBuffer._Data[0..WriteBuffer._Position];
		ubyte[] Transformed = cast(ubyte[])TransformWrite(WriteBuffer._Data, WriteBuffer._Position);
		WriteBuffer._Position = 0;		
		PerformWrite(Transformed);				
	}

	/// Flushes this Stream, applying buffered writes without transforming them.
	/// This can be useful in case you want to create a header or handshake without it being modified.
	/// Inheriting:
	/// 	The base implementation should be called first to write the buffer.
	void FlushWithoutTransform() {
		if(WriteBuffer._Position == 0)
			return;
		PerformWrite(WriteBuffer._Data[0..WriteBuffer._Position]);
	}

	/// Reads the given number of bytes from the Stream.
	/// Params:
	/// 	Length = The number of bytes to read.
	ubyte[] Read(size_t Length) {
		ubyte[] Buffer = new ubyte[min(Length, Available)];
		ReadInto(Buffer.ptr, Buffer.length);
		return Buffer;
	}		

	/// Reads Length bytes into Buffer, replacing up to Length bytes within buffer.
	/// If less than Length bytes are available, writes until the end of the Stream.
	/// Params:
	/// 	Buffer = The buffer to read into.
	/// 	Length = The number of bytes to write to buffer.
	/// Returns:
	/// 	The number of bytes actually written.
	size_t ReadInto(void* Buffer, size_t Length) {		
		/+size_t BytesRead = ReadBuffer.Read(Buffer, Length);
		size_t Remaining = Length - BytesRead;
		size_t TotalRead = BytesRead;
		if(Remaining > 0) {
			// We're out of bytes. Clear the ReadBuffer.
			ReadBuffer._Position = 0;
			ReadBuffer._Size = 0;
			// Read the rest of the bytes the caller requested:
			size_t NewRead = PerformRead(Buffer + BytesRead, Remaining);
			ubyte[] Transformed = TransformRead(Buffer + BytesRead, NewRead);
			size_t AdditionalOffset = 0;
			if(Transformed.ptr != Buffer + BytesRead) {
				// The transform resulted in a new buffer being created. Copy as much as the client requested, and put the rest in our buffer.
				
			}
			TotalRead += NewRead;
			if(NewRead == Remaining) // We may have some bytes left to allocate to the buffer.
				ReadBuffer._Size = PerformRead(ReadBuffer._Data, BufferSize);
		}
		return TotalRead;+/
		size_t RemainingBytes = Length;
		size_t BytesWritten = 0;
		// TODO: Make this not done so... badly. Why are we buffering multiple times instead of just requesting one larger read and then buffering.
		// Still, makes it a lot simpler than keeping track of size adjustments with the transforms.
		do {			
			// From the already transformed buffer, read the amount of bytes they requested.
			size_t ReadBytes = ReadBuffer.Read(cast(ubyte*)Buffer + BytesWritten, Length);
			RemainingBytes -= ReadBytes;
			BytesWritten += ReadBytes;
			if(RemainingBytes > 0) {
				// More bytes to be read, so we fill and transform our buffer again.
				ReadBuffer._Position = 0;
				size_t ReadToBuffer = PerformRead(ReadBuffer._Data, BufferSize);
				// Now transform it.
				ubyte[] Transformed = cast(ubyte[])(ReadToBuffer == 0 ? null : TransformRead(ReadBuffer._Data, ReadToBuffer));
				if(Transformed.ptr != ReadBuffer._Data) {
					// The transform created a new buffer.
					GC.free(ReadBuffer._Data);
					ReadBuffer._Data = Transformed.dup.ptr;					
				}
				ReadBuffer._Size = Transformed.length;				
				if(ReadToBuffer == 0)
					break;
			}
		} while(RemainingBytes > 0);
		return BytesWritten;
	}

	/// Ditto
	size_t ReadInto(void[] Buffer, size_t Length) {		
		return ReadInto(Buffer.ptr, min(Buffer.length, Length));			
	}

	/+ /// Reads the given type from the Stream.
	/// Params:
	/// 	T = The type to read. Must be a primitive, struct, or array of one of those.
	@property T Read(T)() {
		T Result;
		if(ReadInto(&Result, T.sizeof) != T.sizeof)
			throw new StreamException("Insufficient bytes to read a value of type " ~ T.stringof ~ ".");
		return Result;
	}

	/// Reads the given number of T instances from the Stream.
	/// Params:
	/// 	T = The type to read. Must be a primitive, struct, or array of one of those.
	/// 	Count = The number of elements within the array.
	@property T[] Read(T)(size_t Count) {
		T[] Result = new T[Count];
		if(ReadInto(Result.ptr, T.sizeof * Result.length) != T.sizeof * Result.length)
			throw new StreamException("Insufficient bytes to read " ~ to!string(Count) ~ " " ~ T.stringof ~ "s.");
		return Result;
	}+/

	private void[] TransformRead(void* Buffer, size_t Count) {
		ubyte[] Data = cast(ubyte[])Buffer[0..Count];
		foreach(DataTransformer Transformer; Transformers) {
			Data = Transformer.Transform(Data, TransformMode.Read);
		}
		return Data;
	}

	private void[] TransformWrite(void* Buffer, size_t Count) {
		ubyte[] Data = cast(ubyte[])Buffer[0..Count];
		foreach_reverse(DataTransformer Transformer; Transformers)
			Data = Transformer.Transform(Data, TransformMode.Write);
		return Data;
	}

	/// Gets a collection of transformers used to operate on the data in the Stream.
	/// Each Transformer gets called upon a Flush.
	@property DataTransformerCollection Transformers() {
		return _Transformers;
	}

	~this() {
		Close();
	}

protected:	

	/// Returns the minimum size of the buffer used to buffer reads and writes.
	/// This value must be a constant from immediately before the base Stream constructor is called, otherwise the buffers will point to invalid memory.
	/// It is allowed to calculate it in the derived Stream constructor, so long as when the base constructor is called, it remains constant.
	/// This size will be respected for reads, but writes may result in a resized buffer.
	@property size_t BufferSize() {
		return _BufferSize;
	}

	/// Gets an object to buffer reads or writes to.
	@property StreamBuffer ReadBuffer() { return _ReadBuffer; }
	/// Ditto
	@property StreamBuffer WriteBuffer() { return _WriteBuffer; }

	/// Performs the actual writing of data once the buffer is flushed and the result is transformed.
	/// Params:
	/// 	Buffer = The buffer to write to the underlying device.
	abstract void PerformWrite(ubyte[] Data);

	/// Performs the actual read for this Stream, in order to generate a read buffer.
	/// Usually, more data is requested to be read by this method than the caller has requested at this moment.
	/// Params:
	/// 	RequestedSize = The maximum number of bytes that should be read.
	/// 	Buffer = The buffer to write the result to.
	/// Returns:
	/// 	The actual number of bytes written to the buffer.
	/// 	This is allowed to be less than RequestedSize, but may never be more.
	abstract size_t PerformRead(void* Buffer, size_t RequestedSize);	
	
	/// Provides a buffer used to buffer reads or writes within a stream.	
	protected final class StreamBuffer {
		this(size_t Size) {
			this._Size = Size;
			this._Position = 0;
			this._Data = cast(ubyte*)GC.malloc(BufferSize);
			//_Capacity = BufferSize;
		}

		/// For reads, the number of bytes buffered. For writes, the number of bytes the buffer can store before it gets automatically resized, which is the same as BufferSize.
		@property size_t Size() const { return _Size; }

		/// For reads, the number of bytes that have been read from the buffer. For writes, the number of bytes that have been written to the buffer.
		@property size_t Position() const { return _Position; }

		/// The number of available bytes (Size - Position).
		@property size_t Available() const { return Size - Position; }

		/// Writes up to Count bytes from the buffer into Location. Returns the actual number of bytes if less bytes are available in the buffer than requested.
		public size_t Read(ubyte* Location, size_t Count) {
			if(!CanRead)
				throw new StreamException("Reads are not supported by this stream.");
			if(CanWrite) {
				size_t NumRead = PerformRead(Location, Count);
			}
			size_t NumCopied = min(Count, Available);
			memcpy(Location, _Data + Position, NumCopied);
			_Position += NumCopied;
			return NumCopied;
		}

		/// Writes Count bytes from Location into the buffer. 
		public void Write(ubyte* Location, size_t Count) {
			EnsureSize(Count + _Position);
			memcpy(_Data + _Position, Location, Count);
			_Position += Count;
		}

		/// Ensures the buffer can store the given number of bytes. This is done automatically for writes. Used internally to allow buffering reads.
		void EnsureSize(size_t TotalSize) {
			if(TotalSize >= _Size + _Position)
				return;
			size_t NewSize = _Size;
			while(NewSize < TotalSize)
				NewSize <<= 1;			
			ubyte* NewData = cast(ubyte*)GC.malloc(TotalSize);			
			_Size = NewSize;
			memcpy(NewData, _Data, _Position);
			GC.free(_Data);
			this._Data = NewData;
		}		

		~this() {
			GC.free(_Data);
		}

		package size_t _Size;
		package size_t _Position;		
		package ubyte* _Data;
	}
	
private:
	size_t _BufferSize;
	StreamBuffer _ReadBuffer;
	StreamBuffer _WriteBuffer;
	DataTransformerCollection _Transformers;
	StreamMode _Mode;
}

/// Helper class for streams that are only for output.
abstract class OutputStream : Stream {
	
	/// Performs the actual read for this Stream, in order to generate a read buffer.
	/// Usually, more data is requested to be read by this method than the caller has requested at this moment.
	/// Params:
	///		RequestedSize = The maximum number of bytes that should be read.
	///		Buffer = The buffer to write the result to.
	override size_t PerformRead(void* Buffer, size_t RequestedSize) {
		throw new StreamException("This stream does not support reads.");
	}

	this() { 
		super(StreamMode.Write);
	}
}

/// Helper class for streams that are only for input.
abstract class InputStream : Stream {

	/// Performs the actual writing of data once the buffer is flushed and the result is transformed.
	/// Params:
	/// 	Buffer = The buffer to write to the underlying device.
	override void PerformWrite(ubyte[] Data) {
		throw new StreamException("This stream does not support writes.");
	}
	
	this() {
		super(StreamMode.Read);
	}
}

}