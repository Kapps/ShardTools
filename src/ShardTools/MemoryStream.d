module ShardTools.MemoryStream;

import ShardTools.Stream;

/+ /// Provides read-write access to an in-memory stream of data.
class MemoryStream : Stream {

public:
	/// Initializes a new instance of the MemoryStream object.
	this() {
		
	}
	
	/// Gets a value indicating whether this Stream supports reads.
	@property override bool CanRead() {
		return true;
	}

	/// Gets a value indicating whether this Stream supports writes.
	@property override bool CanWrite() {
		return true;
	}

	/ + /// Gets a value indicating whether this Stream supports seeks.
	@property override bool CanSeek() {
		return true;
	}+ /

	/// Flushes this Stream, applying all unbuffered writes.
	override void Flush() {
		// Do nothing.
	}

	/// Returns the length, in bytes, of this Stream.
	@property override size_t Length() {
		
	}

	/// Reads Length bytes into Buffer, replacing up to Length bytes within buffer.
	/// If less than Length bytes are available, writes until the end of the Stream.
	/// Params:
	/// 	Buffer = The buffer to read into.
	/// 	Length = The number of bytes to write to buffer.
	/// Returns:
	/// 	The number of bytes actually written.
	override size_t ReadInto(void* Buffer, size_t Length) {

	}

	/// Gets the position of this Stream.
	@property override size_t Position() {

	}

private:
	
}+/