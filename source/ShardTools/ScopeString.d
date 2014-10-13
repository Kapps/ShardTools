module ShardTools.ScopeString;
import ShardTools.ExceptionTools;
import core.stdc.string;

@nogc:

/// Provides a null-terminated string allocated on the scope of a maximum of N bytes.
struct ScopeString(size_t N) {
	this(string existing) {
		if(existing.length >= N - 1)
			throw new NotSupportedException("String too long for ScopeString.");
		memcpy(_buffer.ptr, existing.ptr, existing.length);
		_buffer[existing.length] = '\0';
		_length = existing.length;
	}

	/// Returns a slice of the null-terminated buffer.
	/// Note that this slice points to a stack array.
	@property char[] buffer() {
		return _buffer;
	}

	/// The number of used bytes in the buffer, excluding null terminator.
	@property size_t length() {
		return _length;
	}

	/// Returns a pointer to the null-terminated string contained in the buffer.
	@property char* ptr() {
		return _buffer.ptr;
	}

	private size_t _length;
	private char[N] _buffer;
}