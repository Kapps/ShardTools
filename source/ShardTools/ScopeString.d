module ShardTools.ScopeString;
import ShardTools.ExceptionTools;
import core.stdc.string;

@nogc:

/// Provides a null-terminated string allocated on the scope of a maximum of N bytes.
struct ScopeString(size_t N) {
@nogc:
	this(string existing) {
		static ex = cast(immutable)(new NotSupportedException("String too long for ScopeString."));
		if(existing.length >= N - 1)
			throw ex;
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

/// Returns a scope string for the given data with a maximum length of N.
/// The default value of 248 bytes is chosen to be 256 bytes after including the extra length field.
auto scoped(size_t N = 248)(string str) {
	return ScopeString!(N)(str);
}