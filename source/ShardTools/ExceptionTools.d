/// Provides basic mixins for helping create new exceptions.
module ShardTools.ExceptionTools;

mixin(MakeException("InvalidOperationException", "The performed operation was considered invalid for the present state."));
mixin(MakeException("NotSupportedException", "The operation being performed was not supported."));
mixin(MakeException("TimeoutException", "The operation reached the maximum timeout time before being complete."));
mixin(MakeException("DuplicateKeyException", "An object with this key already exists."));
mixin(MakeException("InvalidFormatException", "The data passed in was in an invalid format."));
mixin(MakeException("KeyNotFoundException", "The specified key was not found in this collection."));

/// Returns a string to make an exception with the given name and default details, optionally including the base class.
string MakeException(string ExceptionName, string ExceptionDetails, string Base = "Exception") {
	//const char[] MakeException = 
	return
		"public class " ~ ExceptionName ~ " : " ~ Base ~ " {
			public:				
				this(string ExceptionDetails = \"" ~ ExceptionDetails ~ "\", string File = __FILE__, size_t Line = __LINE__) {
					super(ExceptionDetails, File, Line);
				}
		}";
}

/// Creates an exception with the given name that does not have a default message.
string MakeException(string ExceptionName) {
	//const char[] MakeException = 
	return
		"public class " ~ ExceptionName ~ " : Exception {
			public:				
				this(string ExceptionDetails, string File = __FILE__, size_t Line = __LINE__) {
					super(ExceptionDetails, File, Line);
				}
		}";
}