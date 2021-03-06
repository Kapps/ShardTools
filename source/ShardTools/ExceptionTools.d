/// Provides basic mixins for helping create new exceptions.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.ExceptionTools;
import ShardTools.Udas;
import std.traits;

mixin(MakeException("InvalidOperationException", "The performed operation was considered invalid for the present state."));
mixin(MakeException("NotSupportedException", "The operation being performed was not supported."));
mixin(MakeException("TimeoutException", "The operation reached the maximum timeout time before being complete."));
mixin(MakeException("DuplicateKeyException", "An object with this key already exists."));
mixin(MakeException("InvalidFormatException", "The data passed in was in an invalid format."));
mixin(MakeException("KeyNotFoundException", "The specified key was not found in this collection."));
mixin(MakeException("InvalidArgumentException", "An argument passed contained an invalid value."));

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

/// Provides an implementation of enforce that reuses a single exception instance
/// and as a result does not allocate nor leak memory.
/// Note that because 'lazy' can not be @nogc, query is evaluated immediately rather than lazily.
@nogc void enforceNoGC(ExType : Exception, string msg)(bool query) {
	if(!query) {
		static immutable ex = cast(immutable)(new ExType(msg));
		throw ex;
	}
}

/// Example
@name("Enforce Tests")
unittest {
	void foo(bool doBar) {
		enforceNoGC!(NotSupportedException, "Bar is not yet supported.")(!doBar);
	}
	foo(false); // okay
	try {
		foo(true);
		assert(0);
	} catch(NotSupportedException e) {
		assert(e.msg == "Bar is not yet supported.");
	}
}

/// Short-hand to enforce that a given instance is not null using the other overload of $(D enforceNoGC).
/// This is often useful with UFCS, particularly for assignment as the same instance is returned.
@nogc inout(T) enforceNoGC(ExType : Exception = InvalidArgumentException, string msg = "Argument can not be null.", T)(inout(T) inst) if(is(T == class) || isPointer!T || isArray!T) {
	enforceNoGC!(ExType, msg)(inst !is null);
	return inst;
}

/// Example
@name("Enforce Instance Tests")
unittest {
	class Bar { }
	void foo(Bar inst) {
		inst.enforceNoGC();
	}
	try {
		foo(null);
		assert(0);
	} catch(InvalidArgumentException e) {
		assert(e.msg == "Argument can not be null.");
	}
}