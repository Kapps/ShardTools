/// Provides a task that can be run asynchronously without the full overhead of an $(D AsyncAction).
/// $(RED This module is not yet implemented, and may be completely redone.)
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.AsyncTask;
public import ShardTools.AsyncAction;

// TODO: Scrap this and start over.
// Use Untyped, make it an AsyncAction in some way, etc.
// Maybe just use a wrapper around it to make it an AsyncAction, since should still be able to use it as regular light-weight otherwise.

/// Provides a task that can be run asynchronously without the full overhead of an $(D AsyncAction).
/// $(RED This module is not yet implemented, and may be completely redone.)
@disable struct AsyncTask(ReturnType, ArgType...) if(is(ReturnType == void) || is(ReturnType == Variant)) {

public:
	/// Initializes a new instance of the AsyncTask object to call the given function pointer or delegate.
	/// The called method must return either a Variant, or void. Returning void will remove the overhead of a Variant (which at the moment is a fair bit of overhead).
	this(ReturnType function(ArgType) ExecuteFunction) {
		this.ExecuteFunction = ExecuteFunction;
		this.IsDelegate = false;		
	}
		
	/// Ditto
	this(ReturnType delegate(ArgType) ExecuteDelegate) {
		this.ExecuteDelegate = ExecuteDelegate;
		this.IsDelegate = true;
	}

	/// Begins this operation synchronously. Generally, this should only be called by the TaskManager.	
	package void StartSynchronous() {		
		ReturnType Result;
		if(IsDelegate)
			Result = ExecuteDelegate(Args);
		else
			Result = ExecuteFunction(Args);	
	}
	
private:
	union {		
		ReturnType function(ArgType) ExecuteFunction;
		ReturnType delegate(ArgType) ExecuteDelegate;		
	}
	bool IsDelegate;		
	enum bool HasReturnType = !is(ReturnType == void);
	//static if(HasReturnType) {
		ArgType Args;
	//}
}

/// Creates a new AsyncTask with the given return type and argument type.
@disable AsyncTask MakeTask(ReturnType, ArgType...)(ReturnType function(ArgType) ExecuteFunction, ArgType Args) {
	return AsyncTask!(ReturnType, ArgType)(ExecuteFunction, Args);
}

/// Creates a new AsyncTask with the given return type and argument type.
@disable AsyncTask MakeTask(ReturnType, ArgType...)(ReturnType delegate(ArgType) ExecuteDelegate, ArgType Args) {
	return AsyncTask!(ReturnType, ArgType)(ExecuteDelegate, Args);
}