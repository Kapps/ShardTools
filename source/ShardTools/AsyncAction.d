module ShardTools.AsyncAction;
private import core.atomic;
import ShardTools.Untyped;
private import std.functional;
private import std.range;
private import std.stdio;
private import core.sync.mutex;
private import ShardTools.ConcurrentStack;
private import std.container;
private import std.exception;
private import ShardTools.NativeReference;
private import core.thread;
private import std.datetime;
private import std.typecons;
public import std.variant;
private import ShardTools.ExceptionTools;

/// Indicates whether an AsyncAction is complete and whether it was aborted or finished successfully.
enum CompletionType {
	/// The action is not yet complete.
	Incomplete = 0,
	/// The action has been cancelled or aborted.
	Aborted = 1,
	/// The action completed successfully.
	Successful = 2
}

mixin(MakeException("ActionAbortedException", "Attempted to get the results of an action that was aborted prior to completion."));

/// Provides information about an action that executes asynchronously.
abstract class AsyncAction  {
	
public:
	
	// TODO: Rename AsyncAction to Task?
	// Problem is std.parallelism.
	// Probably do AsyncTask actually.
	// ...or just leave it as is at that point.
	// AsyncTask would actually be a subclass of AsyncAction, likely one that carries out a task in a different fiber/thread.
	
	alias void delegate(Untyped, AsyncAction, CompletionType) CompletionCallback;
	
	/// Initializes a new instance of the AsyncAction object.
	this() {
		_TimeoutDuration = dur!"hnsecs"(0);
		_StartTime = Clock.currTime();
		StateLock = new Mutex();
	}
	
	/// Indicates whether this action will ever time out if not completed within TimeoutTime.
	@property bool CanTimeout() const {
		return _TimeoutDuration.total!("hnsecs") > 0;
	}
	
	/// Indicates whether this action can be canceled, disregarding the current completion state of the action.
	@property bool CanAbort() const {
		return true;
	}
	
	/// Indicates whether this action is complete, whether successful or aborted.
	@property bool IsComplete() const {
		return this.Status != CompletionType.Incomplete;
	}
	
	/// Gets or sets the amount of time that an action can run before timing out.
	/// If CanTimeout is false, this will return zero.
	/// If set to zero, indicates no timeout occurs.
	@property Duration TimeoutTime() const {
		return _TimeoutDuration;
	}
	
	/// Ditto
	@property void TimeoutTime(Duration Value) {
		_TimeoutDuration = Value;
	}
	
	/// Indicates when this action was started.
	@property const(SysTime) StartTime() const {
		return _StartTime;
	}
	
	/// Indicates how long has passed since the creation of this action.
	/// The precision of this method is equivalent to the precision of SysTime opSubtract, which is generally 10-20 millisecond precision.
	/// If higher precision is needed, a Timer should be used instead.
	@property Duration Elapsed() const {
		return Clock.currTime() - _StartTime;
	}
	
	/// Indicates the status of this operation.
	@property CompletionType Status() const {
		return _Status;
	}
	
	/// Gets a value indicating whether this action has started being processed.
	@property bool HasBegun() const {
		return _HasBegun;
	}
	
	/// Begins this operation asynchronously, returning this same instance.
	AsyncAction Start() {
		if(!cas(cast(shared)&_HasBegun, cast(shared)false, cast(shared)true))
			throw new InvalidOperationException("The AsyncAction has already been started.");		
		// Make sure we don't get garbage collected after the action has begun.
		NativeReference.AddReference(cast(void*)this);
		return this;
	}	
	
	/// Gets the result of the completion for this command.
	/// The type of the data is unknown, but is generally what the synchronous version would return, or an instance of Throwable.
	/// Accessing this property before the action is complete will result in an InvalidOperationException being thrown.
	/// It is however allowed to retrieve CompletionData for an Aborted action. The standard convention is to store a Throwable if possible in this situation.
	@property Untyped CompletionData() {		
		if(Status == CompletionType.Incomplete)
			throw new InvalidOperationException("Unable to access completion data prior to the action being complete.");
		return _CompletionData;		
	}
	
	/// Invokes the given callback when this action is complete or aborted.
	/// If the operation is already finished, Callback is invoked immediately and synchronously.
	/// Params:
	/// 	State = A user-defined value to pass in to Callback.
	/// 	Callback = The callback to invoke.
	void NotifyOnComplete(Untyped State, CompletionCallback Callback) {
		bool InvokeImmediately = false;
		synchronized(StateLock) {
			if(!IsComplete)
				CompletionSubscribers ~= cast(typeof(CompletionSubscribers[0]))tuple(State, Callback);
			else
				InvokeImmediately = true;						
		}
		// Callback gets invoked outside lock.
		if(InvokeImmediately)
			Callback(State, this, _Status);
	}
	
	/// Attempts to cancel this operation, either synchronously or asynchronously.
	/// Because this may be executed asynchronously, it is possible that the action will complete prior to being cancelled.
	/// If this is the case, the cancel operation will effectively do nothing.	
	/// It is also possible that this simply notifies the operation that it should no longer continue after the next step.
	/// As such, the action may not immediately abort and it's impossible to know whether the action has actually been aborted.
	/// Optionally, Abort can take in a result to provide additional information about the error or a progress update.
	/// The result is then available through the CompletionData property.
	void Abort(Untyped Result = Untyped.init) {
		synchronized(StateLock) {
			if(!CanAbort)
				throw new NotSupportedException("Attempted to abort an AsyncAction that does not support the Abort operation.");
			if(Status != CompletionType.Incomplete)
				return;
			_CompletionData = Result;
			PerformAbort();
		}
	}
	
	/// Blocks the calling thread until this action completes.
	/// Returns the way in which this action was completed.
	/// Params:
	/// 	Timeout = A duration after which to throw an exception if not yet complete.
	CompletionType WaitForCompletion(Duration Timeout = dur!"msecs"(0)) {
		SysTime Start = Clock.currTime();
		bool TimesOut = Timeout > dur!"msecs"(0);
		while(_Status == CompletionType.Incomplete) {						
			SysTime Current = Clock.currTime();
			if(TimesOut && (Current - Start) > Timeout)
				throw new TimeoutException();
			/+else if((Current - Start) > dur!"msecs"(2)) {
			 // Basically, we don't want to waste a huge amount of CPU time, but at the same time we want faster than 1 MS precision.
			 // We can't do faster than 1MS precision for sleeps however, so what we do is first not sleep at all.
			 // Then, after 2 milliseconds, we can assume the precision isn't a huge deal, so sleep for 1 MS at a time.
			 // Update: This is probably a bad idea. So, we'll try for sub-ms precision at the start; it works on Linux in theory!
			 Thread.sleep(dur!"msecs"(1));
			 }
			 Thread.sleep(dur!"usecs"(100));+/
			Thread.sleep(dur!"msecs"(1));
			//Thread.yield();
			// TODO: We can significantly increase performance by using a Fiber here.
			// Basically, we use WaitForCompletion to wrap a fiber that does the actual work.
			// This fiber then gets added to a list in a different static class.
			// This list then goes through each of the actions and calls the Fiber again when ready.
			// Of course, that might not work at all...
			// And by might, I really mean won't.
			// Most importantly because the point would be to allow the thread to do some other work, but that's not possible because the thread is blocking anyways.
			// If we had a FiberPool, it could yield and do something from that pool instead, but we don't.
		}
		return _Status;
	}

	/// Synchronously waits for this action to complete, then returns the result casted as T.
	/// If the completion type was not successful, an exception is thrown.
	/// If the completion data was an instance of Throwable, that same instance is rethrown.
	T GetResultSynchronous(T)(Duration Timeout = dur!"msecs"(0)) {
		auto CompletionType = WaitForCompletion(Timeout);
		if(CompletionType != CompletionType.Successful) {
			Throwable t;
			if(CompletionData.tryGet(t)) {
				throw t;
			}
			throw new ActionAbortedException();
		}
		static if(!is(T == void))
			return CompletionData.get!T;
	}
	
protected:
	
	/// Implement to handle the actual cancellation of the action.
	/// If an action does not support cancellation, CanAbort should return false, and this method should throw an error.
	abstract void PerformAbort();
	
	/// Called when this action is completed.
	void OnComplete(CompletionType Status) {
		// No need to lock here; our status is set to complete, so adding a subscriber will be invoked immediately, not added to the list.		
		foreach(ref Subscriber; CompletionSubscribers) {
			Subscriber.Callback(Subscriber.State, this, Status);
		}			
		CompletionSubscribers = null; // Prevent any references causing memory leaks.		
	}
	
	/// Notifies this AsyncAction that the operation was completed.
	void NotifyComplete(CompletionType Status, Untyped CompletionData) {
		enforce(Status == CompletionType.Successful || Status == CompletionType.Aborted);
		if(!_HasBegun)
			throw new InvalidOperationException("Unable to notify an AsyncAction completion prior to invoking Start on it.");
		// Lock here for adding subscribers.
		synchronized(StateLock) {
			if(IsComplete)
				throw new InvalidOperationException("The action was already in a completed state.");
			this._Status = Status;
			this._CompletionData = CompletionData;
		}		
		NativeReference.RemoveReference(cast(void*)this);
		OnComplete(Status);		
	}
	
private:
	CompletionType _Status;
	Duration _TimeoutDuration;
	Tuple!(Untyped, "State", CompletionCallback, "Callback")[] CompletionSubscribers;	
	SysTime _StartTime;
	Untyped _CompletionData;
	bool _HasBegun;
	Mutex StateLock;
}

private class ActionManager {
	// TODO: Replace this with a PollAction? Maybe.
	// At the very least can be in TaskRepeater.
	// Or just leave it since it's not a big deal and fairly specialized. Not sure yet.
	
	shared static this() {
		Actions = new RedBlackTree!AsyncAction();
		ToAdd = new ConcurrentStack!AsyncAction();
		ToRemove = new ConcurrentStack!AsyncAction();
	}
	
	static void RegisterAction(AsyncAction Action) {
		ToAdd.Push(Action);
		Action.NotifyOnComplete(Untyped.init, toDelegate(&ActionComplete));
	}
	
	static void ActionComplete(Untyped State, AsyncAction Action, CompletionType Status) {
		ToRemove.Push(Action);
	}
	
	static void RunLoop() {
		while(true) {
			// Note that ToAdd must be first, in case it gets removed prior to a call.
			foreach(AsyncAction Action; ToAdd) {
				Actions.insert(Action);
			}
			foreach(AsyncAction Action; ToRemove)
				enforce(Actions.removeKey(Action) == 1);
			foreach(AsyncAction Action; Actions) {
				if(Action.CanTimeout && (Action.StartTime + Action.TimeoutTime) < Clock.currTime()) {
					// TODO: Raise a condition here when implemented... else this will just be hidden.
					// No way to really communicate to the action or to a main thread...
					// Most important is to abort the action.
					// TODO: Consider using some form of exception catching here, or throw an error.
					// Again, issue is any thrown exceptions just get hidden; an error may be needed.
					debug writeln("Action timeout.");					
					Action.Abort();										
				}
			}
			Thread.sleep(dur!"seconds"(1));
		}
	}
	
	// We want to make sure to not force a synchronize and thus delay any action from running yet blocking.
	// So, we wait until after Actions is accessed, and then carry out the calls that were made.
	private static __gshared ConcurrentStack!AsyncAction ToAdd;
	private static __gshared ConcurrentStack!AsyncAction ToRemove;
	
	private static __gshared RedBlackTree!AsyncAction Actions;
	
}