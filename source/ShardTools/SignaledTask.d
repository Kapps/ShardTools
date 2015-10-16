/// Provides an AsyncAction that gets signaled when it should complete.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.SignaledTask;

version(none) {
public import ShardTools.Event;
import std.typecons;

public import ShardTools.AsyncAction;
import ShardTools.Untyped;
import ShardTools.ExceptionTools;

/// Gets a task that is completed only by a call to the Complete method.
class SignaledTask : AsyncAction {

public:
	/// Creates a new SingaledTask that must be called manually.
	this() {
		
	}
	
	/// Notifies the signaled task that it has completed.
	void SignalComplete(Untyped CompletionData) {
		if(!HasBegun)
			throw new InvalidOperationException("Unable to signal a task being complete before Start was called.");
		this.NotifyComplete(CompletionType.Successful, CompletionData);
	}

protected:

	/// Implement to handle the actual cancellation of the action.
	/// If an action does not support cancellation, CanAbort should return false, and this method should throw an error.
	override void PerformAbort() {
		// No action is needed.
	}
}
}