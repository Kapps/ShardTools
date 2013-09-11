module ShardTools.SignaledTask;
public import ShardTools.Event;
import std.typecons;

public import ShardTools.AsyncAction;
import ShardTools.Untyped;

/// Gets a task that is completed only by a call to the Complete method.
class SignaledTask : AsyncAction {

public:
	/// Creates a new SingaledTask that must be called manually.
	this() {
		
	}
	
	/// Notifies the signaled task that it has completed.
	void SignalComplete(Untyped CompletionData) {
		this.NotifyComplete(CompletionType.Successful, CompletionData);
	}

protected:

	/// Implement to handle the actual cancellation of the action.
	/// If an action does not support cancellation, CanAbort should return false, and this method should throw an error.
	override void PerformAbort() {
		// No action is needed.
	}

private:
}