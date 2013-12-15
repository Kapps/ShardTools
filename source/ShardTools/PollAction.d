/// Provides an AsyncAction that polls a delegate to determine when to complete.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.PollAction;
import ShardTools.AsyncAction;
import ShardTools.TaskRepeater;
import ShardTools.Untyped;

/// Provides an AsyncAction that gets it's result by utilizing a polling function.
/// The poll function is expected to complete the action when possible and set the completion data.
/// The action will stop being polled once it reports that it's complete.
/// It is allowed for a PollAction to be completed outside of Poll.
/// When completed outside Poll, Poll will not be called again.
class PollAction : AsyncAction {

	/// Creates a new PollAction that uses the given function to poll.
	/// The function is expected to complete the action when possible.
	/// Once Start is called, a TaskRepeater will be used to automatically handle polling.
	/// If PollFunction throws an Exception, this action is automatically aborted.
	this(void delegate() PollFunction) {
		this._PollFunction = PollFunction;
	}

	/// Requests that this PollAction perform a poll operation.
	/// If the operation results in the desired effect, the action should be completed.
	void Poll() {
		try {
			_PollFunction();
		} catch (Throwable t) {
			Abort(Untyped(t));
		}
	}

	/// Begins polling this action.
	override PollAction Start() {
		return cast(PollAction)super.Start();
	}
	
private:
	void delegate() _PollFunction;

	RepeaterFlags PerformPoll() {
		if(Status != CompletionType.Incomplete)
			return RepeaterFlags.None;
		Poll();
		return Status == CompletionType.Incomplete ? RepeaterFlags.Continue : RepeaterFlags.None;
	}
}

