/// Contains an AsyncAction to use for actions that should complete immediately.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.ImmediateAction;

version(none) {
import ShardTools.AsyncAction;
import ShardTools.Untyped;

/// Implements AsyncAction for those actions that are executed in a way that supports
/// asynchronous actions, and thus AsyncAction, yet themselves complete immediately.
/// This class is instantiated with either $(D ImmediateAction.Success) or $(D ImmediateAction.Fail),
/// corresponding to the Successful and Aborted states respectively.
class ImmediateAction : AsyncAction {

	private this(CompletionType status, Untyped completionData) {
		super();
		this.Start();
		this.NotifyComplete(status, completionData);
	}

	/// Overridden to prevent aborts.
	override bool CanAbort() const pure nothrow {
		return false;
	}

	override void PerformAbort() {
		assert(0);
	}

	/// Returns an AsyncAction that immediately succeeds with the given completion data.
	static ImmediateAction success(Untyped completionData = Untyped.init) {
		return new ImmediateAction(CompletionType.Successful, completionData);
	}

	/// Returns an AsyncAction that immediately fails with the given completion data.
	static ImmediateAction failure(Untyped completionData = Untyped.init) {
		return new ImmediateAction(CompletionType.Aborted, completionData);
	}

	/// ditto
	static ImmediateAction failure(Exception ex) {
		return failure(Untyped(ex));
	}
}

}