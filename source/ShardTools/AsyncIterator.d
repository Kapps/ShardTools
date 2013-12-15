/// Provides an AsyncAction that iterates over a foreach capable object in an asynchronous manner.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.AsyncIterator;

import ShardTools.AsyncAction;
version(None) {
/// Provides an AsyncAction that iterates over a foreach capable object in an asynchronous manner.
/// When using this iterator, though including a return statement would compile, it is not allowed.
/// As such, using a return will result in an Error being thrown.
/// The iterator may continue iterating even when the local scope ends.
/// The iterator uses the default opApply on the input type, but attempts to avoid using the entire range when not necessary.
/// Because of the asynchronous manner of this iterator, break operations are not precise.
/// $(RED This module is not yet implemented and may be scrapped.)
public class AsyncIterator : AsyncAction {

public:
	/// Initializes a new instance of the AsyncIterator object.
	this() {
		
	}
	
private:
	bool _NeedsAbort = false;
	/// Implement to handle the actual cancellation of the action.
	/// If an action does not support cancellation, CanAbort should return false, and this method should throw an error.
	override void PerformAbort() {
		_NeedsAbort = true;
	}
}
}