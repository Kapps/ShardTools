/// An abstract class providing a basic implementation of the IDisposable interface.
/// $(RED This module should be considered deprecated, as it provides no real benefits over destructors.)
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Disposable;
public import ShardTools.IDisposable;

// TODO: Not sure if this should exist. Consider phasing out.

/// An abstract class providing a basic implementation of the IDisposable interface.
abstract class Disposable : IDisposable {
	
	/// Gets a value whether this object is currently disposed.
	nothrow bool IsDisposed() const {
		return _IsDisposed;	
	}
	
	/// Occurs when this object has Dispose called when it was not already disposed.
	protected void OnDispose() {
		_IsDisposed = true;	
	}
	
	~this() {
		IDisposable.Dispose();	
	}
	
	private bool _IsDisposed = false;
}
