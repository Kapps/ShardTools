module ShardTools.Disposable;
public import ShardTools.IDisposable;

// TODO: Not sure if this should exist. Consider phasing out.

/// An abstract class providing a basic implementation of the IDisposable interface.
/// $(RED This module should be considered deprecated, as it provides no real benefits over destructors.)
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
