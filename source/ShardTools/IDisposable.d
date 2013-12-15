/// Provides a basic interface for a type that is considered to be destroyable.
/// $(RED This module should be considered deprecated, as it provides no real benefits over a destructor.)
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.IDisposable;
import ShardTools.ExceptionTools;

// TODO: Not sure if this should exist. Consider phasing out.

mixin(MakeException("ObjectDisposedException", "The object attempted to be accessed was already disposed."));

/// An interface used to handle disposing of an asset, including tracking of whether it is disposed.
/// $(RED This module should be considered deprecated, as it provides no real benefits over a destructor.)
interface IDisposable {
	
	/// Disposes of this object, provided it has not already been disposed.
	public final void Dispose() {
		if(!IsDisposed)
			return;
		OnDispose();		
	}
	
	/// Gets a value whether this object is currently disposed.
	@property nothrow bool IsDisposed() const;			
	
	/// Occurs when this object has Dispose called when it was not already disposed.
	/// It is possible for this to get called in a destructor, and thus it should not reference any subobjects.
	protected void OnDispose();	
}