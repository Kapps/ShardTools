/// Allows access to a weak reference of memory that can be garbage collected.
/// $(RED This module is incomplete.)
module ShardTools.WeakReference;
private import std.conv;
private import std.exception;
import ShardTools.ExceptionTools;

mixin(MakeException("ObjectDestroyedException", "Attempted to access the value of an already collected weak handle."));

private alias void delegate(Object) DEvent;
private extern (C) void rt_attachDisposeEvent(Object h, DEvent e);
private extern (C) void rt_detachDisposeEvent(Object h, DEvent e);

// TODO: Write unit tests.
// Can just use WeakReferences and trigger GC's.
// Make sure to test struct pointers.
// Also, test 64-bit for sure.

/// Provides a weak reference to a single object.
/// BUGS:
///		This class is completely untested. I honestly have no clue if it even works.
@disable class WeakReference(T) {

public:
	// Xor value to point outside GC memory.
	// This is super sketchy.
	private enum int XorValue = 10101010;
	/// Creates a new weak reference to an object.
	/// Params:
	/// 	Value = The value to reference.
	/// 	DestroyedCallback = A callback to invoke when the value is destroyed.
	///							The destructor is called prior to this method being invoked, but the object is still considered alive until after this method is called.
	///							IMPORTANT: There is no guarantee this will be called on program termination! 
	this(T Value, void delegate(WeakReference!(T)) DestroyedCallback = null) {		
		this._Pointer = cast(size_t)cast(void*)Value ^ XorValue;
		this._IsAlive = true;
		this._IsHooked = false;
		this.DestroyedCallback = DestroyedCallback;
		CreateHook();		
	}

	/// Returns the value of the referenced object, or throws if the object has been deleted.	
	@property T Value() {		
		if(!_IsAlive)
			throw new ObjectDestroyedException("Deleted.");
		return cast(T)Pointer;
	}
	
	/// Indicates whether the referenced object is currently alive.
	@property bool IsAlive() const {
		return _IsAlive;		
	}

	/// Gets the pointer to the referenced object.
	/// This pointer points to invalid memory if the object is destroyed, but is otherwise valid.
	@property void* Pointer() {
		return cast(void*)(_Pointer ^ XorValue);
	}

	~this() {
		DeleteHook();
	}

private:	
	bool _IsAlive;
	bool _IsHooked;
	void delegate(WeakReference!(T)) DestroyedCallback;
	size_t _Pointer;		

	void OnDelete(Object obj) {					
		if(DestroyedCallback)
			DestroyedCallback(this);
		_IsAlive = false;
		DeleteHook();		
	}

	void CreateHook() {		
		enforce(!_IsHooked);
		_IsHooked = true;
		rt_attachDisposeEvent(Value, &OnDelete);		
	}

	void DeleteHook() {		
		enforce(_IsHooked);
		_IsHooked = false;
		rt_detachDisposeEvent(Value, &OnDelete);		
	}
}