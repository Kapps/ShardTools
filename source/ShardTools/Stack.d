/// Provides a LIFO stack that is not thread-safe. The $(D ConcurrentStack) may be used for a thread-safe version.
module ShardTools.Stack;
private import core.memory;
private import core.atomic;
private import std.exception;
private import std.c.stdlib;
private import std.c.string;

import ShardTools.ICollection;


/// Represents a last-in first-out (LIFO) stack capable of resizing itself to store however many elements are needed.
/// Params:
/// 	T = The type of elements to store in this Stack.
final class Stack(T) {

public:
	/// Initializes a new instance of the Stack object.
	/// Params:
	///		Capacity = The initial number of elements this Stack is capable of storing.
	this(size_t Capacity = 4) {
		enforce(Capacity > 0, "Initial capacity must be greater than zero.");		
		this._Capacity = Capacity;
		Allocate(_Capacity);
	}

	/// The maximum amount of elements this Stack is capable of storing prior to resizing.
	@property size_t Capacity() const {
		return _Capacity;
	}

	/// Returns the number of elements in this Stack.
	@property size_t Count() const {
		return _Count;
	}

	/// Pops an element from the Stack.
	/// If no elements are available, DefaultValue is evaluated and returned.	
	T Pop(lazy T DefaultValue = T.init) {		
		if(_Count == 0)
			return DefaultValue();
		return *(Elements + _Count-- - 1);		
	}


	/// Pops an element from the Stack without altering the state of the Stack.
	/// If no elements are available, DefaultValue is evaluated and returned.
	@property T Peek(lazy T DefaultValue = T.init) {
		if(_Count == 0)
			return DefaultValue();
		return *(Elements + _Count -1);
	}

	/// Pushes the given value to the top of the stack.
	/// Params:
	/// 	Value = The value to push onto the stack.
	void Push(T Value) {
		if(_Count >= _Capacity)
			Resize(_Count + 1);
		*(Elements + _Count++) = Value;
	}

	unittest {
		Stack!int Tests = new Stack!int();
		Tests.Push(3);
		Tests.Push(6);
		Tests.Push(9);
		assert(Tests.Count == 3);
		assert(Tests.Pop() == 9);
		assert(Tests.Pop() == 6);
		assert(Tests.Peek() == 3);
		assert(Tests.Pop() == 3);
		assert(Tests.Count == 0);
	}
	
private:	
	T* Elements;
	size_t _Capacity;
	size_t _Count;

	void Resize(size_t MinCapacity) {
		assert(MinCapacity > Capacity);
		do 
			_Capacity <<= 1;
		while(_Capacity < MinCapacity);		
		Allocate(_Capacity);
	}

	void Allocate(size_t Capacity) {
		// Can use malloc for primitives like int, but objects need to have a reference kept. We just use (add/remove)Range in this case.		 
		T* New = cast(T*)malloc(T.sizeof * Capacity);
		GC.addRange(New, T.sizeof * Capacity);
		if(Elements) {			
			memcpy(New, Elements, T.sizeof * _Count);			
			GC.removeRange(Elements);
			free(Elements);		
		}
		this.Elements = New;		
	}
}