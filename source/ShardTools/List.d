/// Provides a simple array-list implementation.
/// $(RED This module should be considered deprecated.)
module ShardTools.List;
private import ShardTools.IList;
import std.conv;
import ShardTools.ICollection;
import std.exception;
import std.algorithm;

import std.range;

/// Represents a mutable list accessibly by index.
class List(T) : ICollection!(T), IList!(T) {

public:
	/// Initializes a new instance of the List object.
	/// Params:
	///		Capacity = The number of elements this list is initially capable of storing.
	this(size_t Capacity = 4) {
		assert(Capacity >= 0);
		_Elements = new T[Capacity];
		_Elements.assumeSafeAppend();
		_Elements.length = 0;		
	}

	/// Removes the element at the specified index.
	///	Params:
	///		Index = The index to remove the element at.
	void RemoveAt(size_t Index) {
		//assert(Index >= 0 && Index < _Elements.length,  "List out of bounds. Index " ~ to!string(Index) ~ ".");		
		for(size_t i = Index; i < Count - 1; i++)
			_Elements[i] = _Elements[i + 1];
		_Elements.length = _Elements.length - 1;		
	}

	/// Returns the element at the specified, zero-based, index.
	/// Params:
	///		Index = The zero-based index to get the element at.
	T At(size_t Index) {
		assert(Index >= 0 && Index < _Elements.length, "List out of bounds. Index " ~ to!string(Index) ~ ".");
		return _Elements[Index];
	}

	/// Sets the element at the specified, zero-based, index to the specified value.
	/// Params:
	///		Index = The zero-based index to set the element at.
	///		Value = The value to set the element to.
	void Set(size_t Index, T Value) {
		assert(Index >= 0 && Index < _Elements.length,  "List out of bounds. Index " ~ to!string(Index) ~ ".");
		_Elements[Index] = Value;
	}
	
	/// Adds the specified element to this collection.
	/// Params:
	///		Element = The element to add to the collection.
	/// Returns:
	///		The same instance of the element that was added.
	final T Add(T Element) {							
		_Elements.length = _Elements.length + 1;		
		Set(_Elements.length - 1, Element);		
		return Element;
	}

	/// Gets the underlying elements contained in this list.	
	final T[] Elements() {
		return _Elements;
	}

	/// Returns the number of elements this list is capable of storing.
	final @property size_t Capacity() const {
		return _Elements.capacity;
	}

	/// Gets the number of elements in this collection.
	final @property size_t Count() const {
		return _Elements.length;
	}

	/// Removes the specified element from this collection.
	/// Returns:
	///		A boolean value indicating whether the element was removed.
	final bool Remove(T Element) {
		size_t Index = IndexOf(Element);
		if(Index == -1)
			return false;
		RemoveAt(Index);
		return true;
	}	

	/// Returns whether this collection contains the specified element.
	/// Params:
	///		Element = The element to check for containment.
	final bool Contains(T Element) {
		return IndexOf(Element) != -1;
	}	

	/// Clears this collection, removing all elements contained by it.
	final void Clear() {
		while(Count > 0)
			RemoveAt(Count - 1);
	}	

	/// Shifts all of the elements, starting at the specified index, by the specified amount.
	/// Performs a wrap-around as needed.
	/// Params:
	/// 	Index = The index to begin shifting the elements at.
	/// 	Amount = The amount to shift the elements by.
	private void ShiftRight(size_t Index, int Amount) {
		_Elements.length += Amount;
		for(size_t i = _Elements.length - 1; i > Index; i--)
			_Elements[i] = _Elements[i - 1];

	}

	/// Inserts the element into the given position.
	/// Params:
	/// 	Index = The index to insert the element at.
	/// 	Value = The element to insert.
	final void Insert(size_t Index, T Value) {
		 ShiftRight(Index, 1);
		 Set(Index, Value);
	}

	/// Returns the index of the specified element in this collection, or -1 if it was not found.
	///	Params:
	///		Element = The element to get the index of.
	final size_t IndexOf(T Element) {
		static if(is(T == interface)) {
			for(size_t i = 0; i < Count; i++)
				if(cast(Object)At(i) == cast(Object)Element)
					return i;								
		} else {
			for(size_t i = 0; i < Count; i++)
				if(At(i) == Element)
					return i;
		}
		return -1;
	}

	/// Inserts the given element prior to the relative element.
	/// Params:
	/// 	RelativeTo = The element to insert an element prior to.
	/// 	Value = The element to insert.
	final void InsertBefore(T RelativeTo, T Value) {
		size_t Index = IndexOf(RelativeTo);
		enforce(Index != -1, "The element to insert relative to was not found in the collection.");
		Insert(Index, Value);
	}

	/// Inserts the given element after to the relative element.
	/// Params:
	/// 	RelativeTo = The element to insert an element prior to.
	/// 	Value = The element to insert.
	final void InsertAfter(T RelativeTo, T Value) {
		size_t Index = IndexOf(RelativeTo);
		enforce(Index != -1, "The element to insert relative to was not found in the collection.");
		Insert(Index + 1, Value);
	}

	int opApply(int delegate(ref T) dg) {
		int Result;
		foreach(ref T Element; _Elements)
			if((Result = dg(Element)) != 0)
				break;
		return Result;
	}

	unittest {
		List!size_t TestList = new List!size_t(10);
		assert(TestList.Count == 0);
		for(size_t i = 0; i < 10; i++)
			TestList.Add(i);
		assert(TestList.IndexOf(3) == 3);
		assert(TestList.Contains(4));
		assert(TestList.IndexOf(10) == -1);
		assert(!TestList.Contains(120));
		assert(TestList.Remove(3));
		assert(TestList.Count == 9);
		assert(!TestList.Contains(3));
		TestList.Insert(1, 100);
		assert(TestList[1] == 100);
		assert(TestList[0] == 0);
		assert(TestList[2] == 1);
		
		TestList.Clear();
		assert(TestList.Count == 0);		
	}


private:	
	T[] _Elements;
}