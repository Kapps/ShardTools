module ShardTools.ICollection;
import ShardTools.List;
import std.conv;
import std.exception;

// TODO: Phase this out and remove it.
// Also, really, the point of this was to not need index, yet there's a Set.

/// An interface for a mutable collection accessible by index.
/// $(RED This module comes from a time of less familiarity with D, and should be considered deprecated.)
interface ICollection(T) {

	/// Gets the number of elements in this collection.
	size_t Count();

	/// Adds the specified element to this collection.
	/// Params:
	///		Element = The element to add to the collection.
	/// Returns:
	/// 	The same instance of Element that was passed in.
	T Add(T Element);

	/// Removes the specified element from this collection.
	/// Params:
	///		Element = The element to remove.
	/// Returns:
	///		A boolean value indicating whether the element was removed.
	bool Remove(T Element);

	/// Returns whether this collection contains the specified element.
	/// Params:
	///		Element = The element to check for containment.
	bool Contains(T Element);	

	/// Clears this collection, removing all elements contained in it.
	void Clear();	

	/// Sets the element at the specified, zero-based, index to the specified value.
	/// Params:
	///		Index = The zero-based index to set the element at.
	///		Value = The value to set the element to.
	void Set(size_t Index, T Value);

	/// Implements the append operator by Adding the element.
	/// Params:
	///		Element = The element to append.
	final ICollection!(T) opUnary(string s)(T Element) if(s == "~") {
		Add(Element);
		return this;
	}	
}