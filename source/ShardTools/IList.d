/// Provides an interface for a collection that is ordered and indexable.
/// $(RED This interface should be considered deprecated and no longer used in new code.)
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.IList;
import ShardTools.ICollection;

// TODO: Phase this out and remove it.

/// The base interface for a collection accessible by index.
/// $(RED This interface should be considered deprecated and no longer used in new code.)
/// Params:
/// 	T = The type of the elements in this list.
interface IList(T) : ICollection!T {
	/// Implements the Index Assignment operator by Setting the element at the specified Index to the specified Value.
	/// Params:
	///		Value = The value to assign the specified index to.
	///		Index = The index to assign the value at.
	final T opIndexAssign(T Value, size_t Index) {
		Set(Index, Value);
		return Value;
	}

	/// Implements the Index operator by getting the element At the specified index.
	/// Params:
	///		Index = The index to get the element at.
	final T opIndex(size_t Index) {	
		return At(Index);
	}

	/// Returns the index of the specified element in this collection, or -1 if it was not found.
	///	Params:
	///		Element = The element to get the index of.
	size_t IndexOf(T Element);
	
	/// Removes the element at the specified index.
	///	Params:
	///		Index = The index to remove the element at.
	void RemoveAt(size_t Index);	

	/// Returns the element at the specified, zero-based, index.
	/// Params:
	///		Index = The zero-based index to get the element at.
	T At(size_t Index);

}