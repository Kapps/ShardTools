module ShardTools.ManagedList;
public import ShardTools.List;
public import ShardTools.Event;

/// Represents a mutable collection capable of resizing itself and creating events when modified.
class ManagedList(T) : List!(T) {	

public:

	alias Event!(void, ManagedList!(T), T) ListEvent;

	/// An event raised when an item is added to the collection, or an element is replaced with a new one.
	/// ItemRemoved is called on the pre-existing element.
	@property ListEvent ItemAdded() {
		return _ItemAdded;
	}

	/// An event raised when an item is removed from the collection, or when an element is replaced by a new one.
	/// ItemAdded is called on the newly assigned value.
	@property ListEvent ItemRemoved() {
		return _ItemRemoved;
	}

	/// Initializes a new instance of the ManagedList object.
	/// Params:
	///		Capacity = The number of elements this list is initially capable of storing.
	this(int Capacity = 4) {
		super(Capacity);
		_ItemAdded = new typeof(_ItemAdded)();
		_ItemRemoved = new typeof(_ItemRemoved)();
	}	

	/// Sets the element at the specified, zero-based, index to the specified value.
	/// Params:
	///		Index = The zero-based index to set the element at.
	///		Value = The value to set the element to.
	override void Set(size_t Index, T Value) {
		super.Set(Index, Value);
		_ItemAdded.Execute(this, Value);
	}
	
	/// Removes the element at the specified index.
	///	Params:
	///		Index = The index to remove the element at.
	override void RemoveAt(size_t Index) {
		T Value = At(Index);
		super.RemoveAt(Index);
		_ItemRemoved(this, Value);
	}

private:
	ListEvent _ItemAdded;
	ListEvent _ItemRemoved;
}