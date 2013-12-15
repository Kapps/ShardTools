/// Provides a sorted array list. This module is deprecated as the implementation is poor and unnecessary.
module ShardTools.SortedList;
private import std.exception;
import std.array;

// TODO: Either phase this class out, or re-do implementation.
// Current one is far from ideal.

/// Represents a list sorted by an integer key.
/// $(RED The implementation for this class is poor, and the class may be deprecated in the future.)
class SortedList(T) {
	
	/// Initializes a new instance of the SortedList class.
	/// Params: Capacity = The number of elements to be capable of storing initially.
	this(size_t Capacity = 4) {
		enforce(Capacity > 0);
		Items = new SortedListItem[Capacity];
		assumeSafeAppend(Items);
		_Length = 0;
	}
	
	/// Removes the specified item from the collection.
	/// Params: item = The item to remove.
	bool Remove(T item) {
		size_t index = -1;
		for(index = 0; index < _Length; index++) {
			if(Items[index].Item == item)
				break;			
		}		
		if(index == _Length)
			return false;
		RemoveAt(index);	
		return true;
	}

	/// Removes the element at the given index.
	/// Params:
	/// 	Index = The index to remove the element at.
	void RemoveAt(size_t Index) {
		_Length--;
		for(size_t i = Index; i < _Length; i++)
			Items[Index] = Items[Index + 1];
		Items.length = Items.length - 1;		
	}
	
	/// Adds the specified item to the collection.
	/// Params: 
	///		item = The item to add.
	///		Key = The key of the item to add.
	void Add(T item, size_t Key) {
		size_t Index = GetIndexForKey(Key);
		_Length++;				
		//Items.insertInPlace(Index, new SortedListItem(item, Key));	
		Items = Items[0..Index] ~ new SortedListItem(item, Key) ~ Items[Index..$];
	}
	
	/// Returns the number of elements inside this SortedList.
	@property size_t Count() {
		return _Length;	
	}	

	T opIndex(size_t Index) {
		assert(Index < _Length && Index >= 0);
		return Items[Index].Item;
	}

	int opApply(int delegate(ref T) dg) {
		int Result;
		foreach(ref Item; Items)
			if((Result = dg(Item.Item)) != 0)
				break;
		return Result;
	}
	
private:
	SortedListItem[] Items;
	size_t _Length;
	
	class SortedListItem {
		T Item;
		size_t Key;
		
		this(T Item, size_t Key) {
			this.Item = Item;
			this.Key = Key;
		}
		
		public override int opCmp(Object other) {
			SortedListItem item = cast(SortedListItem)other;
			return Key > item.Key ? 1 : Key == item.Key ? 0 : -1;			
		}
	}
	
	size_t GetIndexForKey(size_t Key) {
		// TODO: Binary search.
		for(size_t i = 0; i < _Length; i++) {
			if(Items[i].Key >= Key)
				return i;
		}
		return _Length;		
	}
}