/// A dictionary class used to lookup an element by key.
module ShardTools.Map;

private import std.string;

/// A dictionary class used to lookup an element by key.
/// For the moment, this class is merely a wrapper around an associative array.
/// $(RED This class should be considered deprecated as it provides no benefits over an AA.)
class Map(Key, Value) {

public:
	/// Initializes a new instance of the Map object.
	this() {
		
	}

	/// Gets the element associated with the specified key.
	Value Get(Key Key, lazy Value DefaultValue = Value.init) {		
		Value* Result = (Key in Elements);
		if(Result is null)
			return DefaultValue;
		return *Result;
	}

	unittest {
		Map!(string, string) TmpMap = new Map!(string, string);
		TmpMap.Set("test", "testing");
		assert(cmp(TmpMap.Get("test"), "testing") == 0);
		assert(cmp(TmpMap.Get("te" ~ "st"), "testing") == 0);
	}

	/// Gets the number of elements contained in this Dictionary.
	size_t Count() const {
		return Elements.length;
	}

	/// Rearranges the collection to make lookups more efficient.
	void Rehash() {
		Elements.rehash;	
	}

	/// Sets the element associated with the specified Key to the specified Value.
	///	This method is also used to add elements to the collection.
	/// Params:
	///		Key = The key of the element.
	///		Value = The value to set the key to.
	void Set(Key Key, Value Value) {
		Elements[Key] = Value;
	}

	/// Removes the given key from this map.
	/// Params:
	/// 	Key = The key to remove from this map.
	/// Returns:
	///		Whether the key was removed.
	bool Remove(Key Key) {
		return Elements.remove(Key);
	}

	/// Gets the values contained in this collection.
	Value[] Values() {
		if(Elements.length == 0)
			return new Value[0];
		return Elements.values;
	}

	/// Gets the keys contained in this collection.
	Key[] Keys() {
		if(Elements.length == 0)
			return new Key[0];
		return Elements.keys;
	}

	/// Provides readonly access to the underlying array for this map.
	Value[Key] GetUnderlyingArray() {
		return Elements;
	}

	/// Determines whether this collection contains the specified key.
	bool ContainsKey(Key Key) const {
		return (Key in Elements) is null;
	}

	Value opIndex(Key Key) {
		return Get(Key);
	}

	int opApply(int delegate(Key, ref Value) dg) {
		//return Elements.opApply(dg);
		foreach(Key, ref Value; Elements) {
			int res = dg(Key, Value);
			if(res != 0)
				return res;
		}
		return 0;
	}

	/// Removes all of the elements in this collection.
	void Clear() {
		// TODO: Just create a new AA instead?
		Key[] Keys = this.Keys();
		foreach(Key; Keys)
			Remove(Key);
	}
	
private:
	Value[Key] Elements;
	
	// TODO: Allow using a custom comparer.
	// Either make two delegates (int Compare and hash_t HashCode), or make an IComparer struct.
	/+	
		IComparer!Value Comparer;
		Map!(Key, Value) Parent;
	}+/
}