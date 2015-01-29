module ShardTools.HashTable;
import std.math;
import core.stdc.string;
import core.stdc.stdlib;
import std.typecons;
import std.functional;
import std.algorithm;
import std.range;

import ShardTools.Logger;
import ShardTools.ConsoleLogger;
import std.format;
import std.traits;
import core.memory;
import ShardTools.Udas;

//@nogc:

/// Represents a manual memory managed HashTable, capable of using custom comparison functions.
/// This struct is ref-counted for convenience.
/// Bugs:
/// 	Using `Key.init` as a key is not yet supported. This is particularly harmful when using integer types as keys.
alias HashTable(Key, Value) = RefCounted!(HashTableImpl!(Key, Value), RefCountedAutoInitialize.yes);

/// Ditto
struct HashTableImpl(Key, Value, alias comparer = (a, b) => a == b) {

	/// Creates a new HashTable with the given load boundaries.
	/// The load boundaries indicate at what $(D loadFactor) the HashTable is resized.
	this(float loadMin = 0.33f, float loadMax = 0.66f) {
		this.loadMin = loadMin;
		this.loadMax = loadMax;
	}

	void toString(scope void delegate(const(char)[]) sink, FormatSpec!char fmt) const {
		sink("HashTable(");
		if(length > 64) {
			sink.formatValue(_numRoots, fmt);
			sink(" Roots, ");
			sink.formatValue(length, fmt);
			sink(" Elements)");
			return;
		}
		bool first = true;
		foreach(e; _entries) {
			if(e.key == Key.init)
				continue;
			if(!first)
				sink(", ");
			sink.formatValue(e, fmt);
			first = false;
		}
		sink(")");
	}
//@nogc:

	/// Gets the number of elements currently in the HashTable.
	@property size_t length() const {
		return _length;
	}

	/// Returns the current load factor of the HashTable.
	/// This is the number of buckets divided by the number of entries (therefore always less than or equal to 1).
	@property float loadFactor() const {
		return _entries.length == 0 ? 1 : cast(float)_numRoots / _entries.length;
	}

	/// Adds the given value to this HashTable.
	/// If the given key already exists, the value is replaced.
	/// The key must not be null (or $(D key.init)).
	void opIndexAssign(Value value, Key key) {
		//logf("Adding %s -> %s.", key, value);
		assert(key != Key.init);
		//logf("Old length is %s, load factor is %s.", _length, loadFactor);
		if(loadFactor >= loadMax)
			resizeTable(max(_entries.length * 2, MIN_ENTRIES));
		auto existing = entryForItem(key);
		//logf("Existing entry is %s.", existing);
		if(existing)
			existing.value = value;
		else {
			addUnchecked(key, value, null);
			_length++;
		}
		//logf("After add, new load factor is %s and new length is %s.", loadFactor, length);
	}

	/// Gets the value with the given key from this HashTable.
	/// If no value exists, $(D Value.init) is returned.
	/// Because $(D Value.init) is a valid value, this may be differentiated with either $(D contains) or $(D tryGet).
	Value opIndex(Key key) {
		auto entry = entryForItem(key);
		return entry is null ? Value.init : entry.value;
	}

	/// Returns a pointer to the value associated with the given key,
	/// or null if the key was not present in the HashTable.
	Value* opIn_r(Key key) {
		auto entry = entryForItem(key);
		return entry is null ? null : &entry.value;
	}

	/// Indicates if this HashTable contains the given key.
	bool contains(Key key) {
		auto entry = entryForItem(key);
		return entry !is null;
	}

	/// Removes the element with the given key from this HashTable.
	/// Returns the value of the element that was removed, or $(D Value.init) if not found. 
	Value remove(Key key) {
		if(_entries.length == 0)
			return Value.init;
		auto idx = indexForKey(key);
		auto e = &_entries[idx];
		if(comparer(e.key, key)) {
			Value res = e.value;
			// Root entry is the one to remove, copy the next one to the root if present, clearing old node.
			if(auto next = e.next) {
				memcpy(e, next, Entry.sizeof);
				free(next);
			} else {
				// If no next, just zero it out.
				memset(e, 0, Entry.sizeof);
			}
			_length--;
			_numRoots--;
			tryLogf("Load factor is %s.", loadFactor);
			if(loadFactor <= loadMin && _entries.length > MIN_ENTRIES * 2)
				resizeTable(_entries.length / 2);
			return res;
		} else {
			auto prev = e;
			for(Entry* curr = _entries[idx].next; curr !is null; curr = curr.next) {
				if(comparer(curr.key, key)) {
					auto val = curr.value;
					prev.next = curr.next;
					free(curr);
					_length--;
					return val;
				}
				prev = curr;
			}
		}
		return Value.init;
	}

	/// Ensures that this HashTable has room for at least the given number of non-colliding entries.
	/// The $(D reserve) method attempts to reserve additional space to avoid a resize due to load factor when reaching $(D numEntries) elements.
	/// If you want exactly the number of entries you specify to be reserved, use $(D reserveExact) instead.
	void reserve(size_t numEntries) {
		// Try to roughly fit the right amount of elements.
		auto loadSize = cast(size_t)(numEntries * (1f / loadMax) * 1.1f);
		reserveExact(loadSize);
	}

	/// Ditto
	void reserveExact(size_t numEntries) {
		assert(numEntries >= 0);
		size_t minSize = _length + numEntries;
		if(_entries.length < minSize)
			resizeTable(minSize);
	}

	/// Clears this HashTable, removing all entries associated with it.
	void clear() {
		freeData(_entries);
		_length = 0;
		_numRoots = 0;
	}

	/// Returns an input range containing a tuple of each entry (key, value) in this HashTable.
	/// The range is invalidated if an entry is added or removed.
	@property auto byEntry()() {
		static struct Result {
			this(Entry[] entries) {
				this.entries = entries;
				this.index = -1;
				popFront();

			}
			@property bool empty() const {
				return index >= entries.length;
			}
			@property auto front() {
				return Tuple!(Key, "key", Value, "value")(item.key, item.value);
			}
			void popFront() {
				if(item && item.next)
					item = item.next;
				else {
					do {
						index++;
						if(index == entries.length) {
							item = null;
							break;
						} else
							item = &entries[index];
					} while(item.key == Key.init);
				}
			}
				
			Result save() {
				auto res = Result(entries);
				res.index = this.index;
				res.item = this.item;
				return res;
			}

			private Entry[] entries;
			private Entry* item;
			private size_t index;
		}
		return Result(_entries);
	}

	/// Returns an input range containing the keys within this HashTable.
	@property auto byKey()() {
		return byEntry.map!(c=>c.key);
	}

	/// Returns an input range containing the keys within this HashTable.
	@property auto byValue()() {
		return byEntry.map!(c=>c.value);
	}

private /+@nogc+/:
	Entry[] _entries;
	size_t _numRoots;
	size_t _length;
	float loadMin = 0.33f;
	float loadMax = 0.66f;

	Entry* entryForItem(Key key) {
		if(_entries.length == 0)
			return null;
		if(key == Key.init)
			return null;
		auto idx = indexForKey(key);
		for(Entry* e = &_entries[idx]; e !is null; e = e.next) {
			if(comparer(e.key, key))
				return e;
		}
		return null;
	}

	alias GCFun = @nogc void function(in void*) nothrow;

	void resizeTable(size_t newSize) {
		tryLogf("Resizing from %s to %s elements.", _entries.length, newSize);
		Entry[] _oldEntries = _entries;
		scope(exit) {
			if(_oldEntries) {
				static if(hasIndirections!Key || hasIndirections!Value) {
					auto remRange = cast(GCFun)&GC.removeRange;
					remRange(_oldEntries.ptr);
				}
				free(_oldEntries.ptr); // Don't free the elements of it because we reuse them.
			}
		}
		Entry[] _newEntries = (cast(Entry*)calloc(newSize, Entry.sizeof))[0..newSize];
		if(_newEntries.ptr is null) {
			static const err = new Error("Ran out of memory when allocating entries for the HashTable.");
			throw err;
		}
		static if(hasIndirections!Key || hasIndirections!Value) {
			auto addRange = cast(GCFun)&GC.addRange;
			addRange(_newEntries.ptr);
		}
		this._entries = _newEntries;
		// Copy over by assigning new entities then just re-assigning values.
		// Can reuse most of the old nodes, but have to clear roots since re-adding.
		_numRoots = 0;
		for(size_t i = 0; i < _oldEntries.length; i++) {
			Entry* e = &_oldEntries[i];
			if(e.key == Key.init)
				continue; // No key means root is not in use.
			Entry* root = e;
			while(e !is null) {
				// Necessary since the memory is invalid after.
				auto next = e.next;
				// Reuse non-root memory. Root is going to be freed, so can't reuse that.
				addUnchecked(e.key, e.value, e is root ? null : e);
				e = next;
			}
		}
		//tryLogf("Resized elements.");
	}

	size_t indexForKey(Key key) {
		// TODO: Look into using a hash method that doesn't generate as many collisions.
		size_t hash = calcHash(key);
		return hash % _entries.length;
	}

	size_t calcHash(Key key) {
		// TODO: Ideally getHash would be @nogc.
		auto ti = typeid(key);
		auto dg = &ti.getHash;
		alias hashDel = @nogc @trusted const size_t delegate(in void*);
		auto casted = cast(hashDel)dg;
		return casted(&key);
	}

	// Adds without checking for duplicate keys or invalid keys.
	// Optionally, use buff instead of allocating a node; if no need for allocation, free buff.
	// Does not increment length nor increment the table size.
	void addUnchecked(Key key, Value value, Entry* buff) {
		//tryLogf("Adding unchecked %s -> %s. Reusing buffer %s.", key, value, buff);
		size_t index = indexForKey(key);
		auto entry = &_entries[index];
		// TODO: Have to manage creating entries (aka now need to start malloc/free or else track it).
		// We already start off with one entry, so only need to use malloc/free if we create a new one.
		if(entry.key != Key.init) {
			// Handle a collision by moving the current root to the heap, then using the root for this new node.
			Entry* next = buff is null ? cast(Entry*)malloc(Entry.sizeof) : buff;
			memcpy(next, entry, Entry.sizeof);
			entry.next = next;
		} else {
			entry.next = null;
			_numRoots++;
			if(buff !is null) {
				free(buff);
				buff = null;
			}
		}
		// In both situations, replace the root values with the new values.
		entry.key = key;
		entry.value = value;
	}

	static void freeData(ref Entry[] entries) {
		if(entries is null)
			return;
		foreach(root; entries) {
			if(root.key is Key.init)
				continue;
			for(Entry* e = root.next; e !is null; e = e.next)
				free(e);
		}
		free(entries.ptr);
		entries = null;
	}

	enum size_t MIN_ENTRIES = 4;
	
	enum EntryFlags : size_t {
		none = 0,
		unused = 1
	}

	struct Entry {
		Key key;
		Value value;
		Entry* next;

		void toString(scope void delegate(const(char)[]) sink, FormatSpec!char fmt) const {
			if(key == Key.init) {
				sink("[]");
				return;
			}
			sink("[");
			sink.formatValue(key, fmt);
			sink(":");
			sink.formatValue(value, fmt);
			sink("]");
			if(next) {
				sink("->");
				next.toString(sink, fmt);
			}
		}
	}
}



@name("HashTable Basic Entries")
unittest {
	/+struct HashPair(V) {
		this(size_t hash, V val) {
			this.hash = hash;
			this.val = val;
		}
		size_t opHash() {
			return hash;
		}
		size_t hash;
		V val;
	}+/
	HashTable!(string, int) a;
	assert(a.length == 0);
	a["one"] = 1;
	assert(a.contains("one"));
	assert(!a.contains("two"));
	a["two"] = 2;
	assert(a.contains("two"));
	a["six"] = 6;
	assert(a.length == 3);
	assert(a.byValue.sum == 9);
	assert(a._entries.length == 4);
	auto sP = "six" in a;
	assert(sP);
	assert(*sP == 6);
	*sP = 7;
	assert(a["six"] == 7);
	assert(a.byValue.sum == 10);
	assert(*("six" in a) == 7);
	a.remove("two");
	assert(a.length == 2);
	assert(a.byValue.sum == 8);
	a.remove("one");
	assert(a.length == 1);
	assert(a._entries.length == 4);
	assert(!a.contains("two"));
	assert(!a.contains("one"));
	assert(a.contains("six"));
	a.clear();
	assert(a.length == 0);
	assert(!a.contains("one"));
}
@name("HashTable Resizing Entries")
unittest {
	auto b = HashTable!(size_t, size_t)(0.4f, 0.66f);
	for(size_t i = 1; i <= 256; i++)
		b[i] = i;
	assert(b.length == 256);
	assert(b._numRoots <= 256);
	for(size_t i = 1; i <= 256; i++)
		assert(b[i] == i);
	auto oldRoots = b._numRoots;
	for(size_t i = 1; i <= 156; i++) {
		auto removed = b.remove(i);
		assert(removed == i);
	}
	assert(b._numRoots < oldRoots);
	assert(b._entries.length == 128);
	assert(b.length == 100);
	for(size_t i = 1; i <= 156; i++) {
		assert(!b.contains(i));
		assert((i in b) is null);
	}
	for(size_t i = 157; i <= 256; i++) {
		assert(b.contains(i));
		assert(b[i] == i);
		assert(i in b);
		assert(*(i in b) == i);
	}
}

@name("HashTable Capacity")
unittest {
	HashTable!(string, int) a;
	assert(a.length == 0);
	a.reserve(4);
	assert(a._entries.length >= 4);
	auto oldLen = a._entries.length;
	a.reserve(1);
	assert(a._entries.length == oldLen);
	oldLen = a._entries.length;
	a.reserveExact(4);
	assert(a._entries.length == oldLen);
	a["one"] = 1;
	assert(a._entries.length == oldLen);
	assert(a.length == 1);
}

@name("HashTable Special Cases")
unittest {
	import std.conv;
	// Mostly to improve coverage.
	auto a = HashTable!(string, size_t)(0.25f, 1f);
	assert(a.remove("0") == size_t.init);
	for(size_t i = 1; i <= 256; i++) {
		a[i.text] = i;
		assert(a[i.text] == i);
	}
	assert(a.length == 256);
	assert(a.remove("257") == 0);
	assert(a.length == 256);
	a["255"] = 257;
	assert(a.remove("255") == 257);
	assert(a.length == 255);
	a.clear();
	assert(a.length == 0);
	assert(a._entries.length == 0);
}