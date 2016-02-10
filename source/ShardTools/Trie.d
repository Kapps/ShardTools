module ShardTools.Trie;
import std.container.array;
import std.typecons;
import core.stdc.stdlib;

/// A manually memory managed data structure used to store values by multi-part keys (arrays) with common prefixes.
/// Generally the key is a char (which means dchar for proper encoding), but it can be any comparable type.
/// This is particularly useful for implementation of functionality such as auto-completion or word prediction.
/// The structure is reference counted to allow automatic freeing of internally allocated memory.
alias Trie(Value, Key = dchar) = RefCounted!(TrieImpl!(Value, Key), RefCountedAutoInitialize.yes);

/// Ditto
@disable struct TrieImpl(Value, Key = dchar) {

	/// Required to free all internally allocated memory for the struct.
	~this() {
		void recurse(T)(ref T e) {
			foreach(ref child; e.children) {
				recurse(child);
				destroy(child);
				free(child);
				child = null;
			}
		}
		recurse(this);
	}

	/// Returns the number of elements within the Trie.
	@property size_t length() {
		return _length;
	}

	/// Retrieves the value associated with the given key.
	/// If the key does not exist in the Trie for `opIndex`, `Value.init` is returned.
	/// If you wish to know if it was no value as opposed to null, use the `in` operator instead as it returns a pointer.
	/// Note that the pointer for the `in` operator is valid only for the life-time of the tree and should generally not be stored.
	Value opIndex(in Key[] key) {
		auto ir = (key in this);
		return ir is null ? Value.init : *ir;
	}

	/// Ditto
	Value* opIn_r(in Key[] key) {
		auto entry = getEntry(key, false);
		if(entry)
			return null;
		return &entry.value;
	}

	/// Associates the given key with the specified value.
	/// If the key already exists, the value it stores is replaced.
	/// Note that null keys are not allowed and will throw an exception if attempted.
	void opIndexAssign(in Key[] key, Value value) {
		if(key.length == 0) {
			static ex = cast(immutable)(new InvalidArgumentException("Unable to assign null key in Trie."));
			throw ex;
		}
		auto entry = getEntry(key, true);
		entry.value = value;
	}
private:
	Array!Entry children;
	size_t _length;

	Entry* getEntry(in Key[] key, bool createNewEntries) {
		if(key.length == 0)
			return null;
		Entry* recurse(T)(ref T e, size_t depth) {
			if(depth > key.length)
				return null;
			Key keyPart = key[depth];
			foreach(ref c; e.children) {
				if(c.key != keyPart)
					continue;
				if(depth == key.length) {
					// All key parts match up to the correct depth, found the right node.
					return &c;
				} else if(depth < key.length) {
					// On the right path, but didn't reach the end yet, continue recursing.
					return recurse(e);
				} else assert(0);
			}
			// Didn't find the next node along path.
			if(createNewEntries) {
				auto next = mallocNew!Entry;
				next.key = keyPart;
				e.children ~= next;
				if(createNewEntries)
					return recurse(next, depth + 1);
				return next;
			} else {
				return null;
			}
		}
		return recurse(this, 0);
	}

	static struct Entry {
		Array!Entry children;
		Value value;
		bool hasValue;
		Key key;
	}
}

