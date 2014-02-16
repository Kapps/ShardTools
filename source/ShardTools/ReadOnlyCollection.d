/// Wraps an existing collection to make it read-only.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.ReadOnlyCollection;
private import std.traits;

/// A wrapper used to provide readonly access to an underlying collection.
/// The readonly aspect is not transitive, so ref types are still non-const.
/// Params:
///		T = The type of the elements within the collection.
///		CollectionType = The type of collection being wrapped.
struct ReadOnlyCollection(T, CollectionType) if(isIterable!CollectionType || is(typeof(CollectionType[0])) && is(typeof(CollectionType.length))) {

public:
	/// Initializes a new instance of the ReadOnlyCollection object.
	/// Params:
	///		Collection = The collection to wrap.
	this(CollectionType Collection) {
		this.Underlying = Collection;
	}

	static if(is(typeof(Underlying.length))) {
		@property size_t length() const {
			return Underlying.length;
		}
	} else static if(is(typeof(Underlying.Count))) {
		@property size_t length() const {
			return Underlying.Count;
		}
	}

	/// Provides foreach access to the collection.
	int opApply(int delegate(ref T) Callback) {
		int Result = 0;
		static if(isIterable!CollectionType) {		
			foreach(Element; Underlying)
				if((Result = Callback(Element)) != 0)
					break;
		} else {
			for(size_t i = 0; i < Underlying.length; i++)
				if((Result = Callback(Underlying[i])) != 0)
					break;
		}
		return Result;
	}

	static if(is(typeof(Underlying[0]))) {
		/// Provides index access to the underlying collection.
		@property T opIndex(size_t Index) {
			return Underlying[Index];
		}
	}
	
private:
	CollectionType Underlying;
}