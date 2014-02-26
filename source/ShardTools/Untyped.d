/// Contains a struct for simple wrapping of data in an untyped manner.
/// Generally this is not as safe or flexible as std.Variant, but is much more efficient.
module ShardTools.Untyped;
private import core.memory;
import std.conv;
import std.traits;
import ShardTools.ExceptionTools;
import std.exception;
import core.stdc.string;
import std.typecons;
import ShardTools.Udas;

mixin(MakeException("InvalidCastException", "The type stored in Untyped did not match the given type."));

/// Provides a fast and lightweight structure to store a value of an unknown type.
/// The result must be the exact same type as was passed in for structs.
/// For classes, a cast will be attempted.
/// Untyped will allocate GC memory for structs that are greater than the size of a void pointer.
/// Otherwise, Untyped will allocate only if typeid allocates or if an exception is thrown.
/// $(RED The implementation of this struct is still extremely poor; use with caution.)
struct Untyped {	

	this(T)(T Value) {
		store(Value);
	}

	alias get this;

	T opCast(T)() {
		return get!T;
	}
	
	/// Gets the type that's stored within this instance.
	@property TypeInfo type() {
		return StoredType;	
	}

	/// Gets the underlying value as the given type.
	/// This must be the exact type of the value that was passed in for structs.
	/// For classes a cast is attempted provided that both the stored and requested types are classes.
	@property T get(T)() {
		T result;
		if(!tryGet!(T)(result))
			throw new InvalidCastException("Unable to cast from " ~ to!string(StoredType) ~ " to " ~ to!string(typeid(T)) ~ ".");
		return result;
	}

	/// Attempts to get the given value, returning whether the attempt was successful.
	/// That is, if get would throw, this returns false; otherwise true.
	bool tryGet(T)(out T value) {
		// TODO: Do a version(NoBoundsCheck) or something here, and skip the check optionally.
		// I'd imagine the check is fairly expensive, especially with the currently slow(?) implementation of typeid comparison.
		// TODO: Consider allowing signed vs unsigned primitives. And arrays of such?
		TypeInfo ti = typeid(T);
		TypeInfo_Class classType = cast(TypeInfo_Class)StoredType;
		if(StoredType != ti) {
			//debug std.stdio.writefln("Trying to cast from %s to %s.", StoredType, ti);
			if(cast(TypeInfo_Class)StoredType) {
				//debug std.stdio.writefln("Determined StoredType is class: %s.", cast(TypeInfo_Class)StoredType);
				// First, check if our result can be casted to that type, if it's a class.
				static if(is(T == class)) {
					if(auto casted = cast(T)cast(Object)Data) {
						value = casted; 
						return true;
					}
				}
			}
			//debug std.stdio.writefln("Failed get.");
			return false;

		}
		static if(is(T == class)) {
			value = cast(T)cast(Object)Data;
			if(value is null) //|| typeid(value) != typeid(T))
				return false;
		} else {
			static if(T.sizeof <= (void*).sizeof) {
				value = *(cast(T*)&Data);
			} else {
				T* ptr = cast(T*)Data;
				value = *ptr;
			}
		}
		return true;
	}

	bool opEquals(T)(T other) {
		static if(is(T == Untyped)) {
			if(other.StoredType != this.StoredType)
				return false;
			if(auto ci = cast(ClassInfo)StoredType) {
				// Turns out TypeInfo_Struct can be casted to ClassInfo...
				// But in this case their vtbl is null, so we can differ by using that.
				// For classes we can't use TypeInfo.equals as it always seems to be true.
				if(ci.vtbl is null)
					return cast(Object)Data == cast(Object)other.Data;
			}
			// Otherwise it's a struct/array/etc, and we can use TypeInfo.equals to check.
			void* a, b;
			if(StoredType.tsize <= (void*).sizeof)
				a = &Data, b = &other.Data;
			else
				a = Data, b = other.Data;
			return StoredType.equals(a, b);
		} else {
			T currVal;
			if(!this.tryGet!(T)(currVal))
				return false;
			return currVal == other;
		}
	}

	void opAssign(T)(T rhs) {
		static if(is(T == Untyped)) {
			this.Data = rhs.Data;
			this.StoredType = rhs.StoredType;
		} else {
			store(rhs);
		}
	}

	private void store(T)(T Value) {
		StoredType = typeid(T);
		static if(is(T == class)) {
			Data = cast(void*)Value;
		} else {
			// Optimization for small values.
			static if(T.sizeof <= (void*).sizeof)
				Data = *(cast(void**)&Value);
			else {
				Data = GC.malloc(T.sizeof);
				//memcpy(Data, &Value, T.sizeof);
				*(cast(T*)Data) = Value;
			}
		}
	}

	private TypeInfo StoredType;
	private void* Data;

}

version(unittest) {
	mixin(MakeException("UntypedDebugException", "This is a test exception."));
	class UntypedDebugClass {
		int a = 2;
	}
	struct UntypedDebugStruct {
		int first = 1;
		void* second = cast(void*)2;
		int third = 3;
	}
}

// TODO: Clean up below test and add it as documentation tests.

// Verify basic usage.
@name("Untyped Basic Usage")
private unittest {
	auto stored = Untyped(2);
	assert(cast(int)stored == 2);
	assert(stored == 2);
	assert(stored != 3);
	assert(stored != 2f);
	assert(stored == Untyped(2));
	assert(stored != Untyped(2f));
	// Below requires comparing Untyped instances to be fixed.
	/+assert(stored != Untyped(3));
	assert(stored == Untyped(2));+/
	// Below requires alias this working with templates?
	/+int storedVal = stored;
		assert(storedVal == 2);+/
	assertThrown!(InvalidCastException)(cast(float)stored);
	auto a = new Object();
	auto b = new Object();
	auto c = Untyped(a);
	assert(cast(Object)c == a);
	assert(cast(Object)c != b);
	assert(c == Untyped(a));
	assert(c == a);
	assert(c != b);
	Untyped d = null;
	assert(d == null);		
	assert(d != c);
	Untyped f = 3;
	assert(f == 3);
	assert(f != d && f != c);
	assert(f == Untyped(3));
	assert(f.get!int == 3);

	auto dbgcls = new UntypedDebugClass();
	dbgcls.a = 4;
	Untyped g = dbgcls;
	assert(g.get!Object is dbgcls);
	assert(g.get!UntypedDebugClass is dbgcls);
	assert(g == cast(Object)dbgcls);
	assert(g == dbgcls);
	assertThrown!(InvalidCastException)(g.get!(std.container.RedBlackTree!(int)));

	UntypedDebugStruct dbgstruct;
	Untyped h = dbgstruct;
	assert(h.get!UntypedDebugStruct == dbgstruct);
	assert(h.get!UntypedDebugStruct.third == 3);
	assertThrown!(InvalidCastException)(h.get!int);
}