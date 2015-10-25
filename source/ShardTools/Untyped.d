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
import std.variant;

mixin(MakeException("InvalidCastException", "The type stored in Untyped did not match the given type."));

/// Provides a fast and lightweight structure to store a value of an unknown type.
/// The result must be the exact same type as was passed in for structs.
/// For classes, a cast will be attempted.
/// Untyped will allocate GC memory for structs that are greater than the size of a void pointer.
/// Otherwise, Untyped will allocate only if typeid allocates or if an exception is thrown.
/// $(RED The implementation of this struct is still extremely poor; use with caution.)
struct Untyped {

	private enum maxSize = std.variant.maxSize!(creal, char[], void delegate());

	/// Creates an Untyped from the given value.
	this(T)(T value) {
		store(value);
	}

	this(this) {
		runPostblit();
	}

	~this() {
		runDestroy();
	}

	/// An operator equivalent to `get`.
	T opCast(T)() {
		return get!T;
	}
	
	/// Gets the type that's stored within this instance.
	@property TypeInfo type() {
		return _type;	
	}

	/// Gets the underlying value as the given type.
	/// This must be the exact type of the value that was passed in for structs.
	/// For classes a cast is attempted provided that both the stored and requested types are classes.
	@property T get(T)() {
		T result;
		if(!tryGet!(T)(result))
			throw new InvalidCastException("Unable to cast from " ~ to!string(_type) ~ " to " ~ to!string(typeid(T)) ~ ".");
		return result;
	}

	/// Attempts to get the given value, returning whether the attempt was successful.
	/// That is, if get would throw, this returns false; otherwise true.
	bool tryGet(T)(out T value) {
		// TODO: Do a version(NoBoundsCheck) or something here, and skip the check optionally.
		// I'd imagine the check is fairly expensive, especially with the currently slow(?) implementation of typeid comparison.
		// TODO: Consider allowing signed vs unsigned primitives. And arrays of such?
		TypeInfo ti = typeid(T);
		TypeInfo_Class classType = cast(TypeInfo_Class)_type;
		if(_type != ti) {
			if(cast(TypeInfo_Class)_type) {
				//debug std.stdio.writefln("Determined _type is class: %s.", cast(TypeInfo_Class)_type);
				// First, check if our result can be casted to that type, if it's a class.
				static if(is(T == class)) {
					if(auto casted = cast(T)cast(Object)dataPtr) {
						value = casted; 
						return true;
					}
				}
			}
			return false;

		}
		static if(is(T == class)) {
			value = cast(T)cast(Object)dataPtr;
			if(value is null) //|| typeid(value) != typeid(T))
				return false;
		} else {
			static if(T.sizeof <= maxSize) {
				memcpy(&value, dataBytes.ptr, T.sizeof);
				//value = *(cast(T*)&data);
			} else {
				T* ptr = cast(T*)dataPtr;
				value = *ptr;
			}
			runPostblit();
		}
		return true;
	}

	bool opEquals(T)(T other) {
		static if(is(T == Untyped)) {
			if(other._type != this._type)
				return false;
			if(auto ci = cast(ClassInfo)_type) {
				// Turns out TypeInfo_Struct can be casted to ClassInfo...
				// But in this case their vtbl is null, so we can differ by using that.
				// For classes we can't use TypeInfo.equals as it always seems to be true.
				if(ci.vtbl is null)
					return cast(Object)dataPtr == cast(Object)other.dataPtr;
			}
			// Otherwise it's a struct/array/etc, and we can use TypeInfo.equals to check.
			void* a, b;
			if(_type.tsize <= maxSize)
				a = dataBytes.ptr, b = other.dataBytes.ptr;
			else
				a = dataPtr, b = other.dataPtr;
			return _type.equals(a, b);
		} else {
			T currVal;
			if(!this.tryGet!(T)(currVal))
				return false;
			return currVal == other;
		}
	}

	void opAssign(T)(T rhs) {
		static if(is(T == Untyped)) {
			runDestroy();
			this.dataBytes = rhs.dataBytes;
			this._type = rhs._type;
		} else {
			store(rhs);
		}
	}

	private void store(T)(T value) {
		runDestroy();
		_type = typeid(T);
		static if(is(T == class)) {
			dataPtr = cast(void*)value;
		} else {
			// Optimization for small values.
			static if(T.sizeof <= maxSize) {
				//data = *(cast(T*)&value);
				memcpy(dataBytes.ptr, &value, T.sizeof);
				//GC.addRange(dataBytes.ptr, T.sizeof, typeid(T));
			} else {
				dataPtr = GC.malloc(T.sizeof);
				memcpy(dataPtr, &value, T.sizeof);
				//*(cast(T*)data) = value;
			}
			runPostblit();
		}
	}

	private void runPostblit() {
		if(this._type) {
			if(_type.tsize <= maxSize)
				_type.postblit(dataBytes.ptr);
			else
				_type.postblit(dataPtr);
		}
	}

	private void runDestroy() {
		if(this._type) {
			if(_type.tsize <= maxSize)
				_type.destroy(dataBytes.ptr);
			else
				_type.postblit(dataPtr);
		}
	}

	private TypeInfo _type;
	union {
		private void[maxSize] dataBytes;
		private void* dataPtr;
	}
}

version(unittest) {
	mixin(MakeException("UntypedDebugException", "This is a test exception."));
	class UntypedDebugClass {
		int a = 2;
	}
	struct UntypedDebugStruct {
		long first = 1;
		void* second = cast(void*)2;
		int third = 3;
		long fourth = 4;
	}
}

// TODO: Clean up below test and add it as documentation tests.

// Verify basic usage.
@name("Basic Usage")
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
	import std.container;
	assertThrown!(InvalidCastException)(g.get!(RedBlackTree!(int)));
	UntypedDebugStruct dbgstruct;
	Untyped h = dbgstruct;
	assert(h.get!UntypedDebugStruct == dbgstruct);
	assert(h.get!UntypedDebugStruct.third == 3);
	assert(h.get!UntypedDebugStruct.fourth == 4);
	assert(h == dbgstruct);
	assertThrown!(InvalidCastException)(h.get!int);

	alias DelegateType = void delegate();
	int tmp = 0;
	DelegateType foo = () {
		tmp++;
	};

	Untyped i = foo;
	auto dg = i.get!DelegateType();
	assert(dg == foo);
	dg = null;
	foo = null;
	GC.collect();
	dg = i.get!DelegateType();
	dg();
	assert(tmp == 1);

	int[] arr = [1, 2, 3];
	Untyped arrU = arr;
	assert(arrU.get!(int[]) == [1, 2, 3]);
	int[3] sarr = [1, 2, 3];
	Untyped sarrU = sarr;
	assert(sarrU.get!(int[3]) == [1, 2, 3]);

	auto j = RefCounted!int(4);
	assert(j.refCountedStore.refCount == 1);
	Untyped k = j;
	assert(j.refCountedStore.refCount == 2);
	Untyped l = k;
	assert(j.refCountedStore.refCount == 3);
	assert(l.get!(RefCounted!int) == 4);
	assert(k.get!(RefCounted!int) == 4);
	k = null;
	assert(j.refCountedStore.refCount == 2);
	l = null;
	assert(j.refCountedStore.refCount == 1);
	l = 2;
	assert(j.refCountedStore.refCount == 1);
}