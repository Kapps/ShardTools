/// Provides helper functions for initializing of values.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Initializers;
import core.stdc.stdlib;
import std.conv;
import ShardTools.Udas;
import std.traits;
import core.stdc.string;
import std.range;

/// Initializes the given specified objects to the result of a call to new.
void constructNew(T...)(ref T values) {
	foreach(ref val; values)
		val = new typeof(val)();
}

/// Creates a new instance of the given class or struct on the heap using malloc.
/// Note that this must be freed using either free manually, or through $(D mallocFree) which handles casting.
/// The result is handled as a pointer for structs, or directly as the reference itself for classes.
T mallocNew(T, ArgTypes...)(ArgTypes args) if(is(T == class)) {
	auto size = __traits(classInstanceSize, T);
	void[] mem = malloc(size)[0..size];
	return emplace!T(mem, args);
}

/// Ditto
T* mallocNew(T, ArgTypes...)(ArgTypes args) if(is(T == struct)) {
	// Duplicated for purpose of allowing easier type information.
	auto size = T.sizeof;
	void[] mem = malloc(size)[0..size];
	return emplace!T(mem);
}

/// Creates an array of $(D length) elements on the heap using malloc.
/// The data in the array is not initialized. If T contains pointers, it should be zeroed out if not immediately assigned.
T[] mallocNew(T)(size_t length) if(isDynamicArray!T) {
	alias ET = ElementType!T;
	static assert(is(ET == struct), "Only structs may be stored in a malloc'd array for the moment.");
	T* mem = cast(T*)malloc(ET.sizeof * length);
	return mem[0..length];
}

/// Example
@name("Malloc Tests")
unittest {
	class Foo {
		int a;
		this(int a) {
			this.a = a;
		}
	}
	auto inst = mallocNew!Foo(3);
	scope(exit) {
		// Must be freed when done with the instance.
		mallocFree(inst);
	}
	assert(inst.a == 3);
}

/// Returns the given array created with malloc rather than the GC.
T[] mallocDup(T)(T[] arr) if(!isArray!T) {
	void* buff = malloc(T.sizeof * arr.length);
	T[] res = (cast(T*)buff)[0..arr.length];
	memcpy(res.ptr, arr.ptr, T.sizeof * arr.length);
	return res;
}

/// Example
@name("Malloc Array Tests")
unittest {
	int[] elements = [1, 2, 3, 4];
	auto duped = elements.mallocDup();
	scope(exit)
		mallocFree(duped);
	assert(duped == elements);
	assert(duped.ptr != elements.ptr);
}


/// Frees an object or array created by $(D mallocNew) or $(D mallocDup).
void mallocFree(T)(T instance) if(is(T == class)) {
	auto mem = cast(void*)instance;
	free(mem);
}

/// Ditto
void mallocFree(T)(T* instance) if(is(T == struct)) {
	free(instance);
}

/// Ditto
void mallocFree(T)(T[] inst) if(!isArray!T) {
	free(inst.ptr);
}