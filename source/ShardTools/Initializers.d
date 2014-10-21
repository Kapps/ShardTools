/// Provides helper functions for initializing of values.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Initializers;
import core.stdc.stdlib;
import std.conv;
import ShardTools.Udas;

/// Initializes the given specified objects to the result of a call to new.
void constructNew(T...)(ref T values) {
	foreach(ref val; values)
		val = new typeof(val)();
}

/// Creates a new instance of the given class on the heap using malloc.
/// Note that this must be freed using either free manually, or through $(D mallocFree) which handles casting.
T mallocNew(T, ArgTypes...)(ArgTypes args) if(is(T == class)) {
	auto size = __traits(classInstanceSize, T);
	void[] mem = malloc(size)[0..size];
	return emplace!T(mem, args);
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
	assert(inst.a == 3);
	// Must be freed when done with the instance.
	mallocFree(inst);
}

/// Frees a class instance created by $(D mallocNew).
void mallocFree(T)(T instance) if(is(T == class)) {
	auto mem = cast(void*)instance;
	free(mem);
}