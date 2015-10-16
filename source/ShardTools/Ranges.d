/// Provides additional ranges to add onto those from the standard library.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Ranges;
import std.algorithm, std.range, std.array;
import ShardTools.Udas;


/// Returns a range created by first invoking `pred` on an element, then invoking it on that element, and so on.
/// This is particularly useful for accessing nodes within a hierarchy.
/// As this returns an infinite range, one would generally use `until` or `untilDefault` to filter it.
auto generate(alias pred, T)(T element) {
	return element.recurrence!((state, n) => pred(state[n-1]));
}

///
@name("Generate Tests")
unittest {
	class Foo {
		size_t val;
		Foo parent;
		this(size_t val, Foo parent) {
			this.val = val;
			this.parent = parent;
		}
	}

	auto elements = new Foo(1, new Foo(2, new Foo(3, new Foo(4, new Foo(5, null)))));
	assert(equal(elements.generate!(c=>c.parent).until!(c=>c is null).map!(c=>c.val), [1, 2, 3, 4, 5]));
}

/// Returns a range that takes the given range and prepends or appends the specified element.
auto prepend(R, T)(R range, T element) {
	return only(element).chain(range);
}

/// Ditto
auto append(R, T)(R range, T element) {
	return range.chain(only(element));
}

///
@name("Prepend / Append Tests")
unittest {
	assert(equal([2, 3, 4].prepend(1), [1, 2, 3, 4]));
	assert(equal([2, 3, 4].append(5), [2, 3, 4, 5]));
	assert(equal([2, 3, 4].prepend(1).append(5).prepend(0), [0, 1, 2, 3, 4, 5]));
}