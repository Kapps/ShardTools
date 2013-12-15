/// Provides helper functions for initializing of values.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Initializers;

// TODO: Having a module for a single 3 line function is absolutely silly.
// But... nowhere else it can really go cleanly.

/// Initializes the given specified objects to the result of a call to new.
void constructNew(T...)(ref T values) {
	foreach(ref val; values)
		val = new typeof(val)();
}
