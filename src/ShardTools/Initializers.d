module ShardTools.Initializers;

// TODO: Having a module for a single 3 line function is absolutely silly.
// But... nowhere else it can really go cleanly.

/// Initializes the given specified objects to the result of a call to new.
void constructNew(T...)(ref T values) {
	foreach(ref val; values)
		val = new typeof(val)();
}
