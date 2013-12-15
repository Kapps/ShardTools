/// Provides basic helpers for enforcing data.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Enforce;
import ShardTools.ExceptionTools;
import std.conv;
mixin(MakeException("AssertEqualFailedException"));
mixin(MakeException("AssertNotEqualFailedException"));

/// Enforces that the two values are equal to one another.
void EnforceEqual(T, T2)(in T first, in T2 second, string FileName = __FILE__, int LineNum = __LINE__) {
	if(first != second)
		throw new AssertEqualFailedException("Expected " ~ to!string(first) ~ " to equal " ~ to!string(second) ~ " in " ~ FileName ~ "(" ~ to!string(LineNum) ~ ".");
}

/// Enforces that the two values are not equal to one another.
void EnforceUnequal(T, T2)(in T first, in T2 second, string FileName = __FILE__, int LineNum = __LINE__) {
	if(first == second)
		throw new AssertNotEqualFailedException("Expected " ~ to!string(first) ~ " to equal " ~ to!string(second) ~ " in " ~ FileName ~ "(" ~ to!string(LineNum) ~ ".");
}