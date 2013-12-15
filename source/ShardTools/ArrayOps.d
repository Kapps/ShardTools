/// $(RED This module should be considered deprecated and not be used in new code.)
/// Provides simple operations that act upon collections with foreach, as opposed to ranges.
/// The operations that mimic std.algorithm are intended to be faster, as they allow use of array foreach, which is the fastest form of iteration.
/// This module is stable in design, but not tested enough to be stable in implementation. Many methods are completely untested.
/// There was a time when this module could have been considered useful and a performance booster, but that is no longer the case.
/// New code should use std.algorithm instead of this module.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.ArrayOps;
private import std.traits;
import std.parallelism;
import std.functional;
import std.conv;

// TODO: Speed difference may have been true before, but is it anymore?


/// Determines whether Range contains any element such that Condition for that element and Element evaluates to true.
/// Params:
/// 	Condition = The condition to evaluate, where the first parameter is the element in the collection, and the second is Element. When this evaluates as true, the function returns true.
/// 	Collection = The type of the collection to use for this Range.
/// 	T = The type of the Element to compare to.
/// 	Range = The range to perform this operation on.
/// 	Element = The element to compare each element in the range to.
bool Contains(alias Condition = "a == b", Collection, T)(Collection Range, T Element) if(is(typeof(binaryFun!Condition))) {
	foreach(ref a; Range)
		if(binaryFun!Condition(a, Element))
			return true;
	return false;
} unittest {
	int[] elements = [1, 2, 3, 4];
	assert(elements.Contains(2));
	assert(!elements.Contains(5));
	assert(elements.Contains!("a == b * b")(2));
	assert(!elements.Contains!("a * a == b")(3));
}

/// Determines the number of elements in the collection that satisfy the given action.
/// This method may accept a condition that only includes elements that evaluate to true.
/// If the condition is left blank, and the range contains a length or Count member, the result of that will be returned instead of iterating over the collection.
/// Params:
///		Condition = The condition to evaluate. When this evalutes as true, the result is incremented.
///		Collection = The type of the collection to use for Range.
///		Range = The range to perform this operation on.
size_t Count(alias Condition = "true", Collection)(Collection Range) if(is(typeof(unaryFun!Condition))) {
	static if(hasMember!(Collection, "length") && (Condition == "true")) {
		return Range.length;
	} else static if(hasMember!(Collection, "Count") && (Condition == "true")) {
		return Range.Count;
	}
	size_t Result = 0;
	foreach(ref a; Range)
		if(unaryFun!Condition(a))
			Result++;
	return Result;
} unittest {
	int[] elements = [1, 2, 3, 4];
	assert(elements.Count == 4);
	assert(elements.Count!(a => a > 2) == 2);
	assert(elements.Count!(a => a > 5) == 0);
}

/// Determines the first index within Range such that Condition is true, or -1 if Condition never evaluated to true.
/// Params:
/// 	Condition = The condition to evaluate. When this evaluates as true, the index it evaluated as true for is returned. Element is be defined as 'b'.
/// 	Collection = The type of the collection to use for this Range.
/// 	T = The type of the Element to compare to.
/// 	Range = The range to perform this operation on.
/// 	Element = The element to compare each element in the range to.
size_t IndexOf(alias Condition = "a == b", Collection, T)(Collection Range, T Element) if(is(typeof(binaryFun!Condition))) {	
	foreach(Index, ref a; Range)
		if(binaryFun!Condition(a, Element))
			return Index;
	return -1;
} unittest {
	int[] elements = [1, 2, 3, 4];
	assert(elements.IndexOf(2) == 1);
	assert(elements.IndexOf(0) == -1);
	assert(elements.IndexOf!("a * a == b")(4) == 1);	
}
 
/// Performs the specified operation on all elements in Range (with each element aliased as a).
/// Params:
///		Collection = The type of the collection to use for Range.
///		Range = The range to perform this operation on.
///		Action = The operation to perform on each element.
void ForEach(alias Action, Collection)(Collection Range) {	
	foreach(a; Range) {
		unaryFun!Action(a);	
	}
} unittest {
	int[] elements = [1, 2, 3, 4];
//	int[] other;
//	elements.ForEach!("other ~= a * a");
//	assert(other == [1, 4, 9, 16], "Expected [1, 4, 9, 16], got " ~ other.text ~ ".");	
}

/// Performs the specified operation on all elements in Range.
/// This method is less efficient than template alias ForEach.
/// Params:
/// 	ElementType = The type of each element within the range.
/// 	Collection  = The type of the range.
/// 	Range       = The range to loop over all elements for.
/// 	Callback    = A delegate to invoke upon each element in this range.
void ForEach(ElementType, Collection)(Collection Range, void delegate(ref ElementType) Callback) {
	foreach(ref a; Range)
		Callback(a);
} unittest {
	int[] elements = [1, 2, 3, 4];
	int sum = 0;
//	auto action = (c => sum += c);
//	elements.ForEach(c => sum += c);
//	assert(sum == 10);
}

version(none) {
/// Performs the specified operation on all elements in Range (with each element aliased as a).
/// Params:
///		Collection = The type of the collection to use for Range.
///		Range = The range to perform this operation on.
///		Action = The operation to perform on each element.
///		ParallelCount = The number of elements per thread to use to perform this task. A value of zero allows the taskpool to decide how many threads to use.
void ForEach(alias Action, Collection)(Collection Range, int ParallelCount) {	
	if(ParallelCount == 0)
		foreach(ref a; parallel(Range))			
			(mixin(Action));					
	else
		foreach(ref a; parallel(Range, ParallelCount))			
			(mixin(Action));					
}
}


/// Determines if all of the elements in the given collection satisfy the given predicate.
/// Params:
///		Condition = The condition to evaluate. If this expression ever returns false, the function does as well.
///		Collection = The type of the collection to use for Range.
///		Range = The range to perform this operation on.
bool All(alias Condition, Collection)(Collection Range) if(is(typeof(unaryFun!Condition))) {
	foreach(ref Element; Range)
		if(!unaryFun!Condition(Element))
			return false;
	return true;
} unittest {
	int[] elements = [1, 2, 3, 4];
	assert(elements.All!(c => c >= 1));
	assert(!elements.All!(c => c >= 2));	
}

/// Determines if any of the elements in the given collection satisfy the given predicate.
/// Params:
///		Condition = The condition to evaluate. If this expression ever returns true, the function does as well.
///		Collection = The type of the collection to use for Range.
///		Range = The range to perform this operation on.
bool Any(alias Condition, Collection)(Collection Range) if(is(typeof(unaryFun!Condition))) {
	foreach(ref Element; Range)
		if(unaryFun!Condition(Element))
			return true;
	return false;
} unittest {
	int[] elements = [1, 2, 3, 4];
	assert(elements.Any!(c => c >= 4));
	assert(!elements.Any!(c => c >= 5));	
}

// TODO: Change any code using this to use filter and remove this method.
/// Lazily iterates over all of the elements in Range that can be explicitly converted to type T.
/// Because this allows casting, ref is not allowed.
/// Params:
/// 	T = The type of the element to cast to.
/// 	Range = The underlying range to get the elements from.
/// 	CollectionType = The type of the range.
deprecated auto OfType(T, CollectionType)(CollectionType Range) {
	struct Result {		
		@property bool empty() {
			return Input.empty;
		}

		void popFront() {
			Input.popFront();
		}		

		@property auto front() {
			return cast(T)Input.front;
		}

		this(CollectionType Input) {
			this.Input = Where!(c=>cast(T)c !is null)(Input);
		}

		private typeof(Where!(c=>cast(T)c !is null)(Range)) Input;
	}
	return Result(Range);
}

// TODO: Change any code using this to use filter and remove this method.
/// Returns all of the elements in Range that evaluate to true for Condition.
/// Params:
/// 	Condition = The condition to evaluate for each element, with each element being called "a".
/// 	Collection = The type of the range.
/// 	Range = The range to perform this operation on.
deprecated auto Where(alias Condition, Collection)(Collection Range) if(is(typeof(unaryFun!Condition))) {
	struct WhereResult {
		alias Unqual!Collection R;			
		@property bool empty() {
			return Input.empty;
		}
			
		void popFront() {
			do 
				Input.popFront();
			while(!Input.empty && !unaryFun!Condition(Input.front));					
		}
			
		@property auto ref front() {
			return Input.front;
		}
			
		this(R Input) {
			this.Input = Input;
			while(!Input.empty && !unaryFun!Condition(Input.front))
				Input.popFront();
		}
			
		int opApply(int delegate(ref size_t) dg) {
			int DgResult;		
			foreach(ref a; Input) {
				if(!(mixin(Condition)))
					continue;					
				DgResult = dg(a);
				if(DgResult)
					break;
			}				
			return DgResult;
		}
			
		R Input;
	}
}
