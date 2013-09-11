module ShardTools.RecursiveRange;
private import std.exception;
private import std.functional;
private import std.range;
private import std.stdio;
import std.algorithm;

/// Lazily converts a recursive range into an array of ranges with the index being the depth.
/// For example, a QuadTree would have a single node for depth 0, four nodes in the array for depth one, sixteen for depth two, etc.
/// This module is not yet implemented.
/// Params:
///		fun = The function to return the next range for an element in the range. Should return an empty range if this element did not contain any children.
///		Range = The type of the input ranges. Currently, all recursed ranges must be of this type.
///				If desired, fun could be made to map into the InputRange interface with that being Range to support multiple types.
version(None) {
@disable struct RecursiveRange(alias fun, Range) {

public:

	alias ElementType!(Range) T;

	/// Initializes a new instance of the RecursiveRange object.
	/// Params:
	/// 	Inputs = The elements contained at depth zero.
	this(Range[] Inputs ...) {
		this._Ranges ~= Inputs;
		this._MaxDepth = -1;
	}	
	
	/// Ditto
	this(Range Inputs) {
		this([Inputs]);
	}
	
	/// Allows foreach access over each element in this range (and all sub-ranges) and (optionally) it's depth.
	/// Elements are traversed with lowest depth first and highest depth last.
	int opApply(int delegate(ref T) dg) {
		int ret = 0;
		foreach(Range[] range; _Ranges) {
			foreach(Range subrange; range) {
				foreach(ref T element; subrange) {
					if((ret = dg(element)) != 0)
						return ret;
				}
			}
		}
		return ret;
	}
	/// Ditto
	int opApply(int delegate(size_t, ref T) dg) {
		// TODO: mixin.
		int ret = 0;
		foreach(size_t depth, Range[] range; _Ranges) {
			foreach(Range subrange; range) {
				foreach(ref T element; subrange) {
					if((ret = dg(depth, element)) != 0)
						return ret;
				}
			}
		}
		return ret;
	}
	
	/// Gets the root element used for this recursive range.
	@property T Root() {
		return _Root;
	}
	
private:
	Range[][] _Ranges;
	size_t _MaxDepth;
	T _Root;

	bool EvaluateDepth(size_t Depth) {
		if(_Ranges.length <= Depth)
			EvaluateDepth(Depth - 1);
		if(_Ranges.length == Depth) {
			_MaxDepth = _Ranges.length;
			return false;
		}
		enforce(Depth == _Ranges.length - 1);
		Range[] ToChain;
		bool Any = false;
		foreach(Range range; _Ranges[Depth-1]) {			
			foreach(T Element; range) {							
				Range Next = unaryFun!fun(Element);
				if(Next !is null && !Next.empty) {
					ToChain ~= Next;
					Any = true;
				}
			}
		}		
		if(Any)
			_Ranges ~= ToChain;
		return Any;
	}

	void EvaluateAll() {
		while(true) {
			if(!EvaluateDepth(_Ranges.length))
				break;
		}	
	}
}

/// Creates a RecursiveRange from the given input.
/// Params:
/// 	fun = The function to return the next range for an element in the range. Should return an empty range (or null) if this element did not contain any children.
/// 	Range = The type of the input range.
/// 	Input = The input to create a RecursiveRange from.
///		Element = A single element to create a RecursiveRange from.
auto Recursive(alias fun, Range)(Range Input) {
	auto res = RecursiveRange!(fun, Range)(Input);
	return res;
}

unittest {
	class SomeNode {
		this(int Depth) {
			static int NextCount = 0;
			this.Depth = Depth;
			this.Count = NextCount++;
		}
	
		SomeNode[2] Children;
		int Depth;
		int Count;
	}

	void CreateChildren(SomeNode Node, int Depth, int MaxDepth) {
		if(Depth > MaxDepth)
			return;
		Node.Children[0] = new SomeNode(Depth);
		Node.Children[1] = new SomeNode(Depth);
		CreateChildren(Node.Children[0], Depth + 1, MaxDepth);
		CreateChildren(Node.Children[1], Depth + 1, MaxDepth);
	}

	static T[] MakeChild(T)(T c) {
		return c.Children[];
	}
	
	SomeNode Root = new SomeNode(0);
	CreateChildren(Root, 1, 7);
	auto rec = Recursive!(MakeChild)([Root]);
	foreach(Depth, Element; rec) {
		writeln(Depth, ": ", Element);
	}	
}
}