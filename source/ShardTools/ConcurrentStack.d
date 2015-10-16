/// Provides a concurrent lock-free stack.
/// $(RED Warning): The implementation of this module is currently very poor.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.ConcurrentStack;
private import core.atomic;
import ShardTools.Udas;

/// Provides a Concurrent Stack that uses cas operations to allow operations to be performed in a lock-free fashion.
/// Note that this implementation does not attempt to work around the ABA problem, so manual memory management of stored values is not recommended.
class ConcurrentStack(T)  {

public:

	/// Initializes a new instance of the ConcurrentStack object.
	this() {
		
	}
	
	private static struct Node {
		T Value;
		Node* Next;			

		this(T Value) { 
			this.Value = Value;
		}
	}		

	/// Returns the number of elements in this stack.
	/// This value is subject to some race conditions, and as such is not completely accurate.
	@property size_t Count() const {
		return _Count;
	}

	/+version(GNU) {
		T casimp(T, V1, V2)(shared(T*) here, shared(T) ifThis, shared(T) writeThis) {
			synchronized {
				if(*here == ifThis)
					*here = writeThis;
				return *here;
			}
		}	
	} else {+/
		private alias core.atomic.cas casimp;
	//}

	/// Pushes the given value to the top of the stack.
	/// This operation is O(1), thread-safe, and lock-free.
	/// Params:
	/// 	Value = The value to push.
	void Push(T Value) {		
		Node* NewNode = new Node(Value);
		Node* OldNode;
		do {
			OldNode = Root;
			NewNode.Next = OldNode;
		} while(!casimp(cast(shared)&Root, cast(shared)OldNode, cast(shared)NewNode));
		//atomicOp!("+=", size_t, int)(_Count, 1);
		atomicOp!("+=", size_t, int)(_Count, 1);
	}

	/// Pops the given value from the top of the stack.
	/// Returns the resulting value, or DefaultValue if the stack is empty.
	/// This operation is O(1) (if DefaultValue is O(1) or the Stack has elements), thread-safe, and lock-free.
	T Pop(scope lazy T DefaultValue = T.init) {		
		T Result;
		if(!TryPop(Result))
			return DefaultValue();
		return Result;
	}

	/// Attempts to Pop a value from the top of the stack, returning whether or not there was an element to pop.
	/// This operation is O(1), thread-safe, and lock-free.
	bool TryPop(out T Value) {
		Node* OldNode;
		T Result;
		do {
			OldNode = Root;
			if(!OldNode)
				return false;
			Result = OldNode.Value;
		} while(!casimp(cast(shared)&Root, cast(shared)OldNode, cast(shared)OldNode.Next));
		Value = Result;
		atomicOp!("-=", size_t, int)(_Count, 1);
		return true;
	}

	/// Repeatedly pops values until the stack is empty, calling dg on them.
	int opApply(int delegate(T) dg) {
		int Result;
		T Value;
		while(TryPop(Value)) {
			if((Result = dg(Value)) != 0)
				break;
		}
		return Result;
	}

	@name("Basic Tests")
	unittest {
		ConcurrentStack!int Stack = new ConcurrentStack!int();
		Stack.Push(3);
		assert(Stack.Pop == 3);
	}
	
private:
	Node* Root;	
	shared size_t _Count;
}