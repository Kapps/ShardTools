module ShardTools.LinkedList;
private import std.functional;

// TODO: Change everything here to malloc and free for nodes.
// Even though the user can access them, they can still be cleared when removed.
// TODO: Make the node a struct not a class.
// TODO: Add the thread-safe template parameter.

/// A basic doubly linked list. Any time an item is added to the list, the node is returned.
/// The node can then be removed directly for O(1) removals given an instance.
/// Params:
/// 	T = The type of the elements within the list.
final class LinkedList(T) {

public:
	/// Initializes a new instance of the LinkedList object.
	this() {
			
	}

	/// Gets the first node within the list.
	/// This operation is O(1).
	@property LinkedListNode Head() {
		return _Head;
	}

	/// Gets the last node within the list.
	/// This operation is O(1).
	@property LinkedListNode Tail() {
		return _Tail;
	}

	/// Gets the number of elements this collection contains.
	/// This operation is O(1).
	@property size_t Count() const {
		return _Count;
	}

	int opApply(int delegate(ref T, LinkedListNode Node) dg) {
		int Result = 0;
		for(LinkedListNode Node = _Head; Node !is null; Node = Node.Next) {
			if((Result = dg(Node._Value, Node)) != 0)
				break;
		}
		return Result;
	}

	int opApply(int delegate(ref T) dg) {
		int Result = 0;
		for(LinkedListNode Node = _Head; Node !is null; Node = Node.Next) {
			if((Result = dg(Node._Value)) != 0)
				break;
		}
		return Result;
	}

	void opOpAssign(string op)(T Value) if(op == "-" || op == "~") {
		static if(op == "-")
			Remove(Value);
		else static if(op == "~")
			Add(Value);
		else static assert(0);
	}

	/// Appends the given value to the back of the list.
	/// This operation is O(1).
	LinkedListNode Add(T Value) {
		LinkedListNode Node = new LinkedListNode();
		Node._Previous = _Tail;
		if(_Tail)
			_Tail._Next = Node;		
		_Tail = Node;
		if(!_Head)
			_Head = Node;
		Node._Value = Value;
		_Count++;
		return Node;
	}

	/// Appends the given value to the front of the list.
	/// This operation is O(1).	
	LinkedListNode AddFront(T Value) {
		LinkedListNode Node = new LinkedListNode();
		Node._Previous = null;
		if(_Head)
			_Head._Previous = Node;
		_Head = Node;
		if(!_Tail)
			_Tail = Node;
		Node._Value = Value;
		_Count++;
		return Node;
	}	

	/// Removes all nodes from the list.
	/// This operation is O(1) as nodes do not get detached.
	void Clear() {
		_Head = _Tail = null;
		_Count = 0;
	}

	/// Removes the given element from the list.
	/// This operation is O(N).
	bool Remove(T Value) {
		LinkedListNode Node = GetNode(Value);
		if(Node is null)
			return false;
		Remove(Node);
		return true;
	}

	/+
	/// Removes the head node and returns the value it stores, or DefaultValue if no elements exist.
	T PopHead(lazy T Default = T.init) {
		if(_Head) {
			_Head = _Head._Next;
			if(_Head)
				_Head._Previous = null;
		}
	}+/

	/// Removes the given node from the list.
	/// The node itself is not altered, thus this is safe to call from a loop.
	/// This operation is O(1).
	void Remove(LinkedListNode Node) {
		assert(GetNode(Node._Value) !is null, "Node was not part of this linked list.");
		if(Node._Previous)
			Node._Previous._Next = Node._Next;
		if(Node._Next)
			Node._Next._Previous = Node._Previous;
		if(Node is _Head)
			_Head = _Head._Next;
		if(Node is _Tail)
			_Tail = _Tail._Previous;
		_Count--;
	}

	/// Gets the node that contains the given value, or null if no node contained the given value.
	/// This operation is O(N).
	LinkedListNode GetNode(T Value) {
		for(LinkedListNode Node = this._Head; Node !is null; Node = Node.Next) {
			if(Node._Value == Value)
				return Node;
		}		
		return null;
	}

	// TODO: Make this a struct!
	static class LinkedListNode {
		/// Gets the node prior to this node.
		@property LinkedListNode Previous() {
			return _Previous;
		}		

		/// Gets the node following this node.
		@property LinkedListNode Next() {
			return _Next;
		}

		/// Gets or sets the object contained by this node.
		@property T Value() {
			return _Value;
		}

		/// Ditto
		@property void Value(T Val) {
			_Value = Val;
		}

		package T _Value;
		package LinkedListNode _Previous;
		package LinkedListNode _Next;
	}

	unittest {
		LinkedList!int List = new LinkedList!int();
		assert(List.Count == 0);
		List.Add(2);
		assert(List.Count == 1);
		assert(List.Head == List.Tail);
		assert(List.Head.Value == 2);
		assert(List.GetNode(2) == List.Head);
		assert(List.Remove(2));
		assert(List.Count == 0);
		List.Add(3);
		List.Add(4);
		assert(List.Count == 2);
		assert(List.Head != List.Tail);
		assert(List.Head.Value == 3);
		assert(List.Tail.Value == 4);
	}
	
private:
	LinkedListNode _Head;
	LinkedListNode _Tail;
	size_t _Count;
}