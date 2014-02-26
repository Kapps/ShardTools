/// A specialized concurrent append-only list that allows only sequential access.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.CaoList;
import core.atomic;
import ShardTools.Udas;

/// A specialized concurrent append-only list that allows only sequential access.
/// Once an item has been added to the list, it may never be removed.
/// Elements are accessed in opposite order that they were added (LIFO).
/// All operations on this list are thread-safe, O(1), and lock-free.
/// This list does not need to be initialized, making it particularly useful for storing
/// global data, such as registered providers, without having to worry about locking / initialization.
struct CaoList(T) {
	
	// TODO: Can we give this a range interface?

	/// Returns the number of elements contained within the list.
	@property size_t length() const {
		return _length;
	}

	/// Pushes the given value to the front of the list.
	void push(T)(T value) {
		Node* node = new Node(value);
		Node* old;
		do {
			old = head;
			node.next = old;
		} while(!cas(cast(shared)&head, cast(shared)old, cast(shared)node));
		atomicOp!("+=", size_t, int)(_length, 1);
	}

	int opApply(int delegate(ref T) dg) {
		for(Node* curr = head; curr !is null; curr = curr.next) {
			int val = dg(curr.value);
			if(val != 0)
				return val;
		}
		return 0;
	}

	private Node* head;
	private shared size_t _length;

	private static struct Node {
		Node* next;
		T value;

		this(T value) {
			this.value = value;
			this.next = next;
		}
	}
}

@name("CaoList Basic Usage")
unittest {
	CaoList!(int) vals;
	vals.push(3);
	assert(vals.length == 1);
	int sum;
	foreach(val; vals)
		sum += val;
	assert(sum == 3);
}
