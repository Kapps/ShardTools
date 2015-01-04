/// Provides an optionally thread-safe FIFO queue.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Queue;
private import ShardTools.SpinLock;
private import core.memory;
import core.stdc.stdlib;

/// Provides an optimized implementation of a FIFO queue, optionally including thread-safety using a SpinLock.
final class Queue(T, bool ThreadSafe = false) {

	// TODO: Can optimize further by determining if a GC.addRoot is needed for elements of type T.
	// If it's not, we can avoid setting Next to null, and we can avoid the call itself which is more important.
	// Ultimately, not really a big deal.
	// TODO: Check if SpinLock is appropriate for this.
	// TODO: Could improve performance by making this cache-aware; perhaps don't actually delete elements as they get removed.
	// Instead, free their element for future uses, and pre-allocate many.	

public:

	/// Gets the number of elements stored within this queue.
	size_t Count() {
		return _Count;
	}

	/// Adds the given item to this queue.
	void Enqueue(T Item) nothrow {
		QueueElement* Element = CreateElement(Item);
		static if(ThreadSafe)
			Lock.lock();		
		if(_Count == 0)
			Tail = Head = Element;
		else {
			Tail.Next = Element;		
			Tail = Element;
		}
		_Count++;		
		static if(ThreadSafe)
			Lock.unlock();
	}

	/// Pushes the given item to the front of the queue, causing it to be the next value returned by Dequeue.
	void PushFront(T Item) nothrow {
		QueueElement* Element = CreateElement(Item);
		static if(ThreadSafe)
			Lock.lock();
		Element.Next = Head;
		Head = Element;
		if(_Count == 0)
			Tail = Head;
		_Count++;
		static if(ThreadSafe)
			Lock.unlock();
	}

	/// Tries to dequeue an element, placing it into Value.
	/// Returns whether there were any elements to dequeue.
	bool TryDequeue(out T Value) nothrow {
		static if(ThreadSafe)
			Lock.lock();
		if(_Count == 0) {
			static if(ThreadSafe)
				Lock.unlock();
			return false;
		}
		auto Element = Head;
		if(_Count == 1)
			Tail = null;
		assert(Head, "Head node was null and Count was not 0.");
		Head = Head.Next;				
		_Count--;
		Value = Element.Data;
		static if(ThreadSafe)
			Lock.unlock();
		return true;					
	}

	/// Dequeues an element, returning DefaultValue if no elements are present.
	T Dequeue(lazy T DefaultValue = T.init) {	
		T Result;	
		if(!TryDequeue(Result))
			Result = DefaultValue();
		return Result;
	}

	/// Checks the top element on the queue, or returns DefaultValue if empty.
	T Peek(lazy T DefaultValue = T.init) {
		static if(ThreadSafe) {
			// No guarantees of nothrow due to lazy, thus must use scope exit.
			Lock.lock();
			scope(exit)
				Lock.unlock();
		}
		if(_Count == 0)
			return DefaultValue();
		return Head.Data;
	}

	~this() {
		while(_Count > 0) {
			auto Current = Tail;			
			Tail = Tail.Next;			
			GC.removeRoot(Current);
			free(Current);
		}
	}

private:
	size_t _Count;
	QueueElement* Tail; // The last item enqueued, where to append to.
	QueueElement* Head; // The first item enqueued.
	static if(ThreadSafe) {
		// TODO: Due to lock contention, we may be better off using a Mutex.
		SlimSpinLock Lock;
	}

	struct QueueElement {
		T Data;
		QueueElement* Next;
	}

	QueueElement* CreateElement(T Value) nothrow {
		QueueElement* Element = cast(QueueElement*)malloc(QueueElement.sizeof);
		assert(Element);
		Element.Data = Value;
		Element.Next = null;
		GC.addRoot(Element);
		return Element;
	}

	private void DestroyElement(QueueElement* Element) {
		GC.removeRoot(Element);
		free(Element);
	}

	unittest {
		Queue!(int, ThreadSafe) queue = new Queue!(int, ThreadSafe)();
		queue.Enqueue(2);
		queue.Enqueue(3);
		assert(queue.Count == 2);
		assert(queue.Dequeue() == 2);
		assert(queue.Count == 1);
		int tmp;
		assert(queue.TryDequeue(tmp));
		assert(queue.Count == 0);
		assert(tmp == 3);
		assert(!queue.TryDequeue(tmp));
		assert(queue.Count == 0);
		queue.Enqueue(4);
		assert(queue.Peek() == 4);
		queue.Enqueue(6);
		queue.PushFront(2);
		assert(queue.Peek() == 2);
		assert(queue.Count == 3);
		assert(queue.Dequeue() == 2);		
		assert(queue.Count == 2);
		assert(queue.Dequeue() == 4);
		assert(queue.Dequeue() == 6);
	}
}