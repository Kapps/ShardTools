/// Provides a basic reusable pool of objects with a default instance.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Pool;
private import ShardTools.List;
private import ShardTools.Stack;
private import ShardTools.ConcurrentStack;

/// A class used to provide simple pooling of objects.
/// This class is thread-safe and has a global default instance.
class Pool(T) {

public:

	// TODO: Should be able to call an initialize function on the pooled object.

	static this() {
		_Default = new Pool!(T)(0);
	}

	/// Initializes a new instance of the Pool object.
	/// Initializing a Pool only creates the data to store the number of elements required.
	/// The caller should push default values into the pool.
	/// Params:
	///		Capacity =  The maximum number of objects this Pool is capable of storing. 
	///				    Attempting to Push a value past the maximum size will simply ignore the call.
	///                 A capacity of zero will result in a Pool that can store an unlimited number of elements.
	this(size_t Capacity) {
		this.Capacity = Capacity;		
		this.Elements = new ConcurrentStack!(T)();		
	}

	/// Pushes the specified instance back into the pool.
	/// Params:
	///		Instance = The instance to push back in to the pool.
	void Push(T Instance) {		
		if(Capacity > 0 && Elements.Count < Capacity)
			Elements.Push(Instance);		
	}

	/// Pops an object from the pool, initializing it and returning the initialized instance.	
	/// Returns:
	///		An initialized instance of the object to create, or init if the pool was empty.
	T Pop() {
		return Pop(T.init);	
	}
		
	/// Pops an object from the pool, initializing it and returning the initialized instance.
	/// Params:
	/// 	DefaultValue = The default value to use if there were no elements remaining.
	/// Returns:
	///		An initialized instance of the object to create, or DefaultValue if the pool was empty.
	T Pop(lazy scope T DefaultValue) {				
		return Elements.Pop(DefaultValue);
	}

	/// Gets a default pool for this type, eagerly initialized with an unbounded number of elements.
	/// This pool is shared but thread-safe and lock-free.
	@property static Pool!(T) Default() {
		return _Default;
	}
	
private:
	static __gshared Pool!(T) _Default;
	ConcurrentStack!(T) Elements;
	size_t Capacity;	
}

