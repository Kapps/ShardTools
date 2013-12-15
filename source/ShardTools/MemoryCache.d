/// Provides the base class for memory caches.
/// Multiple memory caches are likely to exist in the future, such as a local one or memcached connector.
/// At the moment though, nothing in this module is implemented.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.MemoryCache;
private import std.functional;
import std.datetime;
import ShardTools.MessagePack;
import std.array;
import std.typecons;
import ShardTools.AsyncAction;
import ShardTools.SignaledTask;
import ShardTools.Untyped;

// TODO: For LMC, consider a Least Recently Used approach instead of worrying about size vs access count.
// TODO: For LMC determine if memory is low and try to FreeUntil 20% memory available.

/// Provides access to a hash-based memory cache that keeps track of how often an object is accessed and expels infrequently accessed objects when needed.
/// The base class exposes the most basic API that all providers should implement.
/// Additional features, such as operating on sets or performing operations on stored objects may be exposed by subclasses.
/// All operations operate on a snapshot of the value, by default serialized with MessagePack.
/// This means that any changes to the value after a call to Set will not be visible after getting the value again.
/// Operations are generally asynchronous, however a synchronous API exists (which by default uses the asynchronous API).
/// In the case that an asynchronous operation fails (ex: server goes down), the action will be aborted but the value will be set to the default value passed in.
/// This means that it is always safe to assume that asynchronous operations will contain valid data, whether successful or failed.
/// Because synchronous operations return only a value, errors would be silently ignored if the default value is returned.
/// As a result, the synchronous API will throw an ActionAbortedException (the same as GetResultSynchronous on an asynchronous action) if the operation failed.
/// All operations in this class are thread-safe.
/// $(RED This class is not yet implemented.)
@disable shared class MemoryCache {

public:

	// TODO: Move below into LocalMemoryCache.
	// Things like memcached may have this set in the config and we can't change it.
	// TODO: What about using a hash + modulus to store multiple dictionaries for LMC?
	// This could allow only locking one at a time instead of everything and get better performance as a result.
	// Of course, ultimately would need to make it mostly lock-free for optimal performance probably.
	// But that's why memcached exists; LocalMemoryCache is just a nice default that should be fairly efficient but not perfect.

	/+/// Initializes a new instance of the MemoryCache object.
	this(size_t MaxSize, size_t MaxSizePerObject) {
		this._MaxSize = MaxSize;
		this._MaxSizePerObject = MaxSizePerObject;	
	}

	/// Gets the maximum size, in bytes, that this cache can store.
	@property size_t MaxSize() const {
		return	_MaxSize;
	}

	/// Gets the maximum size of a single object stored in the cache.
	/// Objects with a larger size than this will never be stored.
	@property size_t MaxSizePerObject() const {
		return _MaxSizePerObject;
	}+/

	/// Returns a global memory cache instance to use.
	/// If a specific instance is not assigned prior to being accessed, a LocalMemoryCache utilizing 1/8th of the available RAM is used.
	/// It is possible to change this value once it has already been created, but the old cache will remain.
	@property static MemoryCache Global() {
		if(_Global is null) {
			synchronized(typeid(typeof(this))) {
				throw new NotImplementedError("Global");
				//_Global = new LocalMemoryCache();
			}
		}
		return _Global;
	}

	/// Gets the value associated with the specified key, evaluating and returning DefaultValue if a value for the Key was not found.
	final T Get(T)(string Key, lazy T DefaultValue) {
		T Result;
		if(!TryGet(Key, Result))
			Result = DefaultValue();
		return Result;
	}

	/// ditto
	final AsyncAction GetAsync(T)(string Key, lazy T DefaultValue) {
		return GetOrStoreAsyncInternal(Key, false, DefaultValue());
	}

	/// Gets the object with the given key from the cache.
	/// If no object with the key is found, DefaultValue will be evaluated and stored.
	/// Either the retrieved value or newly created value is returned.
	/// The operation will be considered completed only once the new value is set if not found.
	final T GetOrStore(T)(string Key, lazy T DefaultValue) {
		// TODO: See reserving Key todo on Set.
		return GetOrStoreAsync(Key, DefaultValue()).GetResultSynchronous!T();
	}

	/// ditto
	final AsyncAction GetOrStoreAsync(T)(string Key, lazy T DefaultValue) {
		return GetOrStoreAsyncInternal(Key, true, DefaultValue);
	}

	/// Attempts to get the value associated with the given key, returning if successful.
	final bool TryGet(ValueType)(string Key, out ValueType Value) {
		ubyte[] Data = PerformGet(Key);
		if(Data.empty)
			return false;
		Value = Deserialize!(T)(Data);
		return true;
	}

	/// Removes the object with the given key from the cache.
	/// If the key is not found, no operation is performed.
	final void Remove(string Key) {
		PerformRemove(Key);
	}

	/// ditto
	final AsyncAction RemoveAsync(string Key) {
		return PerformRemoveAsync(Key);
	}

	/// Sets the given key to be associated with the specified value.
	/// If a value already exists for this key, it is replaced.
	final void Set(ValueType)(string Key, lazy ValueType Value) {
		// Just use GetResultSynchronous.
		ubyte[] Data = Serialize(Value());
		PerformSet(Key, Data);
	}

	/// ditto
	final AsyncAction SetAsync(ValueType)(string Key, lazy ValueType Value) {
		// TODO: See reserving Key todo on Set.
		ubyte[] Data = Serialize(Value());
		return PerformSetAsync(Key, Data);
	}

protected:

	// TODO: Support custom serializer / deserializer.

	/// Serializes the given value, returning an array of bytes to store.
	/// At the moment this will always use MessagePack, as templates can not be overriden.
	final ubyte[] Serialize(T)(T Value) {
		return pack(Value);
	}

	/// Deserializes the given data, returning an object of type T.
	/// If the value passed in was not of type T, an exception is thrown.
	final T Deserialize(T)(ubyte[] Data) {
		T result;
		unpack(Data, result);
		return result;
	}

	/// Performs a synchronous set request.
	/// The default implementation simply waits on PerformSetAsync.
	void PerformSet(string Key, ubyte[] Data) {
		AsyncAction Action = PerformSetAsync(Key, Data);
		Action.GetResultSynchronous!void();
	}

	/// Override to add or replace the (serialized) value associated with the given key.
	/// The action should be completed once the server processes the request.
	/// No completion data is necessary.
	abstract AsyncAction PerformSetAsync(string Key, ubyte[] Data);

	/// Performs a synchronous get request.
	/// The default implementation simply waits on PerformGetAsync.
	CacheQueryResult PerformGet(string Key) {
		AsyncAction Action = PerformGetAsync(Key);
		return Action.GetResultSynchronous!CacheQueryResult();
	}

	/// Override to return the value associated with the given key.
	/// Once the action is complete, the completion data CacheQueryResult storing information about the action.
	abstract AsyncAction PerformGetAsync(string Key);

	/// Performs a synchronous remove request.
	/// The default implementation waits on PerformRemoveAsync.
	void PerformRemove(string Key) {
		AsyncAction Action = PerformRemoveAsync(Key);
		Action.GetResultSynchronous!void();
	}

	/// Override to remove the value associated with the given key.
	/// If the key is not found, no operation should be performed.
	/// Once the action is complete, the key should be removed if it existed.
	abstract AsyncAction PerformRemoveAsync(string Key);

protected:

	/// Indicates the results of an operation on a MemoryCache.
	static struct CacheQueryResult {
		// TODO: Make const when Untyped supports it.
		/// Indicates if the value was found, as opposed to a cache miss.
		public bool Found;
		/// Assuming found is true, stores the results of the query; otherwise if found is false, the results are undefined.
		public ubyte[] Data;

		this(bool Found, ubyte[] Data) {
			this.Found = Found;
			this.Data = Data;
		}
	}
	
private:
	static __gshared MemoryCache _Global;

	AsyncAction GetOrStoreAsyncInternal(T)(string Key, bool StoreValue, lazy T DefaultValue) {
		// TODO: As soon as a set method is called, reserve Key as having it's value set.
		// Then when a request for Get or GetOrStore on that value is received,
		// we can return the action that's being used for reserving the value.
		SignaledTask Task = new SignaledTask();
		AsyncAction Action = PerformGetAsync(Key);
		// This is rather ugly thanks to nested asynchronous operations.
		// First, start off with a standard GetAsync.
		Action.NotifyOnComplete(Untyped.init, (state, action, status) => {
			if(status != CompletionType.Successful) {
				T Default = DefaultValue();
				Task.Abort(Default);
			} else {
				// Then if that succeeded, if the result was found just return it.
				CacheQueryResult Result = Action.CompletionData.get!CacheQueryResult();
				if(Result.Found)
					Task.Complete(Deserialize!T(Result.Data));
				else {
					// Otherwise if we need to store it we have to use a SetAsync to do so.
					T Value = DefaultValue();
					if(StoreValue) {
						AsyncAction SetAction = SetAsync(Key, Value);
						// Which means we have to wait to trigger the SignaledTask again.
						SetAction.NotifyOnComplete(Untyped.init, (setState, setAction, setStatus) => {
							if(setStatus != CompletionType.Successful)
								Task.Abort(Value);
							else
								Task.Complete(Value);
						});
					} else {
						// Or if we don't need to store it, we can just immediately return the default.
						Task.Complete(Value);
					}
				}
			}
		});
		return Task;
	}
}