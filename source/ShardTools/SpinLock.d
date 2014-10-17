/// Provides a light-weight spinlock that is not re-entrant and has no ordering.
module ShardTools.SpinLock;
private import core.thread;
private import core.atomic;

@nogc:

/// Provides a wrapper around SlimSpinLock that implements Object.Monitor.
final class SpinLock : Object.Monitor {

public:
	/// Attempts to acquire the lock, blocking until done.
	void lock() nothrow {
		.lock(InternalLock);
	}

	/// Removes any existing locks, regardless of who owns them.
	void unlock() nothrow {
		.unlock(InternalLock);
	}

	/// Attempts once to acquire a lock, returning whether it was acquired.
	bool tryLock() nothrow {		
		return .tryLock(InternalLock);
	}
	
private:
	SlimSpinLock InternalLock;
	
}

/// Provides a lock that continuously attempts to acquire a lock until it succeeds.
/// Unlike Mutex, a single unlock will make the SpinLock available, until another lock call is made.
/// This means that SpinLock's are not re-entrant. SpinLocks also do not have any guaranteed ordering.
/// The SlimSpinLock type does not need to be initialized, and instead is default initialized to available.
/// As a result, it can be used as a static variable, particularly useful for singleton initialziation.
/// SlimSpinLock provides a very basic SpinLock, and is recommended to be used when memory is an issue or default initialization is desirable.
/// In standard situations however, the SpinLock wrapper class should be used instead.
alias bool SlimSpinLock;

/// Spins until an exclusive lock for the SlimSpinLock may be obtained.
void lock(ref SlimSpinLock SpinLock) nothrow {
	bool GotLock;
	do {
		GotLock = cas(cast(shared bool*)&SpinLock, cast(shared)false, cast(shared)true);
	} while(!GotLock);
}

/// Removes any existing locks, regardless of who owns them.
void unlock(ref SlimSpinLock SpinLock) nothrow {
	SpinLock = false;	
}

/// Attempts once to acquire a lock, returning whether it was acquired.
bool tryLock(ref SlimSpinLock SpinLock) nothrow {
	return cas(cast(shared bool*)&SpinLock, false, true);		
}