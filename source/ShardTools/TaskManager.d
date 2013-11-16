module ShardTools.TaskManager;
private import std.exception;
import ShardTools.Untyped;
private import std.container;
private import core.sync.condition;
private import core.memory;
private import std.conv;
private import ShardTools.SignaledTask;
public import ShardTools.AsyncTask;
private import core.atomic;
private import ShardTools.Stack;
private import ShardTools.Queue;
public import ShardTools.AsyncAction;
private import ShardTools.SpinLock;
private import core.thread;
private import ShardTools.ConcurrentStack;
private import std.algorithm;
import std.c.stdlib;
import ShardTools.ExceptionTools;
private import std.parallelism;

mixin(MakeException("PoolDestroyedException", "Unable to place new tasks into a TaskManager that is being destroyed."));

// TODO: Consider renaming to TaskQueue; seems nicer.
// Also, TaskManager could be interpreted as something similar to Windows' Task Manager.
// TODO: Actually implement this.

/// Provides a collection of threads which can execute tasks (AsyncActions) across multiple threads.
/// The number of threads used can be dynamically altered, and threads will be created or destroyed automatically as required.
/// Unlike the TaskPool, the TaskManager has the capability to prioritize certain tasks, and the capability to pause or resume tasks using Fibers.
/// $(RED This class is not yet implemented.)
@disable final class TaskManager  {
	
public:
	/// Initializes a new instance of the TaskManager object.
	this(size_t MinThreads = min(2, std.parallelism.totalCPUs), size_t MaxThreads = totalCPUs * 2) {	
		this._MinThreads = MinThreads;
		this._MaxThreads = MaxThreads;
		//this._AwaitsPerThread = 16;
	}
	
	/// Returns a global TaskManager instance to use.
	@property static TaskManager Global() {
		static __gshared SlimSpinLock initLock;
		if(_Default is null) {
			initLock.lock();
			scope(exit)
				initLock.unlock();
			if(_Default is null)
				_Default = new TaskManager();
		}
		return _Default;
	}
	
	
	/// Gets or sets the minimum or maximum number of threads to use for handling tasks.
	/// When the minimum or maximum number of threads is raised, new threads will be created immediately.
	/// When the minimum or maximum number of threads is reduced, the threads will be destroyed as their task is complete.
	/// When the current number of active threads is less than the maximum, a new thread will be created upon being blocked waiting for a thread.
	@property size_t MinThreads() const {
		return _MinThreads;
	}
	
	/// Ditto
	@property void MinThreads(size_t Value) {
		_MinThreads = Value;
	}
	
	/// Ditto
	@property size_t MaxThreads() {
		return _MaxThreads;
	}
	
	/// Ditto
	@property void MaxThreads(size_t Value) {
		_MaxThreads = Value;
	}	
	
	/*/// Gets or sets the maximum number of tasks that can be waiting for completion on a single thread due to an await operation.
	/// Setting this number too high can result in too many operations active at once, bogging down the target device.
	/// For example, if a task awaits for an asynchronous database call, having 100 doing those would bog down the database server.	
	/// Instead, the number of tasks awaiting is limited. The default value is 16 tasks per thread. A value of 0 is infinite.
	@property size_t TasksPerThread() const {
		return _AwaitsPerThread;
	}
	
	/// Ditto
	@property void TasksPerThread(size_t Value) {
		_AwaitsPerThread = Value;
	}*/
		
	/// Pushes the given task to the back of the task queue.
	/// The task will be executed by any thread when available.
	/// Optionally includes a stack size, which will be set to a default of PageSize * 4 if zero. Not using zero as the stack size results in larger overhead.
	/// Returns an AsyncAction that will complete when the task is complete.	
	AsyncAction Push(ReturnType, ArgType)(AsyncTask!(ReturnType, ArgType) Task, size_t StackSize = 0) {
		TaskInfo* ti = CreateTaskInfo(Task, StackSize, new SignaledTask());		
		PushTask(ti);
	}
	
	/// Has the same effect as Push, but will not create an AsyncAction to track the task.
	/// This results in lower overhead.	
	void PushUntracked(ReturnType, ArgType)(AsyncTask!(ReturnType, ArgType) Task, size_t StackSize = 0) {
		TaskInfo* ti = CreateTaskInfo(Task, StackSize, null);
		PushTask(ti);
	}
	
	private TaskInfo* CreateTaskInfo(T)(T Task, size_t StackSize, SignaledTask ResultAction) {		
		// While it would be nice to malloc the TaskInfo instance, we can't because we use a delegate and thus the GC would not know about it.
		TaskInfo* ti = new TaskInfo();
		ti.ExecuteDelegate = &Task.StartSynchronous;
		ti.StackSize = StackSize;
		ti.ResultAction = ResultAction;		
		return ti;
	}
	
	private void PushTask(TaskInfo* ti) {
		QueuedTasks.Enqueue(ti);
		TaskThread Worker;
	}
	
	/// Disallows adding any further tasks, and destroys any threads that are finished executing tasks.
	/// All remaining tasks will be executed.
	AsyncAction Finish() {
		this.FinishAction = new SignaledTask();
		return FinishAction;
	}
	
private:	
	
	static __gshared TaskManager _Default;
	
	size_t _MinThreads;
	size_t _MaxThreads;
	//size_t _AwaitsPerThread;
	RedBlackTree!TaskThread SleepingThreads;
	RedBlackTree!TaskThread ActiveThreads;
	ConcurrentStack!TaskThread AvailableThreads;
	ConcurrentStack!Fiber AvailableFibers; // Pool fibers when possible.
	TaskQueue QueuedTasks;
	SignaledTask FinishAction;
	
	static __gshared TaskManager Default;
	
	alias Queue!(TaskInfo*, true) TaskQueue;
	
	void DestroyThread(TaskThread Thread) {
		synchronized(ActiveThreads) {
			ActiveThreads.removeKey(Thread);
			if(ActiveThreads.length == 0) {
				assert(FinishAction);
				FinishAction.SignalComplete(Untyped.init);
			}
		}
	}
	
	void SpawnThread() {
		TaskThread th = new TaskThread(this);
		synchronized(ActiveThreads) {
			ActiveThreads.insert(th);			
		}
		
		// Thread not available when spawned.key
		th.start();
	}
	
	class TaskFiber : Fiber {
		bool WasTaskYielded;
		Untyped CurrentResult;
		AsyncAction AwaitedAction;
		ResumeStyle ResumeType;
		
		this(void delegate() Callback, size_t sz = 4096 * 4) {
			super(Callback);
		}		
	}
	
	// We need to use our own Thread class, so we can store things like the task queue.
	class TaskThread : Thread {
		size_t ThreadID;
		TaskManager Parent;
		TaskQueue TasksToResume;		
		Mutex WaitLock;		
		size_t TasksYielded;
		ThreadMessage CurrentMessage = ThreadMessage.ProcessTasks;
		
		enum ThreadMessage {
			Unknown = 0,
			Terminate = 1,
			ProcessTasks = 2
		}
		
		static shared size_t NextThreadID;
		
		public this(TaskManager Parent) {
			this.ThreadID = atomicOp!("+=", size_t, int)(NextThreadID, 1);
			this.Parent = Parent;
			this.TasksToResume = new typeof(TasksToResume)();			
			this.WaitLock = new Mutex();
			super(&RunLoop);
		}
		
		void RunLoop() {
			while(Parent.FinishAction is null) {												
				ThreadMessage Message = this.CurrentMessage;
				if(!ProcessMessage(Message))
					break;
				WaitLock.lock();
			}
			
			DestroyThread(this);
		}		
		
		bool ProcessMessage(ThreadMessage Message) {
			final switch(Message) {
				case ThreadMessage.Terminate:
					return false;
				case ThreadMessage.ProcessTasks:
					TaskInfo* ti;
					while(TasksToResume.TryDequeue(ti) || Parent.QueuedTasks.TryDequeue(ti)) {
						ti.ExecuteDelegate();
					}
					return true;
				case ThreadMessage.Unknown:
					assert(0);
			}
		}
		
		void RunTask(TaskInfo* Task) {
			// At the moment, can't pool fibers that aren't the default stack size.
			bool CanPool = Task.StackSize == 0;			
			Fiber CallFiber;			
			if(!CanPool || !AvailableFibers.TryPop(CallFiber)) {
				enum size_t PageSize = 4096; // TODO: Implement this! core.thread has it, but private unfortunately.
				CallFiber = new Fiber(Task.ExecuteDelegate, Task.StackSize == 0 ? PageSize * 4 : Task.StackSize);
			}
			Throwable thrown = cast(Throwable)CallFiber.call(true);
			if(thrown && Task.ResultAction)
				Task.ResultAction.Abort(Untyped(thrown));
		}
	}
	
	struct TaskInfo {
		void delegate() ExecuteDelegate;
		size_t StackSize;
		SignaledTask ResultAction;		
	}
}

enum ResumeStyle {
	AnyThread = 0,
	CurrentThread = 1
}

/// Yields until the given action is complete, at which point execution will resume from the current location.
/// A ResumeStyle of AnyThread will allow any thread to continue execution, whereas CurrentThread will only allow this thread.
/// Optionally, a StackSize may be passed in, otherwise a default value will be used. There may be more overhead when passing in a StackSize manually, as Fiber pooling is disabled.
/// This method is only valid when called from a TaskManager thread.
/// Note that while the task is being run in a Fiber, this method MUST be used instead of Fiber.yield. Calling yield will not resume the task.
/// BUGS:
///		This method may NOT be called from within a lock. If attempted, deadlocks are very likely.
T await(T = Untyped)(AsyncAction Action, ResumeStyle ResumeType, size_t StackSize = 0) {
	// TODO: Special case the action being done already.
	auto ManThread = cast(TaskManager.TaskThread)Thread.getThis();
	auto ManFiber = cast(TaskManager.TaskFiber)Fiber.getThis();
	ManFiber.WasTaskYielded = true;
	ManFiber.AwaitedAction = Action;
	ManFiber.ResumeType = ResumeType;
	Fiber.yield(); // Aka, resume from RunTask.
	static if(!is(T == void))
		return cast(T)ManFiber.CurrentResult;
}

// TODO: The below could potentially be fixed if we're able to set a Fiber in a per-thread static constructor...

/* /// Synchronously executes any task that is waiting to be executed, blocking until the task is done or it yields.
 /// Since a non-TaskManager thread may not yield for a specific action, this is the recommended approach to sleeping or waiting.
 /// Note that this is not recommended when doing a time-sensitive operation, as the caller has no guarantees how long the task will take.
 void YieldAny() {
 throw new NotImplementedError();
 } */

// getThis: Is AwaitOn possible? We can't really yield, because we'd need to yield to the TaskManager.
// And we can't do that...
// Unless we fake a fiber. Just save call stack, and instead of yield make our own method.
/+ /// Yields until the given action is complete, at which point a thread in the given TaskManager will resume execution.
 void AwaitOn(AsyncAction Action, TaskManager Manager) {
 // TODO: Could probably manually implement something similar to Fiber here, to change call stack to one from this.
 // But then, the thread would be acting as a TaskThread during this time.
 }+/