module ShardTools.TaskRepeater;
private import core.thread;
private import ShardTools.ConcurrentStack;
private import ShardTools.LinkedList;

/// Provides information about how a task in a TaskRepeater executed.
enum RepeaterFlags {
	// TODO: This is misleading; should be Abort instead of None.
	/// The task did not do any work, and should not be executed again.
	None = 0,
	/// The task did work, and thus this iteration should not have a sleep.
	WorkDone,
	/// The task should repeat again.
	Continue
}


/// Provides a class used to repeat a task until it is cancelled.
/// These tasks should be light-weight tasks that are run in a loop.
class TaskRepeater  {

public:

	alias RepeaterFlags delegate() TaskCallback;

	/// Initializes a new instance of the TaskRepeater object.
	this() {
		Tasks = new LinkedList!(TaskCallback)();
		ToAdd = new typeof(ToAdd)();
		Thread t = new Thread(&RunLoop);
		t.isDaemon = true;
		t.start();
	}

	/// Gets a default TaskRepeater to use.
	@property static TaskRepeater Default() {
		// TODO: Will this be safe in the future? Double-checked locking with no volatile available.
		if(_Default is null) {
			synchronized(typeid(typeof(this))) {			
				if(_Default is null)
					_Default = new TaskRepeater();
			}
		}
		return _Default;
	}

	/// Adds the given task to be queued.
	/// The task returns whether any work was done (and thus the loop should not sleep before the next iteration), as well as whether the task should be executed again.
	/// Note that a task that always returns WorkDone will use 100% CPU, as there will be no delay between loops.
	void AddTask(TaskCallback Task) {
		ToAdd.Push(Task);
	}

protected:

	bool RunIteration() {
		foreach(Task; ToAdd)
			Tasks.Add(Task);
		bool Sleep = true;
		foreach(Task, Node; Tasks) {			
			RepeaterFlags Flags; 
			try {
				Flags = Task();
				if((Flags & RepeaterFlags.Continue) == 0)
					Tasks.Remove(Node);
				else if((Flags & RepeaterFlags.WorkDone) != 0)
					Sleep = false;
			} catch {
				Tasks.Remove(Node);
			}			
		}
		return Sleep;
	}

	void RunLoop() {
		while(true) {
			bool Sleep = RunIteration();
			if(Sleep)
				Thread.sleep(dur!"msecs"(1));
		}
	}
	
private:
	static __gshared TaskRepeater _Default;	
	static __gshared LinkedList!(TaskCallback) Tasks;
	static __gshared ConcurrentStack!(TaskCallback) ToAdd;	
}