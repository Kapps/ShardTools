/// Provides a means of handling and tracking problems that occur in an application.
/// Problems may have one or more steps taken to attempt to solve them.
/// Solutions can describe the approaches used to solve a problem.
/// Watchers may be used to keep track of problems that occur.
/// Problems can depend on other problems, and thus be chained.
/// This module is not yet implemented.
module ShardTools.Problem;
private import std.algorithm;
private import std.range;
private import ShardTools.ArrayOps;
private import std.exception;
private import ShardTools.SortedList;
private import std.variant;

// TODO: This module is pretty important for some other things, such as networking and databases. Try to implement it.

/+

/// Indicates the way that a single handler attempted to solve a problem.
enum SolutionStepStatus {
	/// Attempting to solve the problem failed non-transitively.
	Failed = 1,
	/// The problem was successfully solved. No further handlers need to be invoked for this problem.
	Success = 2,
	/// Attempting to solve the problem failed and the problem is one that should not be attempted to be solved by any other handler.
	TransitiveFail = 4 | Failed,
}

/// Indicates the result of attempting to solve a Problem fully.
/// The caller may test only for Failed or Success if that is the only information they care about.
enum SolutionStatus {
	/// Attempting to solve the problem failed.
	/// If no other bits are set, indicates that the Problem associated with this Solution was the unsolved one.
	Failed = 1,
	/// The Solution successfully managed to handle the Problem, and all dependencies and dependents did as well.
	Success = 2,	
	/// This Solution managed to successfully solve the Problem, but a Problem that had a dependency on this Solution did not.
	DependentFailed = 4 | Failed,
	/// The Problem did not have an actual Solution attempted because a dependent Condition failed to solve its Problem.	
	DependencyFailed = 8 | Failed
}

alias SolutionStep delegate(Problem, Variant[string]) ProblemHandlerCallback;

/// Indicates a potential problem that occurs during operation, which may or may not be recoverable without aborting the entire operation.
/// One or more ProblemHandlers may be attached to a condition to attempt to attempt recovery.
/// When the Condition is told a problem exists, a Problem is created and the handlers are invoked to attempt to handle it.
/// If no handler successfully solves the problem, the Problem is thrown.
/// A Condition may depend on zero or more other conditions, and when one of those is raised, this Condition will be raised as well.
/// Conditions have a many-to-many dependency system, where one Condition may have multiple dependents and multiple dependencies.
/// All methods in this class are thread-safe unless specified otherwise.
class Condition {

	/// Creates a new Condition.
	this() {
		this._Handlers = new SortedList!ProblemHandler();
	}

	/// Registers the given Observer for notifications about this Condition.
	/// The Condition has a reference to the Observer, but not the other way around.
	void RegisterObserver(Observer obs) {
		_Observers ~= obs;
	}

	/// Registers a global Observer that is notified when any Condition is raised.
	/// Global observers are only notified for the lowest level Condition in a chain.
	/// The Observer will always have a reference to it until manually removed.
	static void RegisterGlobalObserver(Observer obs) {
		_GlobalObservers ~= obs;
	}

	/// Indicates that this Condition is dependent on the given Condition, and thus is required to be checked and processed prior to this one.
	/// Recursive dependencies are not allowed (but due to performance concerns, are not detected), and will result in stack overflows.
	/// Params:
	/// 	Dependency = The Condition that this Condition is dependent on.
	void AddDependency(Condition Dependency) {		
		synchronized(this, Dependency) {
			assert(!Contains(_Dependencies, Dependency));
			_Dependencies ~= Dependency;
			assert(!Contains(Dependency._Dependents, this));
			Dependency._Dependents ~= this;
		}
	}

	/// Removes this Condition from being dependent on Dependency.
	bool RemoveDependency(Condition Dependency) {
		synchronized(this, Dependency) {
			size_t Index = IndexOf(_Dependencies, Dependency);
			if(Index == -1)
				return false;
			_Dependencies = array(remove(_Dependencies, Index));
			size_t ThisIndex = IndexOf(Dependency._Dependents, this);
			assert(ThisIndex != -1);
			Dependency._Dependents = array(remove(Dependency._Dependents, ThisIndex));
			return true;
		}
	}

	/// Adds a new handler that is capable of solving this problem.	
	/// Params:
	/// 	Priority = The priority of this handler. A lower value will execute first.
	/// 	Callback = The callback to invoke to attempt to handle the problem.
	ProblemHandler AddHandler(ProblemHandlerCallback Callback, int Priority = 0) {
		ProblemHandler Handler = new ProblemHandler(Priority, Callback);
		synchronized(this)
			this._Handlers.Add(Handler, Priority);
		return Handler;
	}

	/// Removes the given handler as being capable of solving this problem.
	/// Params:
	/// 	Handler = The handler to remove.
	bool RemoveHandler(ProblemHandler Handler) {
		synchronized(this)
			return _Handlers.Remove(Handler);
	}

	/// Gets a range that enumerates over all of the handlers currently capable of solving this problem.
	/// Iterating over the handlers returned here is $(B, NOT) threadsafe.
	@property auto Handlers() {
		return _Handlers;
	}	

	/// Notifies this Condition that a problem has occurred, with some information provided about the problem.
	/// Params:
	/// 	AdditionalInfo = Any additional information that may be of use in handling the problem.
	/// 	Issue = The problem that occurred.
	Solution HandleProblem(Problem Issue, Variant[string] AdditionalInfo) {
		synchronized(this) {	
			Variant[string] OriginalInfo = AdditionalInfo.dup;
			Solution Result;	
			if(IsSuspended) {		
				// TODO: Consider throwing here instead.		
				AdditionalInfo["SuspendFile"] = _SuspendFile;
				AdditionalInfo["SuspendLine"] = _SuspendLine;	
				SolutionStep Step = new SolutionStep(SolutionStepStatus.Failed, "An issue occurred while attempting to solve issues was suspended. See SuspendFile and SuspendLine additional information for what caused this suspension.");
				Result = new Solution(this, Issue, [Step], AdditionalInfo);	
			} else {
				SolutionStep[] Steps;
				foreach(ProblemHandler Handler; Handlers) {
					SolutionStep Step;
					try {
						Step = Handler.AttemptHandle(Issue, AdditionalInfo);
						enforce(Step);
					} catch (Exception Ex) {
						AdditionalInfo["FailException"] = Ex;
						Step = new SolutionStep(SolutionStepStatus.TransitiveFail, "An exception occurred while attempting to handle the problem. The status of the solution is unknown, and thus a transitive fail is forced. Details: \'" ~ Ex.msg ~ "\'.");
					}
					Steps ~= Step;
					if(Step.Status == SolutionStepStatus.Success || Step.Status == SolutionStepStatus.TransitiveFail) {
						Result._Status = Step.Status == SolutionStepStatus.Success ? SolutionStatus.Success : SolutionStatus.Failed;
						break;					
					}
				}
				if(Steps.length == 0) {
					Steps ~= new SolutionStep(SolutionStepStatus.Failed, "No handlers were attached, and thus no solution was attempted for the problem.");			
					Result = new Solution(this, Issue, Steps, AdditionalInfo);
					Result._Status = SolutionStatus.Failed;
				} else if(cast(int)Result._Status == 0)
					Result._Status = SolutionStatus.Success;
			}
			if(Result.Solved) {
				foreach(Condition Dependent; this._Dependencies) {
					Solution DepSol = Dependent.HandleProblem(Issue, OriginalInfo);
					// TODO: Update other ones if this fails...
				}
			}
			// TODO: Recursive solutions...
			// Best way is probably to only return this Solution, and then they can get at the others through Problem.
			// No they can't... Problem doesn't have Solution.
			// So, need a Caused package property on Solution.
		}
		assert(0);
	}

	private void AddFailedSolutions(Solution soln, Variant[string] AdditionalInfo) {
		auto OriginalInfo = AdditionalInfo.dup;
		foreach(Condition Dependent; soln.Owner._Dependencies) {
			SolutionStep Step = new SolutionStep(SolutionStepStatus.Failed, "A solution was not attempted because there was a direct or indirect dependency on a solution that failed.");
			//Solution NewSolution = new Solution(
		}

	}

	protected void OnProblemHandled(Problem Issue, Solution Result) {
		foreach(Observer obs; chain(this._Observers, _GlobalObservers))
			obs.NotifyProblem(Issue, Result);		
	}

	/// Notifies this Problem to temporarily stop attempting to solve issues when raised.
	/// This may be useful when you temporarily want to attempt an operation and abort it if it doesn't finish, without attempting to solve it.
	/// Use ResumeSolutions to start attempting solutions again.
	void SuspendSolutions(string File = __FILE__, size_t Line = __LINE__) {
		synchronized(this) {
			enforce(!IsSuspended, "Recursive suspends are not allowed.");
			this._SuspendFile = File;
			this._SuspendLine = Line;
			this._SuspendSolutions = true;
		}
	}

	/// Called after a call to SuspendSolutions, and notifies the Problem that it should attempt to solve any issues that occur again.
	void ResumeSolutions() {
		synchronized(this) {
			enforce(IsSuspended, "Unable to resume when not suspended.");
			_SuspendSolutions = false;
			_SuspendFile = null;
			_SuspendLine = 0;
		}
	}

	/// Indicates whether attempting to solve Problems is currently disabled.
	@property bool IsSuspended() const {
		return _SuspendSolutions;
	}

private:
	static __gshared Observer[] _GlobalObservers;
	string _Description;	
	Condition[] _Dependents;
	Condition[] _Dependencies;
	Observer[] _Observers;
	bool _SuspendSolutions;
	string _SuspendFile;
	size_t _SuspendLine;
	SortedList!ProblemHandler _Handlers;
}

/// Provides an Observer that is notified when a Condition or any (recursively) dependent Condition is raised.
/// Observers are only notified after the Problem is actually handled, and as such have access to the Solution.
/// By default, a callback is invoked when the Observer is notified of a Problem, but NotifyProblem may be overridden instead.
/// All methods in this class are thread-safe.
class Observer {

	alias void delegate(Problem, Solution) ObserverCallback;
	alias bool delegate(Problem, Solution) ObserverFilterCallback;	

	/// Creates an Observer that gets invoked on the given callback, optionally with one or more Filters.
	this(ObserverCallback Callback, ObserverFilterCallback[] Filters ...) {
		this.Callback = Callback;
		this.Filters = Filters;
	}

	/// Notifies this Observer that a Problem occurred, and the given Solution was created to handle it.
	void NotifyProblem(Problem Problem, Solution Solution) {
		synchronized(this) {
			foreach(ObserverFilterCallback Filter; Filters) {
				if(!Filter(Problem, Solution))
					return;
			}
			Callback(Problem, Solution);
		}
	}

	/// Adds or removes the given Filter for this Observer.
	/// When any Filter that a Observer has evaluates to false, this Observer will not be notified about a Problem / Solution.
	void AddFilter(ObserverFilterCallback Filter) {
		synchronized(this)
			Filters ~= Filter;
	}

	/// Ditto
	bool RemoveFilter(ObserverFilterCallback Filter) {
		synchronized(this) {
			size_t Index = IndexOf(Filters);
			if(Index == -1)
				return false;
			Filters = array(remove(Filters, Index));
		}
	}

	/// A helper method to add a new filter to notify only on solutions that have the given status set.
	/// The status is checked in a bit-wise fashion, so both Failed and Success are allowed as well.
	void FilterStatus(SolutionStepStatus Status) {
		Filters ~= delegate(Problem, Solution) { return (Solution.Status & Status) != 0; };
	}

protected:
	ObserverFilterCallback[] Filters;
	ObserverCallback Callback;
}

/// Indicates a problem that occurs during operation, which may or may not be recoverable without aborting the entire operation.
/// When a problem occurs, this problem should have the Notify method called, ideally with any details about the problem.
/// Then, all handlers available for this problem attempt to solve the problem.
/// If the problem is solved, any problems that depend on this one are attempted to be solved afterwards.
/// If a Problem is not handled, it may be thrown.
class Problem : Exception {

public:
	/// Initializes a new instance of the Problem object.
	/// Params:
	/// 	Description = A basic description of this problem.
	/// 	Previous = The original Problem that, due to a dependency, caused this Problem to occur. Can be null if this is the base problem.
	/// 	Owner = The Condition that this Problem was created for.
	this(Condition Owner, string Description, Problem Previous = null, string File = __FILE__, size_t Line = __LINE__) {		
		super(Description, File, Line, Previous);		
		this._Owner = Owner;
	}

	/// Gets the Condition that this Problem was created for.
	@property Condition Owner() {
		return _Owner;
	}
	
	override string toString() const {
		// Below stolen from Throwable.toString, but needs to be adapted for Problems.
		char[] buf;		
		char[20] tmp = void;
		if (file)
			buf ~= this.classinfo.name ~ "@" ~ file ~ "(" ~ tmp.intToString(line) ~ ")";		
		else
			buf ~= this.classinfo.name;        


	}

private:
	Condition _Owner;
}

/// Indicates a single handler that can attempt to solve a problem.
class ProblemHandler {

	this(int Priority, ProblemHandlerCallback Callback) {
		this._Priority = Priority;
		this._Callback = Callback;
	}

	/// Gets the priority of this handler. A lower value will execute first.
	@property int Priority() const {
		return _Priority;
	}

	/// Attempts to handle this Problem, returning the solution found.
	/// Params:
	/// 	Issue = The problem to handle.
	/// 	AdditionalInfo = Any additional information that may be useful in handling the Problem.
	SolutionStep AttemptHandle(Problem Issue, ref Variant[string] AdditionalInfo) {
		return _Callback(Issue, AdditionalInfo);
	}

private:
	int _Priority;
	ProblemHandlerCallback _Callback;
}

/// Indicates an attempted solution to a problem.
immutable class SolutionStep {	
	/// Creates a new Solution with the given data.
	/// Params:
	/// 	Status = Gets the status of this problem.
	/// 	Message = A human readable message to display to the user about the attempt at solving this problem (if desired).	
	this(SolutionStepStatus Status, string Message) {		
		this._Message = Message;
		this._Status = Status;		
	}	

	/// Gets a human readable message to display to the user about the attempt at solving this problem (if desired).
	@property string Message() const {
		return _Message;
	}
	/// Indicates whether this step managed to solve the problem.
	@property bool WasSuccessful() const {
		return Status == SolutionStepStatus.Success;
	}

	/// Gets the status of this problem.
	@property SolutionStepStatus Status() const {
		return _Status;
	}

private:
	string _Message;	
	SolutionStepStatus _Status;
}

/// Indicates the steps taken to handle the problem, and the result.
/// There is always at least one step, as an implicit step indicating the result is added if no handlers solved the problem.
class Solution {

	/// Creates a new Solution from the given steps.
	/// Params:
	///		Issue = The Problem that caused this Solution to be generated.
	/// 	Steps = Gets all of the steps that were used to solve this problem.
	/// 	AdditionalInfo = Gets any additional information about the problem or steps taken to solve it.
	/// 	Owner = Gets the Condition that created this Solution.
	this(Condition Owner, Problem Issue, SolutionStep[] Steps, Variant[string] AdditionalInfo) {
		this._Problem = Issue;
		this._Steps = Steps;
		this._AdditionalInfo = AdditionalInfo;
		this._Owner = Owner;
	}

	/// Gets all of the steps that were used to solve this problem.
	/// This range always has at least one element.
	@property auto Steps() const {
		return _Steps;
	}

	/// Gets the final step used for handling this solution. This is never null.
	@property SolutionStep FinalStep() const {
		return Steps[$-1];
	}

	/// Gets any additional information about the problem or steps taken to solve it.
	@property const(Variant[string]) AdditionalInfo() const {
		return _AdditionalInfo;
	}

	/// Gets the Problem that this Solution attempted to fix.
	@property Problem Issue() {
		// TODO: This needs to be const, but Throwable / TraceInfo methods are not.
		return _Problem;
	}	

	/// Gets the way that this Solution was handled.
	@property SolutionStatus Status() const {
		assert(cast(int)_Status != 0);
		return _Status;
	}

	/// Gets the Condition that created this Solution.
	@property Condition Owner() {
		return _Owner;
	}

	/// Returns a container that may be enumerated over using foreach that indicates the Solutions to Problems that have a direct Dependency on the Condition owning this Solution.
	@property auto Causes() {		
		return _Causes;
	}

	/// Indicates whether this Problem was successfully solved (in other words, has the Success flag set).
	@property bool Solved() const {
		return (Status & SolutionStatus.Success) != 0;
	}

	/// If this Solution does not have the Success flag set, Issue is thrown.
	void EnforceSolved() const {
		if(!Solved)
			throw Issue;
	}

private:
	Problem _Problem;
	SolutionStep[] _Steps;
	Variant[string] _AdditionalInfo;
	Condition _Owner;
	package SolutionStatus _Status; // Relies on Condition assigning this.
	package Solution[] _Causes; // Relies on Condition assigning this.
}

/// Indicates if thrown is a Problem that was raised by Condition not being solved.
void IsCausedBy(Throwable thrown, Condition cond) {
	Problem p = cast(Problem)thrown;
	if(p is null)
		return null;
	return p.Owner == cond;
}
+/