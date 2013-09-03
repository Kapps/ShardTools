/// Provides basic, global, logging capabilities.
/// While this module is not deprecated, it is likely to be deprecated once Phobos gets logging.
/// The interface for this module is rather poor and not flexible.
module ShardTools.Logger;

private import std.conv;
private import ShardTools.FileLogger;
private import std.datetime;
public import ShardTools.Event;


/// EventArgs used to provide information about a message being logged using a Logger.
class MessageLoggedEventArgs {
	
public:	
	/// Initializes a new instance of the MessageLoggedEventArgs object.
	/// Params:
	///		LogName = The name of the log being written to.
	///		Message = The message being appended to the log file.
	this(in char[] LogName, in char[] Message) {	
		this._LogName = LogName.idup;
		this._Message = Message.idup;
	}
	
	/// Returns the name of the log being written to.
	string LogName() {
		return _LogName;	
	}
	
	/// Returns the message being logged.
	string Message() {
		return _Message;	
	}
		
private:
	string _LogName;
	string _Message;
}

/// An abstract class used to write to a log.
abstract class Logger {
public:
	/// An event raised when a message is logged.
	Event!(void, Logger, MessageLoggedEventArgs) MessageLogged;
	
	/// Initializes a new instance of the Logger object.
	this() {
		MessageLogged = new typeof(MessageLogged)();
		SyncLock = new Object();
	}
	
	/// Appends the specified message to the log file. This operation is thread-safe.
	/// Params:
	///		LogName = The name of the log being written to.
	///		Message = The message being appended to the log file.
	final void LogMessage(in char[] LogName, in char[] Message) {						
		synchronized(SyncLock) {
			SysTime CurrentTime = Clock.currTime();						
			//PerformLog(LogName, "[" ~ to!const char[](date.day) ~ "/" ~ to!const char[](date.month) ~ to!const char[](date.year) ~ " - " ~ to!const char[](date.hour) ~ ":" ~ to!const char[](date.minute)
				//	   ~ ":" ~ to!const char[](date.second) ~ ":" ~ to!const char[](date.ms) ~ "] " ~ Message);
			PerformLog(LogName, "[" ~ CurrentTime.toSimpleString()  ~"]: " ~ Message);			
			MessageLogged.Execute(this, new MessageLoggedEventArgs(LogName, Message));			
		}
	}
	
	/// Returns the default logger to use, or null if none is set.
	@property static Logger Default() {
		if(_Default is null)
			_Default = new FileLogger();
		return _Default;
	}
	
	/// Sets the specified logger to be the default logger.
	/// Params: logger = The logger to set as being default.
	static nothrow void SetDefault(Logger logger) {
		_Default = logger;	
	}
	
protected:
	/// Performs the actual writing of the message to the log.
	/// Params:
	///		LogName = The name of the log being written to.
	///		Message = The message being appended to the log file.
	abstract void PerformLog(in char[] LogName, in char[] Message);

private:	
	static __gshared Logger _Default;		
	Object SyncLock;
}

/// Appends the specified message to the log file. This operation is thread-safe.
/// Params:
///		LogName = The name of the log being written to.
///		Message = The message being appended to the log file.
void Log(in char[] LogName, in char[] Message) { Logger.Default.LogMessage(LogName, Message); }

void LogIf(bool Condition, lazy string LogName, lazy string Message) {
	if(Condition)
		Log(LogName, Message);
}