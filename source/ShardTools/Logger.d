/// Provides basic, global, logging capabilities without requiring allocations.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Logger;

private import std.conv;
private import std.datetime;
import std.format;
import ShardTools.SpinLock;
import ShardTools.ExceptionTools;

unittest {
	class TestLogger : Logger {
		int started, ended;
		size_t totalLength;
		@nogc void beginMessage(ref LogHeader header) { started++; }
		@nogc void logPart(ref LogHeader header, in char[] part) { totalLength += part.length; }
		@nogc void endMessage(ref LogHeader header) { ended++; }
	}
	auto tl = new TestLogger();
	registerLogger(tl);
	log("Test message.");
	assert(tl.started == tl.ended);
	assert(tl.ended == 1);
	assert(tl.totalLength == "Test Message.".length);
	tl.totalLength = 0;
	logf("This is test message #%s.", tl.ended + 1);
	assert(tl.totalLength == "This is test message #2.".length);
}

@nogc:

/// Provides predefined levels to log messages at.
enum LogLevel {
	/// Messages useful only to the application developer for debugging problems.
	debug_ = 0,
	/// Trace messages, somewhat higher level than debug but still generally not for users.
	trace = 16,
	/// Info messages, usually the lowest level messages the users sees.
	info = 32,
	/// Important messages that indicate a potential problem.
	warn = 64,
	/// Very important messages that indicate a problem has occurred.
	error = 128
}

/// Provides information about a message to be logged.
struct LogHeader {
	const LogLevel level;
	const SysTime time;
	const string file;
	const int line;
	const string func;

	///
	@nogc this(LogLevel level, SysTime time, string file, int line, string func) {
		this.level = level;
		this.time = time;
		this.file = file;
		this.line = line;
		this.func = func;
	}
}

/// Provides the base interface for a GC-free Logger.
interface Logger {
	/// Called to indicate that a new message is being logged.
	/// It is guaranteed that an endMessage occurs between these calls.
	@nogc void beginMessage(ref LogHeader header);
	/// Called to log some data for a message.
	@nogc void logPart(ref LogHeader header, in char[] part);
	/// Called to finish the logging of a message.
	@nogc void endMessage(ref LogHeader header);
}

/// Registers the given logger to receive messages.
/// Removing a logger that is registered is not yet supported.
void registerLogger(Logger logger) {
	_globalLock.lock();
	scope(exit)
		_globalLock.unlock();
	if(_numLoggers >= MAX_LOGGERS) {
		static ex = cast(immutable)(new NotSupportedException("Having more than " ~ MAX_LOGGERS.text ~ " loggers is currently not supported."));
		throw ex;
	}
	_loggers[_numLoggers] = logger;
	_numLoggers++;
}

/// Logs a message with the last letter corresponding to the log level (for example, d for debug).
/// To format values passed in, use the overloads that end with an 'f'.
/// The default 'log' method does not support formatting and logs an info message.
void log(LogLevel level = LogLevel.info)(string msg, string file = __FILE__, int line = __LINE__, string func = __FUNCTION__) {
	foreach(logger; _loggers[0 .. _numLoggers]) {
		auto header = LogHeader(level, currTimeNoGC(), file, line, func);
		logger.beginMessage(header);
		logger.logPart(header, msg);
		logger.endMessage(header);
	}
}

/// Ditto
void logf(LogLevel level = LogLevel.info, string file = __FILE__, int line = __LINE__, string func = __FUNCTION__, T...)(string msg, T args) {
	foreach(logger; _loggers[0 .. _numLoggers]) {
		auto header = LogHeader(level, currTimeNoGC(), file, line, func);
		logger.beginMessage(header);
		scope tmpdg = delegate(const(char[]) dat) {
			logger.logPart(header, dat);
		};
		tmpdg.formattedWriteNoGC(msg, args);
		logger.endMessage(header);
	}
}

/// The type of the delegate to use for formattedWriteNoGC.
alias SinkFunc = @nogc void delegate(const(char[]));

/// Invokes formattedWrite with the expectation that no exception will be thrown,
/// and thus no GC allocations will be made.
/// Instead of appending to an OutputRange, invokes a sink delegate to take in the data.
@nogc uint formattedWriteNoGC(SinkType, Char, A...)(SinkType sink, in Char[] fmt, A args) {
	alias RangeType = FormatSinkOutputRange!Char;
	alias FuncType = @nogc uint function(RangeType, const(Char[]), A);
	auto cbf = &formattedWrite!(RangeType, Char, typeof(args));
	auto cb = cast(FuncType)cbf;
	auto range = RangeType(sink);
	return cb(range, fmt, args);
}

private struct FormatSinkOutputRange(Char) {
	@nogc this(SinkFunc sink) {
		this.sink = sink;
	}
	
	@nogc void put(in Char[] val) {
		sink(val);
	}
	
	SinkFunc sink;
}

private @nogc SysTime currTimeNoGC() {
	alias FuncType = @nogc SysTime function(immutable TimeZone tz = LocalTime());
	auto funcGC = &Clock.currTime;
	auto func = cast(FuncType)funcGC;
	return func();
}

// TODO: Don't use a static array, instead use a dynamic array with malloc.
private enum MAX_LOGGERS = 32;
private Logger[MAX_LOGGERS] _loggers;
private size_t _numLoggers = 0;
private SlimSpinLock _globalLock;

/*/// An abstract class used to write to a log.
class Logger {
	
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
	abstract void PerformLog(in char[] messagePart);

private:	
	static __gshared Logger _Default;		
	SlimSpinLock lock;
}

/// Appends the specified message to the log file. This operation is thread-safe.
/// Params:
///		LogName = The name of the log being written to.
///		Message = The message being appended to the log file.
void Log(in char[] LogName, in char[] Message) { Logger.Default.LogMessage(LogName, Message); }

void LogIf(bool Condition, lazy string LogName, lazy string Message) {
	if(Condition)
		Log(LogName, Message);
}*/