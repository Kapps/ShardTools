/// Provides basic, global, logging capabilities without requiring any GC allocations.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Logger;

private import std.conv;
private import std.datetime;
import std.format;
import ShardTools.SpinLock;
import ShardTools.ExceptionTools;
import ShardTools.Udas;

@name("Integration Tests")
unittest {
	class TestLogger : Logger {
		int started, ended;
		size_t totalLength;
		LogLevel lastLevel;
		@nogc void beginMessage(ref LogHeader header) { started++; }
		@nogc void logPart(ref LogHeader header, in char[] part) { totalLength += part.length; lastLevel = header.level; }
		@nogc void endMessage(ref LogHeader header) { ended++; }
	}
	ubyte[__traits(classInstanceSize, TestLogger)] logbuff;
	auto tl = logbuff.emplace!TestLogger;
	registerLogger(tl);
	log("Test message.");
	assert(tl.started == tl.ended);
	assert(tl.ended == 1);
	assert(tl.totalLength == "Test Message.".length);
	tl.totalLength = 0;
	logf("This is test message #%s.", tl.ended + 1);
	assert(tl.totalLength == "This is test message #2.".length);
	assert(tl.lastLevel == LogLevel.info);
	tl.totalLength = 0;
	logtf("This is test message %s.", tl.ended + 1);
	assert(tl.totalLength == "This is test message 3.".length);
	assert(tl.lastLevel == LogLevel.trace);
	tl.totalLength = 0;
	logt("Another trace message.");
	assert(tl.totalLength == "Another trace message.".length);
	assert(tl.lastLevel == LogLevel.trace);
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
	/// The severity of the log message.
	const LogLevel level;
	/// The time that the message started being logged.
	const SysTime time;
	/// The file invoking the log function.
	const string file;
	/// The line within `file` invoking the log function.
	const int line;
	/// The caller function.
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
/// Note that Loggers may be invoked from a separate thread than they were registered in.
/// Also note that multiple Loggers in the future log in parallel with other loggers,
/// however an individual logger will never log multiple messages concurrently.
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

// TODO: Consider making logging done in a separate thread so that it's non-blocking.

/// Logs a message with the last letter corresponding to the log level (for example, d for debug).
/// To format values passed in, use the overloads that end with an 'f'.
/// The default 'log' method does not support formatting and logs an info message.
void log(LogLevel level = LogLevel.info)(string msg, string file = __FILE__, int line = __LINE__, string func = __FUNCTION__) {
	// TODO: Lock per-logger rather than using a global lock.
	_globalLock.lock();
	scope(exit)
		_globalLock.unlock();

	auto header = LogHeader(level, currTimeNoGC(), file, line, func);
	foreach(logger; _loggers[0 .. _numLoggers]) {
		logger.beginMessage(header);
		logger.logPart(header, msg);
		logger.endMessage(header);
	}
}

/// Ditto
void logf(LogLevel level = LogLevel.info, string file = __FILE__, int line = __LINE__, string func = __FUNCTION__, T...)(string msg, T args) {
	// TODO: Maybe change locking in the same way as log. Less beneficial here though.
	_globalLock.lock();
	scope(exit)
		_globalLock.unlock();

	// Done in multiple steps so we only need to format once.
	auto header = LogHeader(level, currTimeNoGC(), file, line, func);
	foreach(logger; _loggers[0 .. _numLoggers])
		logger.beginMessage(header);
	scope tmpdg = delegate(const(char[]) dat) {
		foreach(logger; _loggers[0 .. _numLoggers])
			logger.logPart(header, dat);
	};
	tmpdg.formattedWriteNoGC(msg, args);
	foreach(logger; _loggers[0 .. _numLoggers])
		logger.endMessage(header);
}

/// Ditto
void logt(string msg, string file = __FILE__, int line = __LINE__, string func = __FUNCTION__) {
	log!(LogLevel.trace)(msg, file, line, func);
}

/// Ditto
void logtf(string file = __FILE__, int line = __LINE__, string func = __FUNCTION__, T...)(string msg, T args) {
	logf!(LogLevel.trace, file, line, func, T)(msg, args);
}

/// Ditto
void logd(string msg, string file = __FILE__, int line = __LINE__, string func = __FUNCTION__) {
	log!(LogLevel.debug_)(msg, file, line, func);
}

/// Ditto
void logdf(string file = __FILE__, int line = __LINE__, string func = __FUNCTION__, T...)(string msg, T args) {
	logf!(LogLevel.debug_, file, line, func, T)(msg, args);
}

/// Ditto
void logw(string msg, string file = __FILE__, int line = __LINE__, string func = __FUNCTION__) {
	log!(LogLevel.warn)(msg, file, line, func);
}

/// Ditto
void logwf(string file = __FILE__, int line = __LINE__, string func = __FUNCTION__, T...)(string msg, T args) {
	logf!(LogLevel.warn, file, line, func, T)(msg, args);
}

/// Ditto
void loge(string msg, string file = __FILE__, int line = __LINE__, string func = __FUNCTION__) {
	log!(LogLevel.error)(msg, file, line, func);
}

/// Ditto
void logef(string file = __FILE__, int line = __LINE__, string func = __FUNCTION__, T...)(string msg, T args) {
	logf!(LogLevel.error, file, line, func, T)(msg, args);
}

/// Attempts to log a message, returning whether successful or not.
/// An attempt to log is successful if no underlying logger throws an Exception during the attempt.
bool tryLogf(LogLevel level = LogLevel.info, string file = __FILE__, int line = __LINE__, string func = __FUNCTION__, T...)(string msg, T args) {
	try {
		logf!(level, file, line, func, T)(msg, args);
		return true;
	} catch(Exception ex) {
		return false;
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
	static if(__VERSION__ >= 2069)
		auto funcGC = &Clock.currTime!(ClockType.normal);
	else
		auto funcGC = &Clock.currTime;
	auto func = cast(FuncType)funcGC;
	return func();
}


// TODO: Don't use a static array, instead use a dynamic array with malloc.
// Once std.container.array is fixed to be @nogc we can use that.
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