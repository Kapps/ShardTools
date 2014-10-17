module ShardTools.ConsoleLogger;
import ShardTools.Logger;
import std.datetime;
import core.stdc.stdio;
import ShardTools.ScopeString;

private extern(C) @nogc nothrow size_t fwrite (const void* ptr, size_t size, size_t count, FILE* stream);

/// Provides a logger that writes to the standard output and error streams.
class ConsoleLogger : Logger {

	override @nogc void beginMessage(ref LogHeader header) {
		scope dg = delegate(const(char[]) dat) @nogc {
			printData(streamFor(header.level), dat);
		};
		auto timeFun = cast(TimeOfDay function(SysTime) @nogc)&convertTime;
		auto time = timeFun(header.time);
		dg.formattedWriteNoGC("[%s] %s: ", time, header.level);
	}

	override @nogc void endMessage(ref LogHeader header) {
		/+scope dg = delegate(const(char[]) dat) {
			printData(streamFor(header.level), dat);
		};
		dg.formattedWriteNoGC(" (%s:%s)\n", header.file, header.line);+/
		auto stream = streamFor(header.level);
		fputc('\n', stream);
		//fflush(stream);
	}

	override @nogc void logPart(ref LogHeader header, in char[] part) {
		fwrite(part.ptr, char.sizeof, part.length, streamFor(header.level));
		//printf("%s", buff.ptr);
	}

	private @nogc void printData(FILE* stream, in char[] dat) {
		fwrite(dat.ptr, char.sizeof, dat.length, stream);
	}
}

private TimeOfDay convertTime(SysTime time) {
	return cast(TimeOfDay)time;
}

private @nogc FILE* streamFor(LogLevel level) {
	if(cast(int)level < cast(int)LogLevel.error)
		return stdout;
	return stderr;
}