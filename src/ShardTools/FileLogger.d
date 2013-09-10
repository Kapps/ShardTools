module ShardTools.FileLogger;

public import ShardTools.Logger;
private import std.file;
private import std.path;
private import std.conv;

/// A basic implementation of the Logger class to log messages to a file.
/// The current implementation is naive and will perform an IO operation on each log.
class FileLogger : Logger {
public:
	
	/// Initializes a new instance of the FileLogger object.
	this() { }
	
protected:	
	/// Performs the actual writing of the message to the log.
	/// Params:
	///		LogName = The name of the log being written to. Note that if this file has no extension, a .txt extension will automatically be added.
	///		Message = The message being appended to the log file.
	/// Throws:
	///		FileException = Raised if there was an error writing to the file.
	override void PerformLog(in char[] LogName, in char[] Message) {	
		// TODO: Keep the file open or else perform logs differently.
		// This is a horrible solution, opening a file on each log...
		string ActualName = to!string(LogName.dup);
		if(extension(ActualName) is null)
			ActualName = setExtension(ActualName, "txt");
		if(!exists(ActualName))
			write(ActualName, Message ~ '\n');
		else
			append(ActualName, Message ~ '\n');
	}
}