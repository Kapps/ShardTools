module ShardTools.PathTools;
import std.algorithm;
import ShardTools.StringTools;
import std.exception;
import std.conv;
import std.string;
import std.file;
import std.path;
import std.process;
import ShardTools.Enforce;
import ShardTools.ArrayOps;
import std.array;

// TODO: This class comes from a time before sufficient path / file management in Phobos.
// Should we just switch to that completely?

/+
/// An enum describing on how to handle a relative path.
enum PathStyle {
	/// The path should be relative to the application exe directory (may or may not be the same as the CurrentDirectory).
	ApplicationDirectory,
	/// The path should be relative to the current directory.
	CurrentDirectory
}+/

version(Windows) { 
	import std.c.windows.windows;		
	static const ushort MAX_PATH = 260;	
	enum string DirSeparator = "\\";
	extern(Windows) {
		BOOL PathRelativePathToA(LPTSTR, LPCTSTR, DWORD, LPCTSTR, DWORD);		
		BOOL PathStripToRootA(LPTSTR);
		LPTSTR PathRemoveBackslashA(LPTSTR);
	}			
	extern(Windows) {
		DWORD GetCurrentDirectoryA(DWORD, LPTSTR);
		BOOL SetCurrentDirectoryA(LPCTSTR);
		DWORD GetFullPathNameA(LPCTSTR, DWORD, LPTSTR, LPTSTR*);
		DWORD GetModuleFileName(HMODULE, LPTSTR, DWORD);
		/+DWORD GetFileAttributesA(LPCTSTR);
		enum DWORD INVALID_FILE_ATTRIBUTES = -1;
		enum DWORD FILE_ATTRIBUTE_HIDDEN = 2;+/
	}
} else {
	import core.sys.posix.unistd : readLinkPosix = readlink;
	enum string DirSeparator = "/";
}

/// Static helper class used to manage paths consistently across platforms.
/// This class comes from a time before the new std.path in Phobos.
/// New code generally won't have to use this class for most operations.
/// However it does provide utilities that std.path may not, and results may differ.
static class PathTools {
public:	
	
	/// Returns the FilePath without the filename or extension.
	static inout(char[]) GetDirectoryPath(inout(char[]) FilePath) {	
		auto Result = MakeAbsolute(cast(char[])dirName(cast(string)FilePath));
		return cast(inout)Result;
	}


	shared static this() {
		SyncLock = new Object();
	}
	
	unittest {
		version(Windows) {		
			// TODO: Fix this stuff.
			EnforceEqual(GetDirectoryPath("D:\\Test\\Test.exe"), "D:\\Test");			
			assert(Equals(GetDirectoryPath("D:\\Test\\Test.exe"), "D:\\Test"));
			assert(Equals(GetDirectoryPath("D:\\Test\\"), "D:\\Test"));
			string TestExe = "D:\\Test\\Test.exe";
			string Path = GetDirectoryPath(TestExe);
			assert(Path[Path.length - 1] != '\\' && Path[Path.length - 1] != '/');
			string DirPath = GetDirectoryPath("D:\\Test.exe");
			EnforceEqual(DirPath, "D:\\");			
		}
	}
	
	/// Adds a trailing slash on Posix, or backslash on Windows to the specified path if it does not already contain one.
	static inout(char[]) AddTrailingSeparator(inout char[] FilePath) {
		if(FilePath.empty) {
			assert(!CurrentDirectory.empty);
			return cast(inout(char[]))AddTrailingSeparator(CurrentDirectory.dup);
		}
		if(isDirSeparator(FilePath[FilePath.length - 1]))
			return cast(inout)(FilePath ~ DirSeparator);
		return FilePath;
	}
	
	unittest {
		version(Windows) {
			EnforceEqual(AddTrailingSlash("D:\\Test"), "D:\\Test\\".dup);
		} else {
			EnforceEqual(AddTrailingSlash("/test"), "/test/".dup);
			EnforceEqual(AddTrailingSlash("/test/"), "/test/".dup);
		}
	}
	
	/// Returns a value indicating whether the CurrentDirectory is the same as the directory containing the .exe file used to launch this application.
	static bool CurrentEqualsApplicationDir() {
		return Equals(ApplicationDirectory, CurrentDirectory);
	}

	/// Gets the extension for the given path, including the leading dot, or an empty string if no extension was present.
	static inout(char[]) GetExtension(inout char[] FilePath) {
		char[] Result = extension(FilePath.dup);
		if(Result is null)
			Result = new char[0];
		return cast(inout)Result;
	}

	/// Returns whether Path is located within the given Directory or any of it's subfolders.
	/// Params:
	/// 	Path = The path to check if within Directory.
	/// 	Directory = The path to check if Path is within.
	static bool IsPathWithinDirectory(in char[] Path, in char[] Directory) {
		/*auto AbsPath = MakeAbsolute(Path);
		auto AbsDir = MakeAbsolute(Directory);
		if(countUntil(AbsPath, "..") != -1)
			return false;*/
		const(char[]) Relative = GetRelativePath(Path, Directory);
		if(Relative is null || Relative.length == 0)
			return false;
		return countUntil(Relative, "..") == -1;
		//return std.string.indexOf(Relative, "..") == -1;		
		//return (First > 'A' && First < 'Z') || (First > 'a' && First < 'z') || (First == '\\' || First == '/');
	}

	unittest {
		version(Windows) {
			assert(IsPathWithinDirectory("D:\\Test\\test.exe", "D:\\Test\\"));
			assert(!IsPathWithinDirectory("D:\\Test\\Test.exe", "D:\\Test\\SubTest"));
			assert(!IsPathWithinDirectory("D:\\Test\\..\\Test.exe", "D:\\Test\\"));
		} else {
			assert(IsPathWithinDirectory("/Test/Test.exe", "/Test/"));
		}		
	}
	
	/// Returns the absolute path of the specified path.
	/// Params: 
	///		Path = The path to make absolute.
	///		Base = Determines the base path to use when making the path absolute.
	static inout(char[]) MakeAbsolute(inout char[] Path) {		 // TODO: Make thread safe.
		version(Windows) {
			char[] Copy = Terminate(Path);
			char[] Result = new char[MAX_PATH];
			uint ResultLen = GetFullPathNameA(Copy.ptr, MAX_PATH, Result.ptr, null);			
			enforce(ResultLen <= MAX_PATH && ResultLen != 0);
			Result = Result.TrimReturn;						
			return cast(inout)Result.TrimReturn;
		} else {
			char[] Result;
			if(IsAbsolute(Path))
				Result = Path.dup;
			Result = absolutePath(cast(immutable)Path).dup;
			Result = buildNormalizedPath(Result).dup;
			//if(IsDirectory(Result))
				//Result = AddTrailingSlash(Result);
			return cast(inout)Result;			
		}
	}
	
	unittest {
		version(Windows) {
			//EnforceEqual(MakeAbsolute("Test.exe"), CurrentDirectory() ~ "\\Test.exe");
			assert(Equals(MakeAbsolute("Test.exe"), CurrentDirectory() ~ "\\Test.exe"));	
		} else {
			assert(Equals(MakeAbsolute("Test.exe"), CurrentDirectory() ~ "/Test.exe"));
		}
	}
	
	/// Returns whether the specified path is an absolute path.
	/// Params: Path = The path to check if absolute.
	static bool IsAbsolute(in char[] Path) {
		return isAbsolute(Path);
	}
	
	unittest {
		version(Windows)
			assert(IsAbsolute("D:/Test/Test.exe"));
		else
			assert(IsAbsolute("/Test/Test.exe"));
		assert(!IsAbsolute("Test.exe"));
	}
	
	/// Returns whether the specified path is located in the current working directory, either directly or as a sub-folder.
	/// Params: Path = The path to check.
	static bool IsInWorkingDirectory(in char[] Path) {		
		return IsPathWithinDirectory(cast(immutable)Path, cast(immutable)CurrentDirectory);		
	}
	
	unittest {
		assert(IsInWorkingDirectory("Test.exe"));
		assert(IsInWorkingDirectory(CurrentDirectory ~ "/Test.exe"));
		version(Windows)
			assert(!IsInWorkingDirectory("D:/toesasdojadoasjd/Test.exe"));
		else
			assert(!IsInWorkingDirectory("/toesasdojadoasjd/Test.exe"));
		assert(!IsInWorkingDirectory(CurrentDirectory ~ "/..//Test.exe"));
	}

	/// Queries the file system to check if the file located at the given path is a hidden file or within a hidden folder.
	/// An exception is thrown if the file does not exist.
	/// Params:
	/// 	FilePath = The path to the file.
	static bool IsHidden(in char[] FilePath) {
		string FullPath = cast(immutable)MakeAbsolute(FilePath);
		enforce(exists(FullPath), "The file to check if hidden did not exist.");
		version(Windows) {
			// TODO: Check if in a hidden directory!
			DWORD FileAttribs = GetFileAttributesA(toStringz(FilePath));
			enforce(FileAttribs != -1, "Error getting the file attributes.");
			if((FileAttribs & FILE_ATTRIBUTE_HIDDEN) != 0)
				return true;
			return false;
		} else version(linux) {
			foreach(const(char[]) Part; splitter(FilePath, '/')) {
				string Trimmed = cast(string)strip(Part);
				if(Trimmed.length == 0)
					continue;
				if(Trimmed[0] == '.')
					return true;
			}
			return false;
		} else
			static assert(0, "Not yet implemented.");
	}
	
	/// Returns whether the specified valid path points to a directory.
	/// If the directory exists, this is equivalent to std.path.isDir(Path).
	/// If DirectoryMustExist is false and the path does not exist, returns false.
	/// Otherwise, returns whether the path ends with a directory separator or is a dot.
	static bool IsDirectory(in char[] Path, bool DirectoryMustExist = false) {
		bool Exists = exists(Path);
		if(DirectoryMustExist && !Exists)
			return false;
		if(!Exists)
			return Path[$-1] == '/' || Path == ".";
		return isDir(Path);
	}
	
	unittest {
		assert(!IsDirectory("D:/Test/Test.exe"));
		assert(IsDirectory("C:/Windows/"));
		assert(IsDirectory("D:/TestageDir/"));
		assert(IsDirectory("./"));
	}	
	
	/// Returns the relative path required to reach EndPath from StartPath.
	/// Does not include a trailing separator.
	/// Params:
	///		EndPath = The target path to reach.
	///		StartPath = The path to start from.	
	/// Returns: Null if the paths were not able to have a relative path formed.
	static inout(char[]) GetRelativePath(inout char[] EndPath, inout char[] StartPath) {
		/*version(Windows) {
			char[] Buffer = new char[MAX_PATH];	
			char[] startCopy = Terminate(StartPath);
			char[] endCopy = Terminate(EndPath);
			if(PathRelativePathToA(Buffer.ptr, startCopy.ptr, !IsDirectory(startCopy) ? 0 : FILE_ATTRIBUTE_DIRECTORY, endCopy.ptr, !IsDirectory(endCopy) ? 0 : FILE_ATTRIBUTE_DIRECTORY))
				return TrimReturn(Buffer);
			return null;				
		} else {			*/			
			if(!IsAbsolute(EndPath))
				return cast(inout)EndPath.dup;			
			string Relative = relativePath(cast(immutable)EndPath, cast(immutable)MakeAbsolute(StartPath));
			if(Relative.length == 0 || Relative == EndPath)
				return null;					
			while(Relative[$-1] == '\\' || Relative[$-1] == '/')
				Relative = Relative[0..$-1];
			return cast(inout)Relative.dup;
		//}
	}	

	unittest {		
		/*version(Windows) {
			EnforceEqual(GetRelativePath("D:\\Testing\\Test.exe", "D:\\testing"), "Test.exe");			
			EnforceEqual(GetRelativePath("D:\\Testing\\Test.exe", "C:\\Test"), cast(char[])null);
			EnforceEqual(GetRelativePath("D:\\RandomTest.exe", "D:\\Testing\\SomeTest\\RandomTest.exe"), "..\\..\\..\\RandomTest.exe");
		} else {
			EnforceEqual(GetRelativePath("/Testing/Test.exe", "/testing"), "Test.exe");
			EnforceEqual(GetRelativePath("/Testing/Test.exe", "/Test"), cast(char[])null);
			EnforceEqual(GetRelativePath("/RandomTest.exe", "/Testing/SomeTest/RandomTest.exe"), "../../../RandomTest.exe");
		}*/
		// Above don't work since not real files.
	}
		
	/// Returns the current working directory.
	@property static string CurrentDirectory() { // TODO: Make thread safe.
		version(Windows) {
			synchronized(SyncLock) {
				char[] Result = new char[MAX_PATH];			
				uint Length = enforce(GetCurrentDirectoryA(cast(uint)Result.length, Result.ptr));			
				Result.length = Length;
				return cast(immutable)Result;			
			}
		} else {
			return getcwd();
		}
	}
	
	/// Returns the full path to the application executable file.
	@property static string ApplicationPath() {	
		if(_ApplicationPath)
			return _ApplicationPath;		
		synchronized(SyncLock) {
			version(Windows) {																		
				char[MAX_PATH] Buffer;
				int ReturnSize = GetModuleFileNameA(null, Buffer.ptr, MAX_PATH);
				enforce(ReturnSize > 0, "GetModuleFileNameA failed!");					
				_ApplicationPath = Buffer[0..ReturnSize].idup;																		
			}  else version(linux) {				
				char[2048] Buffer;				
				size_t NumChars = readLinkPosix("/proc/self/exe", Buffer.ptr, Buffer.length);
				if(NumChars >= Buffer.length) {
					char[] BiggerBuffer = new char[NumChars + 1];
					size_t BiggerChars = readLinkPosix("/proc/self/exe", BiggerBuffer.ptr, BiggerBuffer.length);
					enforce(BiggerChars < BiggerBuffer.length, "Error in getting application path. Your path may be too long.");
					_ApplicationPath = BiggerBuffer[0..BiggerChars].idup;
				} else
					_ApplicationPath = Buffer[0..NumChars].idup;
				if(_ApplicationPath[$-1] == '\0')
					_ApplicationPath = _ApplicationDir[0..$-1];				
				_ApplicationPath = MakeAbsolute(_ApplicationPath);
			} else {
				assert(0, "ApplicationPath not yet supported on platforms besides Linux and Windows.");
			}
		}
		return _ApplicationPath;
	}

	/// Returns the full path to just the directory of the application executable file.
	@property static string ApplicationDirectory() {		
		if(_ApplicationDir)
			return _ApplicationDir;	
		string AppPath = ApplicationPath;		
		string DirPath = GetDirectoryPath(AppPath);		
		_ApplicationDir = DirPath;
		return _ApplicationDir;
	}
	
	
	/// Sets the current working directory to the specified value.
	static void SetWorkingDirectory(in char[] Path) {
		synchronized(SyncLock) {
			version(Windows) {				
				char[] Copy = Terminate(Path);				
				int Result = SetCurrentDirectoryA(Copy.ptr);				
				enforce(Result != 0, "Result was " ~ to!string(Result) ~ " for SetWorkingDirectory with Path of \'" ~ to!string(Path) ~ "\'.");
			} else {
				chdir(Path);				
			}
		}
	}

	/// Gets the root drive for the specified path. On windows, this would return a result such as 'C:\' from 'C:\Test\Test2\Test.exe'.
	/// Params: Path = The path to get the root directory for.
	/// Returns: The root directory for the specified path, or null if the path did not contain a root directory (a relative path).	Always returns "/" on non-Windows systems.
	static inout(char[]) GetRoot(inout char[] Path) {		
		version(Windows) {
			return cast(inout)driveName(Path.dup);
		} else {
			return cast(inout)"/".dup;
		}
	}	

	/// Determines whether the two paths point to the same file or directory.
	static bool Equals(in char[] First, in char[] Second) {		
		version(Windows) {
			return icmp(MakeAbsolute(First), MakeAbsolute(Second)) == 0;
		} else {
			return cmp(MakeAbsolute(First), MakeAbsolute(Second)) == 0;
		}
	}

	unittest {
		version(Windows) {
			string abs = "D:/Test/Test.txt";		
			assert(!Equals(MakeAbsolute("D:/Test/Test/OtherTest/../Test"), abs));
			//assert(Equals(MakeAbsolute("Test.exe"), GetWorkingDirectory() ~ "\\Test.exe"));		
			assert(Equals(MakeAbsolute("D:/Test/Test/../Test.txt"), abs));
		} else {
			EnforceEqual(MakeAbsolute("/Test/Test/OtherTest/../Test"), "/Test/Test/Test");
			EnforceEqual(MakeAbsolute("/Test/OtherTest/../Test.txt"), "/Test/Test.txt");
		}			
	}

	/// Returns the specified path minus any trailing separators, such as backslashes.
	/// Params: Path = The path to remove trailing separators from.
	static inout(char[]) RemoveTrailingSeparator(inout char[] Path) {
		char[] Result = Path.dup;
		version(Windows) {
			while(Result.length > 0 && (Result[Result.length - 1] == '/' || Result[Result.length - 1] == '\\'))
				Result = Result[0..$-1];
		} else {			
			while(Result.length > 0 && Result[Result.length - 1] == '/')
				Result = Result[0..$-1];			
		}
		return cast(inout)Result;
	}

	unittest {
		EnforceEqual(RemoveTrailingSeparator("D:/Test/"), "D:/Test");
		EnforceEqual(RemoveTrailingSeparator("D:/Test"), "D:/Test");
	}

private:
	static __gshared Object SyncLock;
	static __gshared string _ApplicationPath = null;
	static __gshared string _ApplicationDir = null;
}