/// Provides basic compression functions for various formats.
/// $(RED The implementation of this module is buggy and the module will likely be scrapped.)
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Compressor;
import etc.c.zlib;
import std.zlib;
import std.c.stdlib;
import std.c.string;

/// A class used to compress data in various formats.
public class Compressor  {

public:
	/// Compresses the given data using the deflate specification.
	static void[] ToDeflate(void[] Data, bool IncludeMagicNumber = false) {		
		void[] Result = cast(void[])compress(Data);
		char* Ptr = cast(char*)Result.ptr;
		assert(Ptr[0] == 0x78 && Ptr[1] == 0x9C);
		if(!IncludeMagicNumber)
			Result = Result[2..$]; // Strip off magic number.
		return Result;
	}

	/// Compresses the given data using the gzip specification.
	/// This is equivalent to DeflateToGzip(ToDeflate(Data, false)).
	static void[] ToGzip(void[] Data) {
		void[] Deflated = ToDeflate(Data, false);
		return DeflateToGzip(Deflated, Data);
	}

	/// Converts the data that was converted to gzip format with ToGzip into deflate format.
	/// This ONLY works for data converted with ToGzip, as it assumes the header is 10 bytes.
	/// The data returned is the deflate portion of the gzip stream, and thus editing this data edits the initial gzip data.
	static void[] GzipToDeflate(void[] Data) {
		return (cast(ubyte[])Data)[10..$-8];
	}

	/// Converts the data that was converted to deflate format with ToDeflate into gzip format.	
	/// The original data, or the crc and length of the original data, is required.
	/// BUGS:
	///		This may or may not work properly for all devices.
	/// 	The Android Browser does not seem to like things compressed with this method, so there is likely a bug somewhere.
	static void[] DeflateToGzip(void[] Data, const void[] Original) {
		return ToGzipInternal(Data, Original.length, std.zlib.crc32(0, Original));
	}

	/// Ditto
	static void[] DeflateToGzip(void[] Data, uint OriginalCRC, ulong OriginalLength) {
		return ToGzipInternal(Data, OriginalLength, OriginalCRC);
	}

	private static void[] ToGzipInternal(void[] Deflated, ulong OriginalLength, uint OriginalCRC) {
		enum size_t HeaderSize = 10;
		enum size_t FooterSize = 8;
		ubyte[] Result = new ubyte[HeaderSize + FooterSize + Deflated.length];		
		Result[0] = 0x1F; // Magic Number (2 bytes).
		Result[1] = 0x8B;
		Result[2] = 8; // Deflate algorithm.
		Result[3] = 0; // Flags. None.
		Result[4] = 0; // Timestamp indicating last change (4 bytes).
		Result[5] = 0;
		Result[6] = 0;
		Result[7] = 0;
		Result[8] = 0; // Extra flags. None.
		version(Windows)
			Result[9] = 0;
		else version(Posix)
			Result[9] = 3;		
		else
			Result[9] = 255;
		memcpy(Result.ptr + HeaderSize, Deflated.ptr, Deflated.length);
		uint[2] Footer;
		Footer[0] = OriginalCRC;
		Footer[1] = cast(uint)(OriginalLength % uint.max);
		memcpy(Result.ptr + HeaderSize + Deflated.length, Footer.ptr, FooterSize);
		void[] FooterVoid = cast(void[])Footer;
		return cast(void[])Result;
	}
	
private:
}