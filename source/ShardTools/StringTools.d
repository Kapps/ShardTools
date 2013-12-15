/// Provides methods for performing fast yet unsafe string operations.
/// $(RED This module is likely not needed with std.ascii existing, and should be considered deprecated.)
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.StringTools;

import std.conv;
import std.string;

// TODO: This module should maybe be phased out. Was from an earlier time.

// The below functions due to awful preformance of std.string's equivalents.

/// Provides a faster toLower implementation for ascii characters.
pure inout(char[]) toLowerAscii(inout(char[]) Input) {
	char[] Result = cast(char[])Input.dup;
	foreach(ref char c; Result)
		if(c >= 'A' && c <= 'Z')
			c += 32;
	return cast(inout)Result;
} unittest {
	assert(toLowerAscii("ABC") == "abc");
}

/// A fast toLower implementation that edits the input array for ascii characters.
void toLowerInPlaceAscii(char[] Input) {
	foreach(ref char c; Input)
		if(c >= 'A' && c <= 'Z')
			c += 32;
} unittest {
	char[] SomeInput = "ABC".dup;
	toLowerInPlaceAscii(SomeInput);
	assert(SomeInput == "abc".dup);
}

/// Determines whether the two ascii strings are equal.
pure bool EqualsInsensitiveAscii(in char[] First, in char[] Second) {
	if(First.length != Second.length)
		return false;
	const(char)* FP = First.ptr, SP = Second.ptr;
	const(char)* End = FP + First.length;
	while(FP != End) {
		if(*FP != *SP) {
			char c1 = *FP;
			char c2 = *SP;
			int diff = c2 - c1;	
			// TODO: The below can maybe be optimized. But probably doesn't matter.		
			// Check if they're both letters.
			if(((c1 >= 'A' && c1 <= 'Z') || (c1 >= 'a' && c1 <= 'z')) && ((c2 >= 'A' && c2 <= 'Z') || (c2 >= 'a' && c2 <= 'z'))) {
				// If they're letters, the difference has to be either 32 or -32 depending which is upper.
				if(diff != 32 && diff != -32)
					return false;
			} else // Otherwise, they're not both letters and nor are they the same character; thus, false.
				return false;						
		}
		FP++;
		SP++;
	}
	return true;
} unittest {
	assert(EqualsInsensitiveAscii("aBc", "abc"));
	assert(EqualsInsensitiveAscii("ABC", "abc"));
	assert(EqualsInsensitiveAscii("abc", "abc"));
	assert(EqualsInsensitiveAscii("ABC", "ABC"));

	assert(!EqualsInsensitiveAscii("abc", "abd"));
	assert(!EqualsInsensitiveAscii("abd", "abcd"));
	assert(!EqualsInsensitiveAscii("ABCD", "abc"));

	assert(EqualsInsensitiveAscii(null, null));
	assert(!EqualsInsensitiveAscii(null, "ab"));
	assert(!EqualsInsensitiveAscii("ab", null));

	assert(EqualsInsensitiveAscii("abc123", "aBC123"));
	assert(!EqualsInsensitiveAscii("!bc123", "abc123"));
}

/// Returns a null-terminated string containing the letters in String up to any null terminated characters, or otherwise the length.
/// A new string is always made, even if String ends with a single null terminator.
static char[] Terminate(in char[] String) {	
	for(size_t i = 0; i < String.length; i++)
		if(String[i] == '\0')
			return String[0 .. i].dup;	
	return String[0 .. String.length].dup ~ '\0';			
}	

/// Trims the length of the specified string to equal to the size of the string up to the first found null terminator.
/// Params: 
///		String = [inout] The string to alter the length of.
///		IncludeNull = Whether to include the null terminator in the altered string.
/// Returns: Whether any alterations were performed. None will be performed if the string ends in null or did not contain any null terminators.
static bool TrimToNull(ref char[] String, bool IncludeNull = false) {
	size_t nullIndex = IndexOfNull(String);
	if(nullIndex == -1)
		return false;
	if(nullIndex == String.length - 1)
		if(IncludeNull)
			return false;				
	String = String[0 .. nullIndex + (IncludeNull ? 1 : 0)];	
	return true;
}

/// Trims the length of the specified string to equal to the size of the string up to the first found null terminator.
/// Unlike TrimToNull, this method returns the string passed in, and instead WasTrimmed is set to false if the string was not trimmed.
static char[] TrimReturn(ref char[] String, bool IncludeNull = false, bool* WasTrimmed = null) {
	if(!IsTerminated(String)) {
		if(WasTrimmed !is null)
			*WasTrimmed = false;
		return String;
	}			
	if(WasTrimmed != null)
		*WasTrimmed = TrimToNull(String, IncludeNull);
	else
		TrimToNull(String, IncludeNull);
	return String;
}

/// Returns the index of the first null terminator inside the specified string, or -1 if none was found.	
static size_t IndexOfNull(in char[] String) {
	for(size_t i = 0; i < String.length; i++)
		if(String[i] == '\0')
			return i;
	return -1;
}

/// Returns whether the specified string contains any null terminators.
static bool IsTerminated(in char[] String) {
	return IndexOfNull(String) != -1;
}