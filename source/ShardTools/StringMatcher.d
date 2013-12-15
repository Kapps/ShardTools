/// Allows various methods of efficiently matching a string, such as a regex or exact match.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.StringMatcher;
private import std.functional;
public import std.regex;

/// Provides a regex or callback used to determine whether a string matches a pattern.
class StringMatcher  {

public:

	/// Creates a StringMatcher from the given (runtime or compiletime) regex or callback.
	/// If a regex is used, the pattern is matched if match succeeds.
	/// If a function or delegate is used, the callback is invoked and the pattern is matched if it returns true.
	/// If a string is used, a regex is created for the string and used as if a regex was passed in.
	/// If a boolean is used, that boolean is used as a constant and always returned.
	this(Regex!char Pattern) {		
		this.RuntimeRegex = Pattern;
		this.Type = PatternType.RuntimeRegex;
	}
	
	/// Ditto
	this(StaticRegex!char Pattern) {		
		this.CompileRegex = Pattern;
		this.Type = PatternType.CompileRegex;
	}

	/// Ditto
	this(bool delegate(string) Verifier) {
		this.Verifier = Verifier;
		this.Type = PatternType.Verifier;
	}

	/// Ditto
	this(bool function(string) Verifier) {
		this(toDelegate(Verifier));
	}

	/// Ditto
	this(string Pattern) {
		this(regex(Pattern));
	}

	/// Ditto
	this(bool Constant) {
		this.Constant = Constant;
		this.Type = PatternType.Constant;
	}
	
	/// Determines whether this StringMatcher matches the given input.
	bool Match(string Input) {				
		final switch(this.Type) {
			case PatternType.Constant:
				return Constant;
			case PatternType.CompileRegex:
				return !match(Input, CompileRegex).empty();
			case PatternType.Verifier:
				return Verifier(Input);			
			case PatternType.RuntimeRegex:			
				return !match(Input, RuntimeRegex).empty();						
		}		
	}
	
private:
	union {
		Regex!char RuntimeRegex;
		StaticRegex!char CompileRegex;
		bool delegate(string) Verifier;
		bool Constant;
	}
	enum PatternType {
		RuntimeRegex,
		CompileRegex,
		Verifier,
		Constant
	}
	PatternType Type;
}