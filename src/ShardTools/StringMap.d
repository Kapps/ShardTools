module ShardTools.StringMap;
import std.stdio : writeln;
private import std.exception;
private import std.algorithm;
private import ShardTools.DateParse;
private import std.datetime;
private import std.conv;
private import ShardTools.StringTools;
private import std.format;
private import ShardTools.Map;


/// Provides quick access to a map of strings by case insensitive strings, with features for easy parsing.
class StringMap  {

	// TODO: Make a MaxHashCollisions property, and throw if more than that amount occurs.

public:
	/// Initializes a new instance of the StringMap object.
	this() {
		
	}

	/// Gets the number of headers contained in this collection.
	@property size_t Count() const {
		return Headers.length;
	}

	/// Gets all of the header names in this map.
	@property auto Names() {
		return filter!(c=>c.Name)(Headers.byKey());
	}

	/// Gets all of the header values in this map.
	@property auto Values() {
		return Headers.byValue();
	}

	/// Indicates whether the map is currently read-only, and thus no more changes are allowed.
	@property bool Readonly() {
		return _Readonly;
	}

	/// Causes this map to become readonly, preventing further changes.
	void MakeReadonly() {
		_Readonly = true;
	}

	/// Sets the given parameter to the given value.
	/// Params:
	/// 	Name = The name of the parameter.
	/// 	Value = The value to set.
	void Set(string Name, string Value) {
		enforce(!_Readonly, "The map was readonly and thus changes are not allowed.");		
		Headers[ParamName(Name)] = Value;		
	}

	/// Gets the parameter with the given name, converted from a string into the given type.
	/// If the conversion fails, or the parameter did not exist, DefaultValue is returned.
	/// Params:
	/// 	T = The parsed type of the parameter.
	/// 	Name = The name of the parameter.
	/// 	DefaultValue = The value to return if the result is malformed or did not exist.
	T Get(T)(string Name, lazy T DefaultValue = T.init) {		
		string* Ptr = (ParamName(Name) in Headers);
		if(Ptr is null)
			return DefaultValue();
		string Unparsed = *Ptr;
		try {			
			static if(is(T == bool)) {
				if(Unparsed == "1" || EqualsInsensitiveAscii(Unparsed, "true") || EqualsInsensitiveAscii(Unparsed, "yes"))
					return true;
				else if(Unparsed == "0" || EqualsInsensitiveAscii(Unparsed, "false") || EqualsInsensitiveAscii(Unparsed, "no"))
					return false;
				return DefaultValue();
			} else static if(is(T == string)) {
				return Unparsed;
			} else static if(is(T == DateTime)) {
				return DateParse.parseHttp(Unparsed, DefaultValue());
			} else {
				//return DefaultValue();
				return to!T(Unparsed);
			}
		} catch {
			return DefaultValue();
		}
	}

	/// Indicates whether this map contains an element with the given key.
	bool Contains(string Name) {
		return (ParamName(Name) in Headers) !is null;		
	}

	string opIndex(string Name) {
		return Get!string(Name, null);
	}

	int opApply(int delegate(string, string) dg) {
		int Result = 0;
		foreach(Name, Values; Headers) {
			if((Result = dg(Name.Name, Values)) != 0)
				break;
		}
		return Result;
	}

	override string toString() {
		return to!string(Headers);
	}
	
private:
	string[ParamName] Headers;	
	bool _Readonly;

	struct ParamName {
		string Name;		

		this(string Name) {
			this.Name = Name;			
		}
		
		const pure int opCmp(const ref ParamName other) {				
			return cmp(toLowerAscii(Name), toLowerAscii(other.Name));
		}

		const bool opEquals(const ref ParamName other) {							
			return EqualsInsensitiveAscii(Name, other.Name);
		}

		const string toString() {
			return Name;
		}

		const pure nothrow @safe hash_t toHash() {
			hash_t Result;
			foreach(char c; Name) {				
				Result = (Result * 9) + (c >= 'A' && c <= 'Z') ? c - 32 : c;
			}			
			return Result;
		}
	}
}