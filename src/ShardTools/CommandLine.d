/// Provides setting data that utilize std.getopt with support for generating entire structs and displaying help data.
/// Methods within this module accept the UDAs of displayName, ignore, and description.
/// $(RED This module is not yet implemented.)
module ShardTools.CommandLine;
import ShardTools.Reflection;
import ShardTools.Udas;
import std.conv;
import std.stdio;
import std.getopt;
import std.algorithm;
import std.variant;
import ShardTools.ExceptionTools;

mixin(MakeException("MissingArgumentException"));

/// Returns a new instance of type T with all data populated from command line attributes.
/// All values unused are kept in args, and all used values are removed.
/// Values are allowed to be either fields or properties.
/// Methods are not yet supported, and only classes are supported at the moment.
T getCommandLineOptions(T)(ref string[] args) {
	// Once createInstance supports structs we can handle classes as well.
	auto metadata = createMetadata!T;
	T instance = metadata.createInstance().get!T;
	foreach(value; usedValues(metadata)) {
		string name = value.findAttribute!DisplayName(value.name);
		TypeMetadata paramTypeData = value.type.metadata;
		void setValue(string key, string arg) {
			Variant convertedVal = paramTypeData.coerceFrom(arg);
			value.setValue(instance, convertedVal);
		}
		size_t oldLength = args.length;
		getopt(args, config.caseInsensitive, config.passThrough, name, &setValue);
		if(args.length == oldLength && value.findAttribute!Required(false))
			throw new MissingArgumentException("Required argument " ~ name ~ " was not found.");
	}
	return instance;
}

/// Returns a multi-line formatted help string that can be displayed to the user for what values are allowed for this type.
string getHelpString(T)() {
	auto metadata = createMetadata!T;
	T defaultInstance = metadata.createInstance().get!T;
	string result = "";
	foreach(value; usedValues(metadata)) {
		string displayName = "--" ~ value.findAttribute!DisplayName(value.name);
		string description = value.findAttribute!Description("");
		dchar shortName = value.findAttribute!ShortName(dchar.init);
		if(shortName != dchar.init)
			displayName = "-" ~ shortName.text ~ "|" ~ displayName;
		string line = "[" ~ displayName ~ "]: " ~ description;
		if(value.findAttribute!Required(false))
			line ~= " (Required)";
		else
			line ~= " Default: " ~ value.getValue(defaultInstance).text;
		result ~= line ~ "\r\n";
	}
	return result.length > 0 ? result[0..$-2] : result;
}

private auto usedValues(TypeMetadata metadata) {
	return metadata.children.values.filter!(c=>!c.findAttribute!Ignore(false) && c.protection == ProtectionLevel.public_);
}

/// Indicates that the given letter can be used as an alternative to this argument name.
struct ShortName {
	dchar letter;
}