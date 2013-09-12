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
/// Methods are not yet supported, but @Command will be used once they are.
T getCommandLineOptions(T)(ref string[] args) {
	// TODO: If already using reflection, should probably support Variants?
	// TODO: How can we support non-strings?
	// We'd need to coerce to the Variant.
	// This is doable, but would require some reflection changes perhaps.
	// Namely a way to set a value or invoke a method with specifying if coercion is allowed.
	// Well, we know it at compile-time, so we can just do that.
	// Except the metadata isn't created at compile-time.
	// Would be quite nice if we could get reflection to be created at compile-time.
	// Would make it much easier to mix the two and less worry about runtime performance perhaps...
	// Now can use TypeMetadata.castFrom.
	auto metadata = createMetadata!T;
	T instance = metadata.createInstance().get!T;
	foreach(value; usedValues(metadata)) {
		string name = value.findAttribute!DisplayName(DisplayName(value.name));
		TypeMetadata paramTypeData = value.type.metadata;
		void setValue(string key, string arg) {
			Variant convertedVal = paramTypeData.coerceFrom(arg);
			value.setValue(instance, convertedVal);
			debug writeln("Set value of " ~ name ~ " to " ~ arg ~ " (" ~ convertedVal.text ~ ").");
		}
		size_t oldLength = args.length;
		getopt(args, config.caseInsensitive | config.passThrough, name, &setValue);
		if(args.length == oldLength && value.findAttribute!Required(Required(false)))
			throw new MissingArgumentException("Required argument " ~ name ~ " was not found.");
	}
	return instance;
}

/// Returns a multi-line formatted help string that can be displayed to the user for what values are allowed for this type.
/// This returns 
string getHelpString(T)() {
	auto metadata = createMetadata!T;
	T defaultInstance = metadata.createInstance().get!T;
	string result = "";
	foreach(value; usedValues(metadata)) {
		string displayName = "--" ~ value.findAttribute!DisplayName(DisplayName(value.name));
		string description = value.findAttribute!Description;
		dchar shortName = value.findAttribute!ShortName(ShortName(dchar.init));
		if(shortName != dchar.init)
			displayName = "-" ~ shortName.text ~ "|" ~ displayName;
		displayName ~= "Default: " ~ value.getValue(defaultInstance).text;
		string line = "[" ~ displayName ~ "] : " ~ description;
		result ~= line;
	}
	return result;
}

private auto usedValues(TypeMetadata metadata) {
	return metadata.children.values.filter!(c=>!c.findAttribute!Ignore(Ignore(false)).value && c.protection == ProtectionLevel.public_);
}

/+/// Returns the first attribute of the given type on the given member.
/// Returns void if not found.
private static auto getSingleAttributeValue(AttribType, alias member)() {
	foreach(attrib; __traits(getAttributes, member)) {
		static if(is(attrib == AttribType)) {
			static assert(attrib.tupleof.length == 1, "Expected attribute to have only a single member.");
			return attrib.tupleof[0];
		}
	}
	return void;
}+/

/// Indicates that the given letter can be used as an alternative to this argument name.
struct ShortName {
	dchar letter;

	alias letter this;
}

@disable struct Command {
	string name;
}