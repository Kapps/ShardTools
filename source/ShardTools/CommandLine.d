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
import std.ascii;
import std.string;
import std.array;
import std.ascii;

mixin(MakeException("CommandLineException", "An error occurred while validating input."));
mixin(MakeException("ValidationException"));

/// A bitwise enum to set optional flags for getting command line options.
enum CommandLineFlags {
	/// No flags are set.
	none = 0,
	/// An exception should not be thrown if a command is not specified and no default is set.
	allowNoCommand = 1,
	/// No data should be written to stdout.
	noWrite = 2
}

/// Returns a new instance of type T with all data populated from command line attributes.
/// All values unused are kept in args, and all used values are removed.
/// Values are allowed to be either fields or properties.
/// Methods may be invoked by use of the @Command attribute.
/// Commands will only be invoked after all values are assigned.
/// Optionally it may be required for a command to be invoked by specifying requireCommand.
/// If any input errors occur, an exception will be thrown.
/// If a CommandLineException is thrown and flags allows it, a detailed description
/// will have been printed to stdout and the exception will contain the same message.
T getCommandLineOptions(T)(ref string[] args, CommandLineFlags flags = CommandLineFlags.none) {
	auto metadata = createMetadata!T;
	T instance = metadata.createInstance().get!T;
	foreach(value; usedValues(metadata)) {
		string name = getArgName(value);
		TypeMetadata paramTypeData = value.type.metadata;
		void setValue(string key, string arg) {
			Variant convertedVal = paramTypeData.coerceFrom(arg);
			value.setValue(instance, convertedVal);
		}
		size_t oldLength = args.length;
		getopt(args, config.caseInsensitive, config.passThrough, name, &setValue);
		if(args.length == oldLength && value.findAttribute!Required(false)) {
			handleError(flags, "The required parameter " 
				~ value.findAttribute!DisplayName(value.name) ~ " was missing.");
		}
	}

	StoredCommand[] commands;
	bool commandInvoked = false;
	foreach(method; getCommands(metadata)) {
		string name = getArgName(method);
		void storeCommand(string name, string arg) {
			commands ~= StoredCommand(method, arg);
		}
		void storeCommandWithoutValue(string name) {
			commands ~= StoredCommand(method, null);
		}
		if(method.parameters.length > 0)
			getopt(args, config.caseInsensitive, config.passThrough, name, &storeCommand);
		else
			getopt(args, config.caseInsensitive, config.passThrough, name, &storeCommandWithoutValue);
	}
	if(commands.length > 1) {
		string[] nonMultiNames = commands.map!(c=>c.metadata)
			.filter!(c=>(c.findAttribute!Command.flags & CommandFlags.allowMulti) == 0)
			.map!(c=>c.name).array();
		if(nonMultiNames.length > 0) {
			handleError(flags, "The commands " ~ nonMultiNames.text ~ " may not be invoked "
				~ "with any other commands.");
		}
	}
	if(commands.length == 0 && (flags & CommandLineFlags.allowNoCommand) == 0) {
		MethodMetadata[] defaultMethods = getCommands(metadata)
			.filter!(c=>c.findAttribute!Command.flags & CommandFlags.setDefault)
			.array();
		if(defaultMethods.length > 1)
			throw new NotSupportedException("Multiple default methods are not supported.");
		if(defaultMethods.length == 1)
			commands = [StoredCommand(defaultMethods[0], null)];
		else {
			handleError(flags, 
				"You must specify a command.",
			    "Valid commands are:",
			    "\t" ~ getCommandText(metadata).replace("\n", "\n\t")
			);
		}
	}
	foreach(comm; commands) {
		if((comm.metadata.findAttribute!Command.flags & CommandFlags.argRequired) != 0 && comm.arg.strip.length == 0)
			handleError(flags, "The " ~ comm.metadata.findAttribute!DisplayName(comm.metadata.name) ~ " command requires an argument to be passed in.");
	}
	foreach(initializer; getInitializers(metadata))
		initializer.invoke(instance);
	foreach(comm; commands) {
		invokeCommand(instance, comm);
	}
	return instance;
}

private void handleError(CommandLineFlags flags, string[] desc...) {
	conPrint(flags, desc);
	throw new CommandLineException(join(desc, newline));
}


// Conditionally print if flags allows it.
private void conPrint(CommandLineFlags flags, string[] lines...) {
	if((flags & CommandLineFlags.noWrite) == 0) {
		foreach(line; lines)
			writeln(line);
	}
}

private void invokeCommand(T)(T instance, StoredCommand command) {
	Variant[] params = new Variant[command.metadata.parameters.length];
	if(params.length > 1)
		throw new NotSupportedException("Commands with multiple parameters are not supported.");
	if(params.length == 1) {
		TypeMetadata typeData = command.metadata.parameters[0].type.metadata;
		Variant parsed = typeData.coerceFrom(command.arg);
		params[0] = parsed;
	}
	try {
		Variant result = command.metadata.invoke(instance, params);
		if(result.type != typeid(void))
			writeln(result);
	} catch (ValidationException e) {
		writeln(e.msg);
	}
}

private struct StoredCommand {
	MethodMetadata metadata;
	string arg;
	this(MethodMetadata metadata, string arg) {
		this.arg = arg;
		this.metadata = metadata;
	}
}

private string getArgName(T)(T instance) {
	string name = instance.findAttribute!DisplayName(instance.name);
	dchar shortName = instance.findAttribute!ShortName(dchar.init);
	if(shortName != dchar.init)
		name ~= "|" ~ shortName.text;
	return name;
}

/// Returns a multi-line formatted help string that can be displayed to the user for what values are allowed for this type.
string getHelpString(T)() {
	auto metadata = createMetadata!T;
	T defaultInstance = metadata.createInstance().get!T;
	string optionText = "";
	string result = "";
	foreach(value; usedValues(metadata)) {
		string line = getDescriptionString(value);
		if(value.findAttribute!Required(false))
			line ~= " (Required)";
		else
			line ~= " Default: " ~ value.getValue(defaultInstance).text;
		optionText ~= line ~ "\r\n";
	}
	string commandText = getCommandText(metadata);
	result ~= getFormattedSection("Commands", commandText);
	result ~= getFormattedSection("Options", optionText);
	return result.stripRight;
}

private string getFormattedSection(string title, string sectionText) {
	if(sectionText.length > 0) {
		string result = title ~ ':' ~ newline ~ '\t' ~ sectionText.replace("\n", "\n\t");
		if(result[$-1] == '\t')
			result = result[0..$-1];
		return result;
	}
	return null;
}

private string getCommandText(TypeMetadata metadata) {
	string commandText = "";
	foreach(comm; getCommands(metadata)) {
		string line = getDescriptionString(comm);
		commandText ~= line ~ "\r\n";
	}
	return commandText;
}

private string getDescriptionString(T)(T metadata) {
	string displayName = getDisplayName(metadata);
	string description = metadata.findAttribute!Description("");
	string result = "[" ~ displayName ~ "]: " ~ description;
	return result;
}

private string getDisplayName(T)(T metadata) {
	string displayName = "--" ~ metadata.findAttribute!DisplayName(metadata.name);
	dchar shortName = metadata.findAttribute!ShortName(dchar.init);
	if(shortName != dchar.init)
		displayName = "-" ~ shortName.text ~ "|" ~ displayName;
	return displayName;
}

private auto getInitializers(TypeMetadata metadata) {
	return metadata.children.methods.filter!(c=>c.findAttribute!CommandInitializer.value);
}

private auto usedValues(TypeMetadata metadata) {
	return metadata.children.values.filter!(c=>!c.findAttribute!Ignore.value && c.protection == ProtectionLevel.public_);
}

private auto getCommands(TypeMetadata metadata) {
	return metadata.children.methods.filter!(c=>c.findAttribute!Command.value);
}

/// Indicates whether the method that this attribute is applied to is a command.
/// Commands are invoked upon finding an argument with the given name or DisplayName.
/// Commands with a return value will have that value printed to the user.
/// Commands may optionally take in a single parameter containing the value passed in for the command.
struct Command {
	bool value;
	CommandFlags flags;

	this(bool val = true) {
		this.value = val;
		this.flags = CommandFlags.none;
	}

	this(CommandFlags flags, bool val = true) {
		this.flags = flags;
		this.value = val;
	}
}

/// Provides additional options for a Command.
enum CommandFlags {
	/// No options are set.
	none = 0,
	/// This command may be invoked with other commands that have allowMulti set.
	allowMulti = 1,
	/// This command should be invoked with no arguments if no other command is specified.
	/// Only a single default command is set.
	setDefault = 2,
	/// Indicates that an argument is required for this command.
	argRequired = 4
}

/// Indicates that the given letter can be used as an alternative to this argument name.
struct ShortName {
	dchar letter;
}

/// Indicates that this method should be invoked after options are set but before commands are executed.
/// This is used for initializing any data passed through as options in preparation for a command.
/// The method this is applied to must have zero arguments and return void.
struct CommandInitializer {
	bool value;
	this(bool value = true) {
		this.value = value;
	}
}