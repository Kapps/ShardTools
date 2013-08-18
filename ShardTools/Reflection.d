module ShardTools.Reflection;
version=logreflect;
import ShardTools.ExceptionTools;
import std.range;
import std.variant;
import std.traits;
import std.algorithm;
import std.array;
import std.typetuple;
import std.conv;
version(logreflect) import std.stdio;
import core.stdc.stdlib;
import std.typecons;
import core.memory;
import core.stdc.string;
import std.exception;
import std.string;

/// Indicates the protection level of a symbol, such as private or public.
enum ProtectionLevel {
	private_,
	protected_,
	package_,
	public_,
	export_
}

/// Indicates whther a value refers to a field or a property.
enum DataKind {
	field,
	property,
	constant
}

/// Indicates whether an instance of TypeMetadata provides information for a struct, class, union, interface, or enum.
/// Also includes a primitive type, for built in types (int, long, etc) as well as pointers and arrays.
enum TypeKind {
	struct_, 
	class_, 
	union_, 
	enum_,
	interface_,
	primitive_
}

alias Variant function(Variant instance) DataGetterFunction;
alias void function(Variant instance, Variant value) DataSetterFunction;
alias Variant function(MethodMetadata, void*, Variant[] args) MethodInvokeFunction;

mixin(MakeException("ReflectionException"));
mixin(MakeException("InvalidParameterException", "The arguments passed in to invoke a method were invalid."));

// TODO: Make all metadata immutable (except for invoke of course).
// This includes symbols.
// They already are after being loaded, it's just not typed properly.
// Actually even invoke could be immutable, just not pure.

// TODO: name and protection can be either a mixin or in a separate Symbol struct.
// We repeat them far too much. Note that parameters have names but not protection levels.

/// Provides information about a module and the symbols it contains.
struct ModuleMetadata {

	this(string name, string packageName, SymbolContainer children) {
		this._name = name;
		this._packageName = packageName;
		this._children = children;
	}

	/// Gets the name of this module, excluding the package.
	@property string name() const pure nothrow {
		return _name;
	}

	/// Gets the name of this package. This can be null if this module has no package.
	@property string packageName() const pure nothrow {
		return _packageName;
	}

	/// Returns the qualified name of this module (that is, the package name combined with the module name).
	@property string qualifiedName() const pure nothrow {
		return packageName.length == 0 ? name : (packageName ~ "." ~ name);
	}

	/// Gets the symbols that this module contains.
	@property SymbolContainer children() {
		return _children;
	}

	string toString() const pure nothrow {
		return qualifiedName;
	}

private:
	string _name;
	string _packageName;
	SymbolContainer _children;
	// TODO: Find a way to get ModuleInfo and store that, and store imported modules that way.
}

/// Stores information about a type (class, union, struct, enum, interface).
struct TypeMetadata {

	this(string name, ProtectionLevel protection, size_t instanceSize, TypeInfo type, TypeKind kind, SymbolContainer children, TypeInfo base, TypeInfo[] interfaces) {
		this._name = name;
		this._base = base;
		this._interfaces = interfaces;
		this._children = children;
		this._kind = kind;
		this._type = type;
		this._instanceSize = instanceSize;
		this._protection = protection;
	}

	/// Returns the protection level of this symbol.
	@property ProtectionLevel protection() const pure nothrow {
		return _protection;
	}

	/// Gets the size of a single instance of this type.
	/// This would be the class instance size for classes, 0 for interfaces, and sizeof for everything else.
	@property size_t instanceSize() const pure nothrow {
		return _instanceSize;
	}

	/// Gets the underlying TypeInfo for this aggregate.
	@property const(TypeInfo) type() const pure nothrow {
		return _type;
	}

	/// Indicates what type this metadata applies to; either a struct, class, enum, interface, or union.
	@property TypeKind kind() const pure nothrow {
		return _kind;
	}

	/// Gets the name of this type.
	/// This can be null for anonymous types.
	@property string name() const pure nothrow {
		return _name;
	}

	/// Returns the base class for this type.
	/// If this type has no base type (Object) or can not have a base type (structs / unions / interfaces), null is returned.
	/// Enums will return the default int base if no base type is specified, otherwise they will return that base type.
	/// Classes will include Object as a base type.
	@property const(TypeInfo) base() const pure nothrow {
		return _base;
	}

	/// Returns all of the interfaces that this type directly or indirectly implements.
	/// At the moment this only applies to classes and interfaces.
	@property const(TypeInfo[]) interfaces() const pure nothrow {
		return _interfaces;
	}

	/// Gets the child symbols that this type contains.
	@property SymbolContainer children() {
		return _children;
	}

	string toString() const {
		return protection.text[0..$-1] ~ " " ~ kind.text[0..$-1] ~ " " ~ name;
	}

private:
	TypeInfo _base;
	TypeInfo[] _interfaces;
	TypeInfo _type;
	size_t _instanceSize;
	string _name;
	SymbolContainer _children;
	TypeKind _kind;
	ProtectionLevel _protection;
}

/// Provides a means of storing symbols within a type or module.
struct SymbolContainer {

	// TODO: Don't duplicate; get a real non-transitive read-only return type instead.

	this(TypeInfo[] types, MethodMetadata[] methods, ValueMetadata[] values) {
		this._types = types;
		this._methods = methods;
		this._values = values;
	}

	/// Returns all of the types that this symbol contains.
	@property const(TypeInfo[]) types() const {
		return _types;
	}

	/// Returns a copy of all of the methods that this symbol contains.
	/// This does not include properties, as those are considered values.
	@property MethodMetadata[] methods() {
		return _methods.dup;
	}

	/// Returns a copy of all of the fields and properties that this symbol contains.
	@property ValueMetadata[] values() {
		return _values.dup;
	}

private:
	TypeInfo[] _types;
	MethodMetadata[] _methods;
	ValueMetadata[] _values;
}

/// Represents a single parameter in a method call.
struct ParameterMetadata {

	this(TypeInfo type, string name, bool hasDefaultValue, Variant defaultValue, ParameterStorageClass modifiers) {
		this._type = type;
		this._name = name;
		this._hasDefaultValue = hasDefaultValue;
		this._defaultValue = defaultValue;
		this._modifiers = modifiers;
	}

	/// Gets the type of this parameter.
	@property const(TypeInfo) type() const pure nothrow {
		return _type;
	}

	/// Gets the name of this parameter.
	/// This can be null for unnamed parameters.
	@property string name() const pure nothrow {
		return _name;
	}

	/// Indicates if this parameter has a default value available.
	@property bool hasDefaultValue() const pure nothrow {
		return _hasDefaultValue;
	}

	/// Gets the default value of this parameter.
	/// The result is undefined if $(D, hasDefaultValue) is false.
	@property const(Variant) defaultValue() const pure nothrow {
		return _defaultValue;
	}

	/// Gets the attributes for this parameter.
	@property ParameterStorageClass modifiers() const pure nothrow {
		return _modifiers;
	}

	string toString() const {
		string result = modifiers == ParameterStorageClass.none ? "" : (modifiers.text ~ " ");
		result ~= type.text ~ (name == null ? "" : (" " ~ name));
		if(hasDefaultValue)
			result ~= " = " ~ defaultValue.text;
		return result;
	}

private:
	TypeInfo _type;
	Variant _defaultValue;
	ParameterStorageClass _modifiers;
	bool _hasDefaultValue;
	string _name;
}

/// Provides metadata for a method, including return type, parameters, and invocation.
struct MethodMetadata {
	
	this(string name, ProtectionLevel protection, MethodInvokeFunction invoker, ParameterMetadata[] parameters, 
			TypeInfo returnType, size_t vtblSlot) {
		this._name = name;
		this._invoker = invoker;
		this._parameters = parameters;
		this._returnType = returnType;
		this._protection = protection;
		this._vtblSlot = vtblSlot;
	}

	/// Gets the index of this function within the vtbl of the type containing this method.
	/// For an interface this is the index within the interface, which the type has a pointer to.
	/// This value is 0 for for non-virtual methods (including final methods overriding non-final children).
	@property size_t vtblSlot() const pure nothrow {
		return _vtblSlot;
	}

	/// Gets the protection level for this method, such as private or public.
	@property ProtectionLevel protection() const pure nothrow {
		return _protection;
	}

	/// Gets the name of this method.
	@property string name() const pure nothrow {
		return _name;
	}

	/// Gets the return type of this method.
	@property const(TypeInfo) returnType() const pure nothrow {
		return _returnType;
	}

	/// Gets the parameters that are used as arguments to this method.
	/// At the moment varadic arguments are not supported.
	@property const(ParameterMetadata[]) parameters() const pure nothrow {
		return _parameters;
	}

	/// Invokes this method with the given arguments.
	/// If the method can not be invoked with the given arguments, an exception is thrown.
	/// In the case of static methods, instance may be null; otherwise instance must not be null.
	/// For static methods the value of instance is ignored.
	/// Bugs:
	/// 	Abstract and interface methods will throw a NotImplementedError.
	/// 	Virtual methods will have only the base method called, not the overriden one.
	Variant invoke(InstanceType, T...)(InstanceType instance, T arguments) {
		Variant[] args = new Variant[arguments.length];
		foreach(i, arg; arguments)
			args[i] = arg;
		void* instancePtr;
		static if(is(InstanceType == class) || is(InstanceType == interface)) {
			instancePtr = cast(void*)instance;
		} else {
			// TODO: Find a better way to check if variant.
			static if(is(InstanceType == Variant)) {
				// TODO: Add support for this.
			}
			instancePtr = cast(void*)&instance;
		}
		return _invoker(this, instancePtr, args);
	}

	string toString() const {
		string result = protection.text[0..$-1] ~ " " ~ returnType.text ~ " " ~ name ~ "(";
		foreach(ref param; _parameters) {
			result ~= param.text ~ ", ";
		}
		if(_parameters.length > 0)
			result = result[0..$-2];
		result ~= ")";
		return result;
	}

private:
	MethodInvokeFunction _invoker;
	ParameterMetadata[] _parameters;
	TypeInfo _returnType;
	string _name;
	ProtectionLevel _protection;
	size_t _vtblSlot;
	void* _functionPtr;
}

/// Provides metadata for fields or properties.
/// Even if a property has more than one setter, it will still be represented by a single instance of ValueMetadata.
/// The setter used will then be dependent on the type passed in to setValue.
struct ValueMetadata {
	
	this(string name, ProtectionLevel protection, DataKind kind, TypeInfo type, DataGetterFunction getter, DataSetterFunction setter) {
		this._name = name;
		this._type = type;
		this._getter = getter;
		this._setter = setter;
		this._kind = kind;
		this._protection = protection;
	}

	/// Gets the protection level for this value, such as private or public.
	@property ProtectionLevel protection() const pure nothrow {
		return _protection;
	}

	/// Indicates whether this value is a field or a property.
	@property DataKind kind() const pure nothrow {
		return _kind;
	}

	/// Gets the name of this member.
	@property string name() const pure nothrow {
		return _name;
	}

	/// Gets the type of this member.
	@property const(TypeInfo) type() const pure nothrow {
		return _type;
	}

	/// Indicates whether this value can be changed dynamically.
	/// This would return true for fields, readable properties, and user defined attributes.
	@property bool canGet() const pure nothrow {
		return _getter !is null;
	}
	
	/// Indicates whether this value can be changed dynamically.
	/// This would return true for fields and properties with setters, and false for read-only properties and user defined attributes.
	/// For enum values, this is always false.
	@property bool canSet() const pure nothrow {
		return _getter !is null;
	}

	/// Returns the value that the given instance has for this member.
	/// If the value is static or an enum value, instance is ignored and may be null.
	Variant getValue(InstanceType)(InstanceType instance) {
		if(!canGet)
			throw new NotSupportedException("Attempted to get a value on a member that did not support it.");
		Variant result = _getter(Variant(instance));
		return result;
	}

	/// Sets this member on the specified instance to the given value.
	/// If the value is static, instance should be null.
	void setValue(InstanceType, ValueType)(InstanceType instance, ValueType value) {
		if(!canSet)
			throw new NotSupportedException("Attempted to set a value on a member that did not support it.");
		_setter(Variant(instance), Variant(value));
	}

	string toString() const {
		return format("%s %s %s%s", kind == DataKind.constant ? "enum" : protection.text[0..$-1], type.text, name, kind == DataKind.property ? "()" : "");
	}

private:
	TypeInfo _type;
	DataGetterFunction _getter;
	DataSetterFunction _setter;
	string _name;
	DataKind _kind;
	ProtectionLevel _protection;
}

/// Returns metadata for the given object, if it's registered.
/// If it's not registered, it will attempt to be generated if possible.
/// Otherwise, TypeMetadata.init is returned.
/// If a variant is passed in, metadata for the type it contains will be returned instead.
/// If a variant is passed in that refers to a type that does not have metadata, init will be returned.
/// If T refers to a class and instance is a class derived from T for which reflection information
/// is not available, reflection data for type T will be returned instead.
TypeMetadata metadata(T)(T instance) {
	TypeInfo typeInfo;
	static if(is(T == Variant)) {
		// TODO: Figure out how to check if an actual variant and not just the alias.
		//version(logreflect) writeln("Passed in variant, using ", instance.type.text, " instead.");
		typeInfo = instance.type;
	} else static if(is(T : TypeInfo)) {
		typeInfo = instance;
	} else {
		static if(is(T == interface))
			typeInfo = (cast(Object)instance).classinfo;
		else
			typeInfo = typeid(instance);
		version(logreflect) writeln("Got type info for ", typeInfo, " from ", instance);
	}
	// TODO: Figure out a way to lock this; can't just use typeInfo because synchronized(typeid(int)) segfaults.
	synchronized(typeid(TypeMetadata)) {
		TypeMetadata existing = getStoredExisting(typeInfo, false);
		if(existing != TypeMetadata.init)
			return existing;
		static if(!is(T == Variant) && !is(T : TypeInfo)) {
			version(logreflect) writeln("No metadata for " ~ typeInfo.text ~ "; creating and returning metadata for " ~ typeid(Unqual!T).text ~ ".");
			return createMetadata!T();
		} else {
			static if(is(T == Variant)) {
				version(logreflect) writeln("Got variant of unknown type " ~ instance.type.text ~ "; returning init.");
			} else {
				version(logreflect) writeln("Got TypeInfo of unknown type " ~ typeInfo.text ~ "; returning init.");
			}
			return TypeMetadata.init;
		}
	}
}

/// Generates reflection data for the given class, struct, union, or enum.
/// Types that are referred to directly by the given type will be lazily loaded upon being accessed.
TypeMetadata createMetadata(T)() {
	// TODO: Decide how to handle this synchronization.
	// typeid(T) does make sense sort of, but could lead to nasty deadlocks and is a not ideal choice.
	// Yet we want a way to lock each individual type for when we generate metadata for it...
	// And rather not have to create our own Mutex each time, and worry about storing those etc.
	// Perhaps we can take advantage of knowing we're a template, and use a static variable.
	// We could also just not bother and risk race conditions generating reflection data multiple times.
	// It probably wouldn't be a big deal since everything is structs and the AA value would be replaced.
	// Synchronizing typeid(TypeMetadata) is definitely a bad idea; need a RWMutex or Spinlock for that for sure.

	// For now, I doubt the concurrency is going to be too important so just lock a single thing (RWMutex it).
	//synchronized(typeid(T)) {
	synchronized(typeid(TypeMetadata)) {
		TypeMetadata existing = getStoredExisting(typeid(Unqual!T), true);
		if(existing != TypeMetadata.init)
			return existing;
		static if(isPrimitive!T && !is(T == enum)) {
			string name = typeid(Unqual!T).text;
			SymbolContainer symbols = SymbolContainer.init;
		} else {
			string name = __traits(identifier, Unqual!T);
			SymbolContainer symbols = getSymbols!T;
		}
		TypeKind kind = getTypeKind!T;
		TypeInfo type = typeid(Unqual!T);
		TypeInfo base = getBase!T;
		ProtectionLevel protection = getProtection!T;
		size_t instanceSize = getSize!T;
		TypeInfo[] interfaces = getInterfaces!T;
		TypeMetadata result = TypeMetadata(name, protection, instanceSize, type, kind, symbols, base, interfaces);
		//synchronized(typeid(TypeMetadata)) {
		_typeData[typeid(Unqual!T)] = StoredTypeMetadata(result);
		//}
		return result;
	}
}

/// Register a loader that can get type metadata for this instance upon demand.
/// Returns the TypeInfo for the type that was registered.
TypeInfo registerLazyLoader(T)() {
	// TODO: Figure out a way to lock this; can't just use typeInfo because synchronized(typeid(int)) segfaults.
	// On second thought, forget it, I highly doubt anything is going to care that much about concurrency here.
	// It's an optimization for later if need be.
	// A RWMutex is important though.
	//synchronized(typeid(T)) {
	synchronized(typeid(TypeMetadata)) {
		if(typeid(Unqual!T) !in _typeData) {
			version(logreflect) writeln("Generating lazy loader for ", T.stringof, ".");
			_typeData[typeid(Unqual!T)] = StoredTypeMetadata(&createMetadata!T);
		}
	}
	//}
	return typeid(Unqual!T);
}

/// Returns the first method with the specified name on the given instance metadata.
/// The instance must be an instance of either ModuleMetadata or TypeMetadata.
/// Only methods that can be invoked with the paramTypes are returned (with an empty array for no arguments).
/// At the moment this is flawed and only returns parameters that exactly match the given type.
/// Variadic arguments are not yet supported either.
/// If no method with that name and set of arguments exists, init is returned.
MethodMetadata findMethod(T)(T instance, string methodName, TypeInfo[] paramTypes...) if(is(T == TypeMetadata) || is(T == ModuleMetadata)) {
	// TODO: Consider optimizing this.
	foreach(ref data; instance.children._methods) {
		if(data.name == methodName) {
			if(data.parameters.length != paramTypes.length)
				continue;
			bool falseParam = false;
			foreach(i, param; data.parameters) {
				if(param.type != paramTypes[i]) {
					falseParam = true;
					break;
				}
			}
			if(!falseParam)
				return data;
		}
	}
	version(logreflect) writeln("Did not find method with overloads of ", paramTypes, ".");
	return MethodMetadata.init;
}

/// Finds the first method with the given type params, as subject to $(D, findMethod).
/// Afterwards invokes the method.
/// This is merely a shortcut to prevent having to pass in methods multiple times.
/// If no method was found that can be invoked with the given exact arguments, a ReflectionException is thrown.
Variant invokeMethod(T, InstanceType, ArgTypes...)(T metadata, string methodName, InstanceType instance, ArgTypes args) {
	TypeInfo[] paramTypes = templateArgsToTypeInfo!ArgTypes;
	MethodMetadata method = findMethod(metadata, methodName, paramTypes);
	if(method == MethodMetadata.init)
		throw new ReflectionException("Unable to find a method that can be invoked with these arguments.");
	return method.invoke(instance, args);
}

/// Creates a new instance of a type given metadata and arguments to pass in to the constructor.
Variant createInstance(ArgTypes...)(TypeMetadata metadata, ArgTypes args) {
	if(metadata.kind == TypeKind.interface_)
		throw new ReflectionException("Unable to create an instance of an interface.");
	if(metadata.kind != TypeKind.class_)
		throw new NotImplementedError("Only creating instances of classes is supported at the moment.");
	TypeInfo[] argTypes = templateArgsToTypeInfo!(ArgTypes);
	MethodMetadata method = findMethod(metadata, "__ctor", argTypes);
	if(method == MethodMetadata.init) {
		if(metadata.children._methods.any!(c=>c.name == "__ctor") && ArgTypes.length == 0) {
			ClassInfo ci = cast(ClassInfo)metadata.type;
			Object instance = ci.create();
			return Variant(instance);
		} else
			throw new ReflectionException("No constructor found that matches the arguments passed in.");
	} else {
		ubyte[] data = cast(ubyte[])GC.malloc(metadata.instanceSize)[0 .. metadata.instanceSize];
		ClassInfo ci = cast(ClassInfo)metadata.type;
		enforce(data.length == ci.init.length);
		memcpy(data.ptr, ci.init[].ptr, ci.init.length);
		Object result = cast(Object)(cast(void*)data.ptr);
		auto retResult = method.invoke(result, args);
		assert(result == retResult.coerce!Object);
		return retResult;
	}
}

private SymbolContainer getSymbols(alias T)() {
	SymbolContainer result;
	foreach(m; __traits(allMembers, T)) {
		// Fields we can handle non-public members for.
		// Not sure yet for methods or types. Probably could types, doubt methods.
		static if(is(T == enum)) {
			ValueMetadata value = getEnumValue!(T, m);
			result._values ~= value;
		} else static if(hasField!(T, m)) {
			ValueMetadata value = getField!(T, m);
			result._values ~= value;
		} else static if(__traits(compiles, __traits(getMember, T, m))) {
			//version(logreflect) writeln("Processing member ", m, " on ", T.stringof, ".");
			alias aliasSelf!(__traits(getMember, T, m)) member;
			static if(isType!member) {
				result._types ~= registerLazyLoader!(typeof(member));
				version(logreflect) writeln("Added ", m, " as a type with metadata available to be loaded upon demand.");
			} else static if(__traits(getOverloads, T, m).length > 0) {
				foreach(func; __traits(getOverloads, T, m)) {
					auto method = getMethod!(func, T);
					result._methods ~= method;
					//version(logreflect) writeln("Added ", fullyQualifiedName!func, " overload.");
				}
			} else {
				version(logreflect) writeln("Skipped unknown member ", m, " on ", T.stringof, ".");
			}
		} else {
			version(logreflect) writeln("Skipped member ", m, " on ", T.stringof, " because it was not accessible.");
		}
	}
	return result;
}

private bool hasField(T, string m)() {
	static if(fieldIndex!(T, m) != -1)
		return true;
	else static if(is(T== class) && BaseClassesTuple!T.length > 0)
		return hasField!(BaseClassesTuple!T[0], m);
	else
		return false;
}


private ValueMetadata getEnumValue(T, string m)() {
	enum dataKind = DataKind.constant;
	enum protection = getProtection!T;
	string name = m;
	TypeInfo type = typeid(T);
	DataGetterFunction getter = &getEnumConstant!(T, m);
	DataSetterFunction setter = null;
	return ValueMetadata(name, protection, dataKind, type, getter, setter);
}

private ValueMetadata getField(T, string m)() {
	enum dataKind = DataKind.field;
	string name = m;
	static if(fieldIndex!(T, m) == -1)
		return getField!(BaseClassesTuple!T[0], m);
	else {
		TypeInfo type = typeid(typeof(T.tupleof[fieldIndex!(T, m)]));
		DataGetterFunction getter = &getFieldValue!(T, fieldIndex!(T, m));
		DataSetterFunction setter = &setFieldValue!(T, fieldIndex!(T, m));
		// Get odd errors when using getProtection, so have to do it ourselves...
		string protString = __traits(getProtection, __traits(getMember, T, m));
		ProtectionLevel protection = to!ProtectionLevel(protString ~ "_");
		return ValueMetadata(name, protection, dataKind, type, getter, setter);
	}
}

private MethodMetadata getMethod(alias func, T)() {
	string functionName = __traits(identifier, func);
	ParameterMetadata[] params = getParameters!(func);
	// Can't just set MPK inside static if due to ICE.
	static if(is(T == class))
		auto invoker = &(invokeMethod!(func, T, MethodParentKind.class_));
	else static if(is(T == interface))
		auto invoker = &(invokeMethod!(func, T, MethodParentKind.interface_));
	else {
		static assert(!__traits(isVirtualMethod, func), "Expected virtual method to have it's parent be either a class or interface.");
		auto invoker = &(invokeMethod!(func, T, MethodParentKind.unknown));
	}
	TypeInfo returnType = registerLazyLoader!(ReturnType!func);
	ProtectionLevel protection = getProtection!func;
	static if(__traits(isVirtualMethod, func)) {
		size_t vtblSlot = __traits(getVirtualIndex, T, func);
	} else {
		size_t vtblSlot = 0;
	}

	return MethodMetadata(functionName, protection, invoker, params, returnType, vtblSlot);
}

private ParameterMetadata[] getParameters(alias func)() {
	ParameterMetadata[] params;
	foreach(paramIndex, paramName; ParameterIdentifierTuple!(func)) {
		TypeInfo paramType = registerLazyLoader!(ParameterTypeTuple!(func)[paramIndex]);
		static if(is(ParameterDefaultValueTuple!(func)[paramIndex] == void)) {
			bool hasDefaultValue = false;
			Variant defaultVal = null;
		} else {
			Variant defaultVal = Variant(ParameterDefaultValueTuple!(func)[paramIndex]);
			bool hasDefaultVal = true;
		}
		ParameterStorageClass storageClass = cast(ParameterStorageClass)ParameterStorageClassTuple!(func)[paramIndex];
		ParameterMetadata paramData = ParameterMetadata(paramType, paramName, hasDefaultValue, defaultVal, storageClass);
		params ~= paramData;
	}
	return params;
}

private TypeInfo getBase(T)() {
	static if(is(T == class)) {
		// BaseClassesTuple seems to break on Object?
		static if(is(Unqual!T == Object) || BaseClassesTuple!(T).length == 0)
			return null;
		else
			return registerLazyLoader!(BaseClassesTuple!(T)[0]);
	} else static if(is(T == enum)) {
		return registerLazyLoader!(OriginalType!T);
	} else
		return null;
}

private TypeKind getTypeKind(T)() {
	static if(is(T == class))
		return TypeKind.class_;
	else static if(is(T == struct))
		return TypeKind.struct_;
	else static if(is(T == union))
		return TypeKind.union_;
	else static if(is(T == enum))
		return TypeKind.enum_;
	else static if(is(T == interface))
		return TypeKind.interface_;
	else static if(isBuiltinType!T || is(Unqual!T == void) || isPointer!T)
		return TypeKind.primitive_;
	else
		static assert(0, "Unknown type passed in.");
}

private TypeInfo[] getInterfaces(T)() {
	static if(is(T == class) || is(T == interface)) {
		TypeInfo[] result = new TypeInfo[InterfacesTuple!T.length];
		foreach(i, type; InterfacesTuple!T) {
			result[i] = registerLazyLoader!type;
		}
		return result;
	} else
		return null;
}

private ProtectionLevel getProtection(T)() {
	static if(isPrimitive!T) {
		return ProtectionLevel.export_;
	} else {
		enum stringVal = __traits(getProtection, T);
		return to!ProtectionLevel(stringVal ~ "_");
	}
}

private ProtectionLevel getProtection(alias T)() {
	enum stringVal = __traits(getProtection, T);
	return to!ProtectionLevel(stringVal ~ "_");
}

private size_t getSize(T)() {
	static if(is(T == class))
		return __traits(classInstanceSize, T);
	else
		return T.sizeof;
}

private Variant invokeMethod(alias func, T, MethodParentKind parentType)(MethodMetadata metadata, void* instance, Variant[] args) {
	// TODO: Look at staticMap to do this.
	static if(arity!func > 0)
		Unqual!(ParameterTypeTuple!(func)) params;
	else
		alias TypeTuple!() params;
	static if(arity!func == 1)
		params = args[0].get!(typeof(params));
	else static if(arity!func > 0) {
		foreach(i, type; ParameterTypeTuple!(func)) {
			params[i] = args[i].get!type;
		}
	}
	// TODO: Clean up return type stuffs.
	static if(__traits(isStaticFunction, func)) {
		static if(!is(ReturnType!func == void))
			auto result = func(params);
		else
			func(params);
	} else {
		ReturnType!func delegate(ParameterTypeTuple!func) dg;
		static if(__traits(isVirtualMethod, func)) {
			// If this is a virtual method we'll have to handle dispatching it manually.
			enforce(metadata.vtblSlot > 0, "Attempting to call a virtual function with no vtable index computable.");
			size_t thisOffset;
			void* funcPtr = getVirtualFunctionPointer!(parentType)(instance, typeid(T), metadata.vtblSlot, thisOffset);
		} else {
			void* funcPtr = cast(void*)&func;
			size_t thisOffset = 0;
		}
		dg.funcptr = cast(ReturnType!func function(ParameterTypeTuple!func))(funcPtr);
		dg.ptr = cast(void*)instance + thisOffset;
		static if(!is(ReturnType!func == void))
			auto result = dg(params);
		else
			dg(params);
	}
	static if(is(ReturnType!func == void))
		return Variant(null);
	else // Cast away from const to prevent Variant assignment issues. Rather sketchy.
		return Variant(cast()result);
}

private void* getVirtualFunctionPointer(MethodParentKind parentType)(void* instance, TypeInfo ti, size_t vtblSlot, out size_t thisOffset) {
	// If we're operating on a virtual function, we have to worry about vtables and such.
	auto obj = cast(Object)instance;
	if(obj is null)
		throw new ReflectionException("Virtual methods may only be invoked on an instance that is an Object.");
	ClassInfo ci = obj.classinfo;
	void*[] vtbl;
	// For classes, this is trivial; just get the slot returned by __traits(getVirtualIndex) in it's vtable.
	static if(parentType == MethodParentKind.class_) {
		enforce(vtbl.length > vtblSlot);
		thisOffset = 0;
		return ci.vtbl[vtblSlot];
	} else static if(parentType == MethodParentKind.interface_) {
		// For interfaces this is a fair bit more complex.
		// First, we have to find the right instance of Interface which stores the vtbl for that ClassInfo.
		// That instance may not be on the ClassInfo that instance is, but rather a base class.
		// When we find it, we can get a pointer to that class' function using the same approach as for classes.
		// Unfortunately this won't handle overrides.
		// So we then find the index in that class' vtbl that matches the function pointer for the interface
		// Since it's the same class that implemented it, this is safe.
		// If we find one, then we call that function in the derived class using that as the new vtbl slot.
		// If we don't, it wasn't overriden.
		// Note that we need to adjust the thisPtr if calling an interface function (but not if using it for derived vtbl).
		TypeInfo_Interface typeInterface = cast(TypeInfo_Interface)ti;
		// TODO: This should be optimized.
		// The current implementation is rather horrendously slow I'd imagine.
		// Can just cache it.
		// I'm sure there's a more proper way to do this though.
		for(ClassInfo curr = ci; curr; curr = curr.base) {
			foreach(inter; curr.interfaces) {
				if(inter.classinfo == typeInterface.info) {
					enforce(inter.vtbl.length > vtblSlot);
					auto interPtr = inter.vtbl[vtblSlot];
					if(curr == ci) {
						// The type of instance implements the interface, so we can use it's instance vptr directly.
						thisOffset = inter.offset;
						return interPtr;
					} else {
						// TODO: This approach is actually wrong.
						// Not sure what the correct approach is though...
						// Need to somehow find the function in the type's vtbl, but no clue how to do that.
						throw new NotImplementedError("Calling functions from interface metadata on an instance which derives from the type that implements the interface is not yet supported.");
						// Otherwise, find the vtbl index.
						/+writeln("Trying to figure out vtbl slot for ", interPtr, ".");
						foreach(i, ptr; curr.vtbl) {
							writeln(ptr, " = ", ptr - inter.offset, " = ", ptr + inter.offset);
							if(ptr == interPtr - inter.offset) {
								writeln("Found match at ", i);
								size_t derivedVtblSlot = i;
								enforce(ci.vtbl.length > derivedVtblSlot);
								return ci.vtbl[derivedVtblSlot];
							}
						}
						// If not found, then not virtual and so can call directly.
						thisOffset = inter.offset;
						return interPtr;+/
					}
				}
			}
		}
		throw new ReflectionException("Unable to find the vtable to invoke this method.");
	} else {
		static assert(0, "Expected non-virtual method when not a class or interface parent.");
	}
}

private enum MethodParentKind {
	unknown,
	interface_,
	class_
}

private Variant getFieldValue(InstanceType, size_t fieldIndex)(Variant instanceWrapper) {
	InstanceType instance = instanceWrapper.get!InstanceType;
	auto result = instance.tupleof[fieldIndex];
	return Variant(result);
}

private void setFieldValue(InstanceType, size_t fieldIndex)(Variant instanceWrapper, Variant valueWrapper) {
	InstanceType instance = instanceWrapper.get!InstanceType;
	instance.tupleof[fieldIndex] = valueWrapper.get!(typeof(instance.tupleof[fieldIndex]));
}

private Variant getEnumConstant(T, string m)(Variant instance) {
	// NOTE: cast not to, in order to prevent to!string(enum) which is wrong if base is string.
	return Variant(cast(OriginalType!T)__traits(getMember, T, m));
}

private TypeInfo[] templateArgsToTypeInfo(ArgTypes...)() {
	TypeInfo[] paramTypes = new TypeInfo[ArgTypes.length];
	foreach(i, argType; ArgTypes)
		paramTypes[i] = typeid(argType);
	return paramTypes;
}

private __gshared StoredTypeMetadata[TypeInfo] _typeData; // Should always use unqualified typeinfo.
private __gshared StoredModuleMetadata[string] _moduleData;

private struct StoredTypeMetadata {
	TypeMetadata function() loader;
	TypeMetadata metadata;
	bool isLoaded;

	void load() {
		assert(!isLoaded);
		this.metadata = loader();
		this.loader = null;
		this.isLoaded = true;
	}

	this(TypeMetadata function() loader) {
		this.loader = loader;
		this.metadata = TypeMetadata.init;
		this.isLoaded = false;
	}

	this(TypeMetadata metadata) {
		this.metadata = metadata;
		this.isLoaded = true;
		this.loader = null;
	}
}

private struct StoredModuleMetadata {
	ModuleMetadata delegate() loader;
	string moduleName;
}

// If metadata for the given type info exists, it will be lazily loaded and returned; otherwise init.
private TypeMetadata getStoredExisting(TypeInfo typeInfo, bool skipLoad) {
	if(auto res = typeInfo in _typeData) {
		if(!res.isLoaded) {
			if(skipLoad)
				return TypeMetadata.init;
			version(logreflect) writeln("Metadata for " ~ typeInfo.text ~ " was stored but not loaded; loading now.");
			res.load();
		}
		return res.metadata;
	}
	return TypeMetadata.init;
}

// Helper template to allow us to alias __traits(getMember, T, m).
private template aliasSelf(alias T) {
	alias T aliasSelf;
}

private template isPrimitive(T) {
	enum isPrimitive = (isBuiltinType!T || isArray!T || isPointer!T);
}

private template isType(T) {
	enum isType = is(T == class) || is(T == struct) || is(T == union) || is(T == enum) || is(T == interface);
}

private template isType(alias T) {
	enum isType = false;
}

private template isField(alias T) {
	enum isField = hasField!(__traits(parent, T), __traits(identifier, T));
}

// hasField (fieldIndex) and hasFieldImpl (fieldIndexImpl) credits to Jacob Carlborg.
// Simply changed a bit to alias to an index instead so we can use it with tupleof.
private template fieldIndex(T, string field) {
	static if(is(T == interface))
		enum fieldIndex = -1;
	else {
		enum fieldIndex = fieldIndexImpl!(T, field, 0);
	}
}

private template fieldIndexImpl (T, string field, size_t i) {
	static if (T.tupleof.length == i)
		enum fieldIndexImpl = -1;
	else static if(T.tupleof[i].stringof == field) // Something changed that seems to require this?
		enum fieldIndexImpl = i;
	//else static if (T.tupleof[i].stringof[1 + T.stringof.length + 2 .. $] == field)
	//	enum fieldIndexImpl = i;
	else
		enum fieldIndexImpl = fieldIndexImpl!(T, field, i + 1);
}
