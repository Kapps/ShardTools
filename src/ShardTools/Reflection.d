/// Provides runtime reflection data for D symbols, such as classes or modules.
/// All reflection data is generated at compile-time, and thus metadata must be specified first.
/// Once a symbol has metadata generated for it, any symbols it can access will have a lazy
/// loader set to generate metadata for those symbols.
/// For example, a method returning an instance of Foo will result in a lazy loader
/// being created to generate metadata for Foo.
/**
	Example:
	---
	class Foo {
		int _val;
		this(int val) {
			this._val = val;
		}

		@property int val() const {
			return _val;
		}

		@property void val(int value) {
			_val = value;
		}

		int getSquare(int input) {
			return input * input;
		}
	}

	// First, we need to generate metadata for Foo.
	auto metadata = createMetadata!Foo;
	// Then we can create an instance.
	// Note that creating an instance returns a Variant, so the result must be converted.
	Variant varInstance = metadata.createInstance(3);
	Foo instance = varInstance.get!Foo;
	// Can easily get any property or field through getValue.
	assert(metadata.getValue("val", instance) == 3);
	// Since the type may not be known at compile-time, we can also operate on the Variant directly.
	// Note that in this scenario the metadata will have to be created through createMetadata prior to this.
	// We created it above with createMetadata!Foo, so we're fine.
	assert(metadata == varInstance.metadata);
	assert(metadata.getValue("val", varInstance) == 3);
	// Of course, can set values and invoke methods as well.
	metadata.setValue("val", instance, 6);
	assert(metadata.getValue("val", instance) == 6);
	assert(metadata.invokeMethod("getSquare", instance, 4) == 16);
	---
*/
module ShardTools.Reflection;
version=logreflect;
import ShardTools.ExceptionTools;
import std.range;
import std.variant;
import std.traits;
import std.algorithm;
import std.typetuple;
import std.conv;
import std.string;
version(logreflect) import std.stdio;
import core.stdc.string;
import core.memory;
import std.exception;

/// Indicates the protection level of a symbol, such as private or public.
enum ProtectionLevel {
	/// No protection level exists for this particular symbol.
	none_ = 0,
	/// The symbol is private, accessible only to the current module.
	private_,
	/// The symbol is protected, accessible to types derived from the given type.
	protected_,
	/// The symbol is accessible by any module within the same package.
	package_,
	/// The symbol is public, accessible by any module from the same library.
	public_,
	/// The symbol is exported across library boundaries.
	export_
}

/// Indicates whther a value refers to a field or a property.
enum DataKind {
	/// The data is a raw field.
	field,
	/// The data has a property backing it.
	property,
	/// The data is an enum constant.
	constant
}

/// Indicates whether an instance of TypeMetadata provides information for a struct, class, union, interface, or enum.
/// Also includes a primitive type for built in types (int, long, etc) as well as pointers and arrays.
enum TypeKind {
	///
	struct_, 
	///
	class_, 
	///
	union_, 
	///
	enum_,
	///
	interface_,
	///
	primitive_
}

/// Provides a bitwise enumeration of the modifiers that are applied to one more than one kind of symbol.
/// This includes storage classes and type qualifiers.
enum SymbolModifiers {
	/// 
	none_ = 0,
	///
	const_ = 1,
	///
	extern_ = 2,
	///
	immutable_ = 4,
	///
	scope_ = 8,
	///
	shared_ = 16,
	///
	static_ = 32,
	///
	synchronized_ = 64,
	///
	gshared_ = 128
	// No support for deprecated yet.
	/+///
	deprecated_ = 512,+/
}

alias Variant function(ValueMetadata metadata, Variant instance) DataGetterFunction;
alias void function(ValueMetadata metadata, Variant instance, Variant value) DataSetterFunction;
alias Variant function(MethodMetadata, void*, Variant[] args) MethodInvokeFunction;

mixin(MakeException("ReflectionException"));

// TODO: Make all metadata immutable (except for invoke of course).
// This includes symbols.
// They already are after being loaded, it's just not typed properly.
// Actually even invoke could be immutable as it doesn't modify anything related to the metadata, just not pure.

/// Provides information about the symbol that a metadata instance represents.
/// This includes features such as name, protection, and user-defined attributes.
struct Symbol {

	this(string name, ProtectionLevel protection, Variant[] attributes, SymbolModifiers modifiers) {
		this._name = name;
		this._protection = protection;
		this._attributes = attributes;
		this._modifiers = modifiers;
	}
	
	/// Returns the name of this symbol.
	/// In certain cases, such as anonymous structs or unnamed parameters, this may be null.
	@property string name() @safe const pure nothrow {
		return _name;
	}

	/// Returns the protection level for this symbol, such as public or private.
	/// Some symbols may not have a protection level, such as modules and parameters.
	/// In this case the value would be none.
	@property ProtectionLevel protection() @safe const pure nothrow {
		return _protection;
	}

	// TODO: Implement.
	/// Returns the modifiers that apply to this symbol.
	@disable @property SymbolModifiers modifiers() @safe const pure nothrow {
		return _modifiers;
	}

	//mixin(getIsFlagsMixin!(SymbolModifiers, "modifiers"));

	/// Returns a duplicate of the array containing any attributes that apply to this symbol.
	@property Variant[] attributes() pure {
		// TODO: Don't duplicate, use a readonly range instead.
		return _attributes.dup;
	}

	/// Indicates if this symbol has one or more attributes of the given type.
	/// Note that this must be the exact type of the attribute.
	bool hasAttribute(TypeInfo type) {
		foreach(ref attrib; _attributes) {
			if(attrib.type == type)
				return true;
		}
		return false;
	}

	/// Returns the last specified attribute that matches type T exactly.
	/// If no attribute is found matching that type, defaultValue is evaluated and returned.
	T findAttribute(T)(lazy T defaultValue = T.init) {
		foreach(ref attrib; retro(_attributes)) {
			if(attrib.type == typeid(T))
				return attrib.get!T;
		}
		return defaultValue();
	}
	
	private string _name;
	private ProtectionLevel _protection;
	private Variant[] _attributes;
	SymbolModifiers _modifiers;
}

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

	this(Symbol symbol, size_t instanceSize, TypeInfo type, TypeKind kind, SymbolContainer children, 
	     TypeInfo base, TypeInfo parent, TypeInfo[] interfaces) {

		this._symbol = symbol;
		this._base = base;
		this._interfaces = interfaces;
		this._children = children;
		this._kind = kind;
		this._type = type;
		this._instanceSize = instanceSize;
		this._parent = parent;
	} 

	alias symbol this;

	/// Gets the symbol representing the type this metadata refers to.
	@property Symbol symbol() pure nothrow {
		return _symbol;
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

	/// If this type is a nested type, returns the parent of the type.
	/// Otherwise, returns null.
	@property const(TypeInfo) parent() const pure nothrow {
		return _parent;
	}

	/// Gets the child symbols that this type contains.
	@property SymbolContainer children() {
		return _children;
	}

	string toString() {
		return symbol.protection.text[0..$-1] ~ " " ~ kind.text[0..$-1] ~ " " ~ symbol.name;
	}

private:
	TypeInfo _base;
	TypeInfo[] _interfaces;
	TypeInfo _type;
	TypeInfo _parent;
	size_t _instanceSize;
	SymbolContainer _children;
	TypeKind _kind;
	Symbol _symbol;
}

/// Provides a means of storing symbols within a type or module.
struct SymbolContainer {

	// TODO: Don't duplicate; get a real non-transitive read-only return type instead.

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
	/// Gets the name of this parameter, or null if this parameter is anonymous.
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

/// Provides metadata for a method (including a constructor), providing the return type, parameters, and invocation data.
struct MethodMetadata {
	
	this(Symbol symbol, MethodInvokeFunction invoker, ParameterMetadata[] parameters, TypeInfo returnType, size_t vtblSlot) {
		this._symbol = symbol;
		this._invoker = invoker;
		this._parameters = parameters;
		this._returnType = returnType;
		this._vtblSlot = vtblSlot;
	}

	alias symbol this;

	/// Gets the index of this function within the vtbl of the type containing this method.
	/// For an interface this is the index within the interface, which the type has a pointer to.
	/// This value is 0 for for non-virtual methods (including final methods overriding non-final children).
	@property size_t vtblSlot() const pure nothrow {
		return _vtblSlot;
	}

	/// Gets the symbol of the method this instance provides metadata for.
	@property Symbol symbol() pure nothrow {
		return _symbol;
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

	/// Finds the parameter within this method that has the given name, or init if not found.
	ParameterMetadata findParameter(string name) {
		ptrdiff_t index = _parameters.countUntil!"a.name == b"(name);
		return index == -1 ? ParameterMetadata.init : _parameters[index];
	}

	/// Invokes this method with the given arguments.
	/// If the method can not be invoked with the given arguments, an exception is thrown.
	/// In the case of static methods, instance may be null; otherwise instance must not be null.
	/// For static methods the value of instance is ignored.
	Variant invoke(InstanceType, T...)(InstanceType instance, T arguments) {
		// TODO: Check if pure, non-static, and struct. If so, throw because not passed by ref.
		// Maybe make version(logreflect) log it.
		return invokeInternal(&instance, arguments);
	}

	/// ditto
	Variant invoke(InstanceType, T...)(ref InstanceType instance, T arguments) if(is(T == struct)) {
		return invokeInternal(&instance, arguments);
	}

	private Variant invokeInternal(InstanceType, T...)(InstanceType* instancePtr, T arguments) {
		Variant[] args = new Variant[arguments.length];
		foreach(i, arg; arguments)
			args[i] = arg;
		// If it's a class, the context pointer should be the reference.
		// Otherwise if it's a struct, a pointer to the struct.
		static if(is(InstanceType == struct)) {
			static if(isVariant!InstanceType) {
				enum storeIndex = fieldIndex!(InstanceType, "store");
				void* contextPtr;
				ClassInfo ci = cast(ClassInfo)instancePtr.type;
				if(cast(ClassInfo)instancePtr.type)
					contextPtr = cast(void*)instancePtr.coerce!Object;
				else if(cast(TypeInfo_Interface)instancePtr.type) {
					// We need a way to get the pointer to the value stored within the Variant.
					// We can't just coerce to void* or Object, and we don't know the Interface type.
					// So we do the only reasonable thing and access the private store backing and hack around it.
					auto contents = instancePtr.tupleof[storeIndex][];
					void* interPtr = *cast(void**)contents.ptr;
					Interface* inter = **cast(Interface***)interPtr;
					contextPtr = interPtr - inter.offset;
				} else {
					if(instancePtr.type.tsize > instancePtr.size) {
						auto contents = instancePtr.tupleof[storeIndex][];
						contextPtr = *cast(void**)contents.ptr;
					} else
						contextPtr = cast(void*)instancePtr;
				}
			} else {
				void* contextPtr = cast(void*)instancePtr;
			}
		} else static if(is(InstanceType == interface)) {
			// If it's an interface, casting to void* is offset by the instance's offset value.
			void* contextPtr = cast(void*)cast(Object)*instancePtr;
		} else
			void* contextPtr = cast(void*)(*instancePtr);
		return _invoker(this, contextPtr, args);
	}

	string toString() {
		string result = symbol.protection.text[0..$-1] ~ " " ~ returnType.text ~ " " ~ symbol.name ~ "(";
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
	Symbol _symbol;
	size_t _vtblSlot;
	void* _functionPtr;
}

/// Provides metadata for fields or properties.
/// Even if a property has more than one setter, it will still be represented by a single instance of ValueMetadata.
/// The setter used will then be dependent on the type passed in to setValue.
struct ValueMetadata {
	
	this(T)(Symbol symbol, DataKind kind, TypeInfo type, DataGetterFunction getter, DataSetterFunction setter, T backingData) 
			if(is(T == FieldValueMetadata) || is(T == PropertyValueMetadata)) {
		this._symbol = symbol;
		this._type = type;
		this._getter = getter;
		this._setter = setter;
		this._kind = kind;
		static if(is(T == FieldValueMetadata))
			this._fieldData = backingData;
		else
			this._propertyData = backingData;
	}

	alias symbol this;

	/// Gets the symbol that this instance provides metadata for.
	@property Symbol symbol() pure nothrow {
		return _symbol;
	}

	/// Indicates whether this value is a field or a property.
	@property DataKind kind() const pure nothrow {
		return _kind;
	}

	/// Gets information about the field that this value references.
	/// If kind is not set to field or constant, a ReflectionException is thrown.
	@property FieldValueMetadata fieldData() {
		if(kind != DataKind.constant && kind != DataKind.field)
			throw new ReflectionException("Unable to get the field data for a value that is not a field or constant.");
		return _fieldData;
	}

	/// Gets information about the property that this value references.
	/// This includes both the getter method and setter methods for the value.
	/// Each method contained by this value contains the appropriate symbol.
	/// This allows you to get method-specific attributes or other such data.
	@property PropertyValueMetadata propertyData() {
		if(kind != DataKind.property)
			throw new ReflectionException("Unable to get the property data for a value that is not a property.");
		return _propertyData;
	}

	/// Gets the type of the value.
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
		Variant result = _getter(this, Variant(instance));
		return result;
	}

	/// Sets this member on the specified instance to the given value.
	/// If the value is static, instance should be init.
	void setValue(InstanceType, ValueType)(InstanceType instance, ValueType value) {
		// TODO: Need to so support checking for static.
		//if(is(InstanceType == struct) && !isStatic)
		//	throw new InvalidOperationException("A struct was passed by value to setValue to a non-static method, causing the operation to have no effect."); 
		enforceCanSet(instance);
		_setter(this, Variant(&instance), Variant(value));
	}

	/// ditto
	void setValue(InstanceType, ValueType)(ref InstanceType instance, ValueType value) if(is(InstanceType == struct)) {
		enforceCanSet(instance);
		_setter(this, Variant(&instance), Variant(value));
	}

	private void enforceCanSet(InstanceType)(ref InstanceType instance) {
		if(!canSet)
			throw new NotSupportedException("Attempted to set a value on a member that did not support it.");
		static if(isVariant!InstanceType) {
			// This isn't really possible.
			// While yes, we could make it work in this some cases (adjusting values on a Variant),
			// this would lead to things like getValue(instance).setValue("bar", 3) and expecting instance.bar to be changed.
			// In reality, a copy of instance.bar would be changed instead.
			if(cast(TypeInfo_Struct)instance.type)
				throw new NotSupportedException("Unable to set a value on a struct contained by a Variant as it would operate on a copy.");
		}
	}

	string toString() {
		string result = (kind == DataKind.constant ? "enum" : symbol.protection.text[0..$-1])
			~ " " ~ type.text ~ " " ~ symbol.name ~ (kind == DataKind.property ? "()" : "");
		return result;
	}

private:
	TypeInfo _type;
	DataGetterFunction _getter;
	DataSetterFunction _setter;
	DataKind _kind;
	Symbol _symbol;
	union {
		FieldValueMetadata _fieldData;
		PropertyValueMetadata _propertyData;
	}
}

/// Provides backing information for a field referenced by a ValueMetadata instance.
struct FieldValueMetadata {

	this(size_t index, size_t offset, TypeInfo declaringType) {
		this._index = index;
		this._offset = offset;
		this._declaringType = declaringType;
	}

	/// Gets the index of the field within the type containing it.
	/// Note that it is possible for two fields to have the same index in the case
	/// where a class derived from another class that implements the field.
	/// The index is guaranteed to be unique on type however.
	@property size_t index() const {
		return _index;
	}

	/// Gets the offset, in bytes, of the location of this field within the type.
	/// The D ABI specifies that this will be the same for classes deriving from type.
	@property size_t offset() const {
		return _offset;
	}

	/// Gets the type that declares this field.
	@property TypeInfo declaringType() {
		return _declaringType;
	}

private:
	size_t _index;
	size_t _offset;
	TypeInfo _declaringType;
}

/// Provides backing information for a property referenced by a ValueMetadata instance.
/// This includes the property's getter method and all of it's setter methods.
struct PropertyValueMetadata {

	this(MethodMetadata getter, MethodMetadata[] setters) {
		this._getter = getter;
		this._setters = setters;
	}

	/// Returns the method that's used to get the value of this property.
	/// A getter is defined as a method marked with @property that has zero parameters.
	@property MethodMetadata getter() {
		return _getter;
	}

	/// Returns a duplicate of the array that contains the setter methods for this property.
	/// A setter is defined as a method marked with @property that contains parameters.
	@property MethodMetadata[] setters() {
		// TODO: Again, a readonly array return-type that doesn't make each item const.
		return _setters.dup;
	}

	private MethodMetadata _getter;
	private MethodMetadata[] _setters;
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
	static if(isVariant!T) {
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
		static if(!isVariant!T && !is(T : TypeInfo)) {
			version(logreflect) writeln("No metadata for " ~ typeInfo.text ~ "; creating and returning metadata for " ~ typeid(Unqual!T).text ~ ".");
			return createMetadata!T();
		} else {
			static if(isVariant!T) {
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
	// Below pragma could be useful in situations where you want to know exactly what's being compiled in.
	version(debugreflect) pragma(msg, "Creating metadata for " ~ T.stringof);
	synchronized(typeid(TypeMetadata)) {
		TypeMetadata existing = getStoredExisting(typeid(Unqual!T), true);
		if(existing != TypeMetadata.init)
			return existing;
		Symbol symbol = getSymbol!T;
		static if(isPrimitive!T && !is(T == enum)) {
			SymbolContainer symbols = SymbolContainer.init;
			TypeInfo parent = null;
		} else {
			SymbolContainer symbols = getSymbols!T;
			static if(__traits(compiles, isAggregateType!(__traits(parent, T)))) {
				static if(isAggregateType!(__traits(parent, T)))
					TypeInfo parent = registerLazyLoader!(__traits(parent, T));
				else
					TypeInfo parent = null;
			} else
				TypeInfo parent = null;
		}
		TypeKind kind = getTypeKind!T;
		TypeInfo type = typeid(Unqual!T);
		TypeInfo base = getBase!T;
		size_t instanceSize = getSize!T;
		TypeInfo[] interfaces = getInterfaces!T;
		TypeMetadata result = TypeMetadata(symbol, instanceSize, type, kind, symbols, base, parent, interfaces);
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

/// Finds the type with the given name on the specified metadata.
/// If no value is found, init is returned.
TypeMetadata findType(T)(T instance, string name) {
	foreach(ref type; instance.children._types) {
		// HACK: We don't know the actual name of the type as TypeInfo does not store the relative name.
		// So instead we just take the absolute name that .text gives us and strip past the last dot.
		string typeName = type.text;
		size_t lastDot = typeName.retro.countUntil('.');
		if(lastDot != -1)
			typeName = typeName[$-lastDot..$];
		if(typeName == name)
			return type.metadata;
	}
	return TypeMetadata.init;
}

/// Returns the value on the given type that has the specified name.
/// This name is guaranteed to be unique on that type.
/// If no value is found, init is returned.
ValueMetadata findValue(T)(T instance, string name) {
	foreach(ref val; instance.children._values) {
		if(val.name == name)
			return val;
	}
	return ValueMetadata.init;
}

/// Returns the first method with the specified name on the given instance metadata.
/// The instance must be an instance of either ModuleMetadata or TypeMetadata.
/// Only methods that can be invoked with the paramTypes are returned (with an empty array for no arguments).
/// At the moment this is flawed and only returns parameters that exactly match the given type.
/// Variadic arguments are not yet supported either.
/// If no method with that name and set of arguments exists, init is returned.
MethodMetadata findMethod(T)(T metadata, string methodName, TypeInfo[] paramTypes...) if(is(T == TypeMetadata) || is(T == ModuleMetadata)) {
	return findMethodInternal(metadata.children._methods, methodName, paramTypes);
}

private MethodMetadata findMethodInternal(MethodMetadata[] methods, string methodName, TypeInfo[] paramTypes...) {
	// TODO: Consider optimizing this.
	foreach(ref data; methods) {
		if(data.name == methodName) {
			if(data.parameters.length != paramTypes.length)
				continue;
			bool falseParam = false;
			// TODO: Use _d_istypeof2 and/or _d_dynamic_cast to do this.
			// That way it can handle things like this for us.
			// Or else Variant.implicitConversionTargets.
			// Just use something to not have to force exact types.
			// Also fix qualified parameters (const, etc).
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
		throw new ReflectionException("Unable to find a method named " ~ methodName ~ " within " ~ metadata.name ~ " that can be invoked with these arguments.");
	return method.invoke(instance, args);
}

/// A shortcut to get or set the first value with the given name.
/// If no value exists with the given name on the specified metadata, a ReflectionException is thrown.
Variant getValue(T, InstanceType)(T metadata, string valueName, InstanceType instance) {
	ValueMetadata value = metadata.findValue(valueName);
	if(value == ValueMetadata.init)
		throw new ReflectionException("Unable to find a value named " ~ valueName ~ " within " ~ metadata.name ~ ".");
	return value.getValue(instance);
}

/// ditto
void setValue(T, InstanceType, ValueType)(T metadata, string valueName, InstanceType instance, ValueType value) {
	ValueMetadata valueData = metadata.findValue(valueName);
	if(valueData == ValueMetadata.init)
		throw new ReflectionException("Unable to find a value named " ~ valueName ~ " within " ~ metadata.name ~ ".");
	valueData.setValue(instance, value);
}

/// Creates a new instance of a type given metadata and arguments to pass in to the constructor.
/// Bugs:
/// 	Structs are not yet supported.
Variant createInstance(ArgTypes...)(TypeMetadata metadata, ArgTypes args) {
	if(metadata.kind != TypeKind.struct_ && metadata.kind != TypeKind.class_)
		throw new NotSupportedException("Only structs and classes may be instantiated through createInstance.");
	// TODO: We should be able to create instances of nested types if they're marked static.
	//if(metadata.parent !is null)
	//	throw new NotSupportedException("Unable to create instances of nested types.");
	TypeInfo[] argTypes = templateArgsToTypeInfo!(ArgTypes);
	MethodMetadata method = findMethod(metadata, "__ctor", argTypes);
	if(metadata.kind == TypeKind.struct_) {
		// TODO: Accessing m_init for some structs causes a segfault.
		// Figure out a work-around, or else fix this.
		// Until then, can't support structs.
		throw new NotSupportedException("Calling createInstance with a struct is not yet implemented.");
		//ubyte[] data = new ubyte[metada.instanceSize];

	} else {
		if(method == MethodMetadata.init) {
			if(ArgTypes.length == 0 && metadata.children._methods.any!(c=>c.name == "__ctor")) {
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
}

private Symbol getSymbol(Args...)() if(Args.length == 1) {
	alias T = Args[0];
	string name = getName!T;
	ProtectionLevel protection = getProtection!T;
	Variant[] attributes = getAttributes!T;
	SymbolModifiers modifiers = SymbolModifiers.none_;
	Symbol result = Symbol(name, protection, attributes, modifiers);
	return result;
}

private Variant[] getAttributes(Args...)() if(Args.length == 1) {
	alias T = Args[0];
	static if(!__traits(compiles, __traits(getAttributes, T))) {
		version(logreflect) writeln("Calling getAttributes on " ~ T.stringof ~ " does not compile. Returning null.");
		return null;
	} else {
		auto tup = __traits(getAttributes, T);
		return attributeTupleToArray(tup);
	}
}

private Variant[] attributeTupleToArray(T...)(T tup) {
	Variant[] result = new Variant[tup.length];
	foreach(index, attrib; tup) {
		registerLazyLoader!(typeof(attrib));
		result[index] = Variant(attrib);
	}
	return result;
}

private string getName(Args...)() if(Args.length == 1) {
	alias T = Args[0];
	static if(is(T[0]) && isPrimitive!T && !is(T == enum)) {
		return typeid(Unqual!T).text;
	} else static if(__traits(compiles, Unqual!T)) {
		return __traits(identifier, Unqual!T);
	} else {
		return __traits(identifier, T);
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
				result._types ~= registerLazyLoader!(member);
			} else static if(__traits(getOverloads, T, m).length > 0) {
				MethodMetadata propertyGetter;
				MethodMetadata[] propertySetters;
				foreach(func; __traits(getOverloads, T, m)) {
					auto method = getMethod!(func, T);
					static if(functionAttributes!func & FunctionAttribute.property) {
						// A getter is defined as a property that returns non-void and takes in zero parameters.
						if(arity!func == 0 && !is(ReturnType!func == void)) {
							// TODO: BUG: It is possible to have multiple getters.
							// For example, one const and one non-const.
							// Decide how this should be handled.
							// For the moment it's fine because we completely ignore const and such.
							// Obviously that's not a great solution going forward however.
							//assert(propertyGetter == MethodMetadata.init);
							propertyGetter = method;
						} else {
							// Otherwise, it's a setter.
							propertySetters ~= method;
						}
					} else {
						result._methods ~= method;
					}
				}
				if(propertyGetter != MethodMetadata.init || propertySetters.length > 0)
					result._values ~= getProperty!(T)(propertyGetter, propertySetters);
			} else {
				version(logreflect) writeln("Skipped unsupported member ", member.stringof, " on ", T.stringof, ".");
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
	// Enums can't have attributes or protection.
	Symbol symbol = Symbol(m, ProtectionLevel.none_, null, SymbolModifiers.immutable_);
	TypeInfo type = typeid(T);
	DataGetterFunction getter = &getEnumConstant!(T, m);
	DataSetterFunction setter = null;
	enum size_t index = fieldIndex!(T, m);
	enum size_t offset = index * T.sizeof;
	FieldValueMetadata fieldData = FieldValueMetadata(index, 0, typeid(T));
	return ValueMetadata(symbol, dataKind, type, getter, setter, fieldData);
}

private ValueMetadata getField(T, string m)() {
	enum dataKind = DataKind.field;
	enum index = fieldIndex!(T, m);
	static if(index == -1)
		return getField!(BaseClassesTuple!T[0], m);
	else {
		TypeInfo type = typeid(typeof(T.tupleof[index]));
		size_t offset = T.tupleof[index].offsetof; //__traits(getMember, T, m).offsetof;
		DataGetterFunction getter = &getFieldValue!(T, index);
		DataSetterFunction setter = &setFieldValue!(T, index);
		// Unfortunately have to duplicate this, couldn't get getSymbol to work with private fields.
		// Passing in the symbol causes it to try to be evaluated and has errors with static.
		string name = m;
		ProtectionLevel protection = to!ProtectionLevel(__traits(getProtection, __traits(getMember, T, m)) ~ "_");
		auto unparsedAttribs = __traits(getAttributes, T.tupleof[index]);
		Variant[] attributes = attributeTupleToArray(unparsedAttribs);
		SymbolModifiers modifiers = SymbolModifiers.none_;
		Symbol symbol = Symbol(name, protection, attributes, modifiers);
		TypeInfo declaringType = typeid(T);
		FieldValueMetadata fieldData = FieldValueMetadata(index, offset, declaringType);
		return ValueMetadata(symbol, dataKind, type, getter, setter, fieldData);
	}
}

private ValueMetadata getProperty(T)(MethodMetadata getterMethod, MethodMetadata[] setterMethods) {
	enum kind = DataKind.property;
	TypeInfo type = getterMethod._returnType;
	string name = getterMethod.symbol._name;
	ProtectionLevel protection = getterMethod.symbol._protection;
	// We default to the getter's name and protection, but no attributes.
	// Attributes should be gotten for each individual method.
	Symbol symbol = Symbol(name, protection, null, SymbolModifiers.none_);
	PropertyValueMetadata propertyData = PropertyValueMetadata(getterMethod, setterMethods);
	DataGetterFunction getter = &getPropertyValue!(T);
	DataSetterFunction setter = &setPropertyValue!(T);
	return ValueMetadata(symbol, kind, type, getter, setter, propertyData);
}

private MethodMetadata getMethod(alias func, T)() {
	Symbol symbol = getSymbol!func;
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
	static if(__traits(isVirtualMethod, func)) {
		static if(__traits(compiles, __traits(getVirtualIndex, func)))
			size_t vtblSlot = __traits(getVirtualIndex, func);
		else {
			pragma(msg, "Your compiler does not support __traits(getVirtualIndex). Invoking virtual methods will fail.");
			size_t vtblSlot = 0;
		}
	} else {
		size_t vtblSlot = 0;
	}

	return MethodMetadata(symbol, invoker, params, returnType, vtblSlot);
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
			bool hasDefaultValue = true;
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
	if(args.length != arity!func)
		throw new ReflectionException("Expected " ~ arity!func.text ~ " arguments to " ~ metadata.name ~ ", not " ~ args.length.text ~ ".");
	staticMap!(Unqual, ParameterTypeTuple!func) params;
	foreach(i, type; typeof(params)) {
		params[i] = args[i].get!type;
	}
	// TODO: Clean up return type stuffs.
	static if(__traits(isStaticFunction, func)) {
		static if(!is(ReturnType!func == void))
			auto result = func(params);
		else
			func(params);
	} else {
		// Unqual to prevent inout issues.
		Unqual!(ReturnType!func) delegate(ParameterTypeTuple!func) dg;
		static if(__traits(isVirtualMethod, func)) {
			// If this is a virtual method we'll have to handle dispatching it manually.
			enforce(metadata.vtblSlot > 0, "Attempting to call a virtual function with no vtable index computable.");
			size_t thisOffset;
			void* funcPtr = getVirtualFunctionPointer!(parentType)(instance, typeid(T), metadata.vtblSlot, thisOffset);
		} else {
			void* funcPtr = cast(void*)&func;
			size_t thisOffset = 0;
		}
		dg.funcptr = cast(Unqual!(ReturnType!func) function(ParameterTypeTuple!func))(funcPtr);
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
		vtbl = ci.vtbl;
		enforce(vtbl.length > vtblSlot && vtblSlot > 0);
		thisOffset = 0;
		return ci.vtbl[vtblSlot];
	} else static if(parentType == MethodParentKind.interface_) {
		// For interfaces this is a bit more complex.
		// First, we have to find the right instance of Interface which stores the vtbl for that ClassInfo.
		// That instance may not be on the ClassInfo that instance is, but rather a base class.
		// When we find it, we can get a pointer to that class' implementation using the same approach as for classes.
		// Aka, vtblSlot within the object.Interface instance's vtbl.
		// Unfortunately this won't handle overrides.
		// To handle overrides, we get the offset of the interface, and the void*** there is a pointer to our vtbl.
		// Also, interface context pointers are actually offset by the object.Interface.offset value.
		// So we have to handle that as well, and set thisOffset to that value.

		// TODO: We can optimize this easily.
		// Ultimately, we're only trying to find the Interface instance to get it's offset.
		// But if the object we're invoking the method on is an interface reference,
		// we can get the offset through that. At least, I assume we can. 
		// Not sure how Foo (Interface) -> Bar : Foo -> DerivedBar : Bar -> DerivedDerived : Foo, DerivedBar would work.
		// Really though, the cost should be negligible compared to the performance hit of using Variants and such.
		TypeInfo_Interface typeInterface = cast(TypeInfo_Interface)ti;
		for(ClassInfo curr = ci; curr; curr = curr.base) {
			foreach(inter; curr.interfaces) {
				if(inter.classinfo == typeInterface.info) {
					void*** vtblPtr = cast(void***)(instance + inter.offset);
					vtbl = (*vtblPtr)[0..inter.vtbl.length];
					enforce(vtbl.length > vtblSlot);
					auto interPtr = vtbl[vtblSlot];
					thisOffset = inter.offset;
					return interPtr;
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

private Variant getFieldValue(InstanceType, size_t fieldIndex)(ValueMetadata metadata, Variant instanceWrapper) {
	InstanceType instance = instanceWrapper.get!InstanceType;
	// TODO: We can likely reduce template bloat if we can make the below work at compile-time instead of fieldIndex.
	// For example, by manually making a switch statement that gets the right value.
	// That might create more bloat than it saves though, not sure...
	// For now, this works fine.
	//auto result = instance.tupleof[metadata.fieldData.index];
	auto result = instance.tupleof[fieldIndex];
	return Variant(result);
}

private void setFieldValue(InstanceType, size_t fieldIndex)(ValueMetadata metadata, Variant instanceWrapper, Variant valueWrapper) {
	// TODO: Same as for getFieldValue. Remove fieldIndex template param.
	//size_t fieldIndex = metadata.fieldData.index;
	InstanceType* instance = instanceWrapper.get!(InstanceType*);
	instance.tupleof[fieldIndex] = valueWrapper.get!(typeof(instance.tupleof[fieldIndex]));
}

private Variant getPropertyValue(InstanceType)(ValueMetadata metadata, Variant instanceWrapper) {
	auto method = metadata.propertyData.getter;
	if(method == MethodMetadata.init)
		throw new ReflectionException("No getter was found for this method.");
	return method.invoke!(InstanceType)(instanceWrapper.get!(InstanceType));
}

private void setPropertyValue(InstanceType)(ValueMetadata metadata, Variant instanceWrapper, Variant valueWrapper) {
	auto setters = metadata.propertyData.setters;
	auto method = findMethodInternal(setters, metadata.symbol.name, [valueWrapper.type]);
	if(method == MethodMetadata.init)
		throw new ReflectionException("Unable to find a setter that takes in a " ~ valueWrapper.type.text ~ ".");
	method.invokeInternal!(InstanceType)(instanceWrapper.get!(InstanceType*), valueWrapper);
}

private Variant getEnumConstant(T, string m)(ValueMetadata unused, Variant instance) {
	// NOTE: cast instead of to, in order to prevent to!string(enum) which is wrong if base is string.
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

private template isType(T...) if(T.length == 1) {
	enum isType = is(T[0] == class) || is(T[0] == struct) || is(T[0] == union) || is(T[0] == enum) || is(T[0] == interface);
}

private template isField(alias T) {
	enum isField = hasField!(__traits(parent, T), __traits(identifier, T));
}

private template isVariant(T) {
	// TODO: Need a real way of checking.
	enum isVariant = is(Unqual!T == Variant);
}

private template fieldIndex(T, string field) {
	static if(is(T == interface))
		enum fieldIndex = -1;
	else {
		enum fieldIndex = fieldIndexImpl!(T, field, 0);
	}
}

private template fieldIndexImpl(T, string field, size_t i) {
	static if(is(T == enum)) {
		static if(__traits(allMembers, T).length == i)
			enum fieldIndexImpl = -1;
		else static if(__traits(allMembers, T)[i] == field)
			enum fieldIndexImpl = i;
		else
			enum fieldIndexImpl = fieldIndexImpl!(T, field, i + 1);
	} else static if (T.tupleof.length == i)
		enum fieldIndexImpl = -1;
	else static if(T.tupleof[i].stringof == field)
		enum fieldIndexImpl = i;
	else
		enum fieldIndexImpl = fieldIndexImpl!(T, field, i + 1);
}

private string getIsFlagsMixin(T, string flagsName)() if(is(T == enum)) {
	string result = "";
	foreach(m; __traits(allMembers, T)) {
		string transformedName = capitalize(m);
		if(transformedName[$-1] == '_')
			transformedName = transformedName[0..$-1];
		result ~= "@property bool is" ~ transformedName ~ "() @safe const pure nothrow {";
		result ~= "\r\n\treturn (" ~ flagsName ~ " & " ~ T.stringof ~ "." ~ m ~ ") != 0;\r\n}";
	}
	return result;
}

version(unittest) {
	struct ReflectionTestStruct {
		string stringVal;

		@property string stringValProperty() const {
			return stringVal;
		}
	}

	struct ReflectionTestAttribute {
		int val;
		this(int val) {
			this.val = val;
		}
	}

	struct ReflectionLargeStructTester {
		ubyte[16] unusedPadding;
		ubyte[Variant.size] maxVariantPadding;
		int _val;

		this(int val) {
			_val = val;
		}

		@property int val() const {
			return _val;
		}

		@property void val(int val) {
			_val = val;
		}

		int returnDouble() const {
			return val * 2;
		}
	}

	struct ReflectionFloatStructTester {
		int a;
		float b;
	}

	interface ReflectionTestInterface {
		@property int val() const;
	}

	class ReflectionTestClass : ReflectionTestInterface {
		private int _val = 3;

		@property int val() const {
			return _val;
		}

		int foo(int x = 1) {
			return x;
		}

		void doubleRef(lazy scope int x) {
			int tmp = x();
			tmp *= 2;
		}

		static int staticMethod() {
			return 3;
		}

		this() {

		}

		this(int val) {
			this._val = val;
		}
	}

	class ReflectionDerivedClass : ReflectionTestClass {
		int derivedField;
		@property override int val() const {
			return super.val * 2;
		}

		int bar(int x) {
			return x * 2;
		}

		override int foo(scope int x = 2) {
			return x * 10;
		}

	}

	class ReflectionUdaTest {
		@ReflectionTestAttribute(3) int val;

		// TODO: This results in errors in std.traits.DefaultValueTuple when getting parameters for check.
		//@CheckerAttribute!(c)() int checkedVal;

		@(6) @property int valProp() const {
			return val;
		}
	}

	/+bool c(int val) {
		return val > 3;
	}

	struct CheckerAttribute(alias fun) { 
		bool check(int args) {
			return unaryFun!fun(args);
		}
	}+/

	enum ReflectionTestEnum {
		a, b, c
	}

	enum ReflectionTestBaseEnum : string {
		myVal = "mv",
		myVal2 = "mv2",
		myValDup = "mv"
	}

	class ReflectionTestNestedClass {
		int a;
		class Nested {
			int _b;
			this(int b) {
				this._b = b;
			}

			@property int b() const {
				return _b;
			}

			int returnDouble() const {
				return b * 2;
			}
		}
	}

	// Full functionality tests.

	// Basic struct tester:
	unittest {
		TypeMetadata metadata = createMetadata!ReflectionTestStruct;
		assert(metadata.kind == TypeKind.struct_);
		ReflectionTestStruct instance = ReflectionTestStruct();
		instance.stringVal = "abc";
		auto children = metadata.children;
		assert(children.types.length == 0);
		assert(children.methods.length == 0);
		assert(children.values.length == 2);
		ValueMetadata field = metadata.findValue("stringVal");
		assert(field.name == "stringVal");
		assert(field.protection == ProtectionLevel.public_);
		assert(field.attributes.length == 0);
		assert(field.getValue(instance) == "abc");
		assert(field.type == typeid(string));
		field.setValue(instance, "def");
		assert(field.getValue(instance) == "def");
		assert(field.getValue(instance).type == typeid(string));
		assert(instance.stringVal == "def");
		ValueMetadata prop = metadata.findValue("stringValProperty");
		assert(prop.getValue(instance) == "def");
		assert(prop.propertyData.getter.name == "stringValProperty");
		// TODO: Implement
		//auto floatTesterMeta = createMetadata!ReflectionFloatStructTester;
		//ReflectionFloatStructTester inst2 = floatTesterMeta.createInstance.get!ReflectionFloatStructTester;
		//assert(inst2.a == 0);
		//assert(isNaN(inst2.b));
	}

	// Basic class tester:
	unittest {
		TypeMetadata metadata = createMetadata!ReflectionTestClass;
		assert(metadata.base == typeid(Object));
		assert(metadata.kind == TypeKind.class_);
		assert(metadata.parent is null);
		ReflectionTestClass instance = new ReflectionTestClass();
		auto children = metadata.children;
		auto firstMethod = metadata.findMethod("foo", [typeid(int)]);
		assert(firstMethod != MethodMetadata.init);
		assert(firstMethod.name == "foo");
		assert(firstMethod.returnType == typeid(int));
		assert(firstMethod.invoke(instance, 3) == 3);
		assert(metadata.invokeMethod("foo", instance, 4) == 4);
		assert(metadata.findMethod("val") == MethodMetadata.init);
		ValueMetadata val = metadata.findValue("val");
		assert(val != MethodMetadata.init);
		assert(val.getValue(instance) == 3);
		auto newInst = metadata.createInstance(200).get!ReflectionTestClass;
		assert(newInst.val == 200);
		auto valBackingField = metadata.findValue("_val");
		assert(valBackingField.name == "_val");
		assert(valBackingField.protection == ProtectionLevel.private_);
		assert(valBackingField.fieldData.declaringType == typeid(ReflectionTestClass));
		MethodMetadata doubler = metadata.findMethod("doubleRef", typeid(int));
		assert(doubler.name == "doubleRef");
		assert(doubler.symbol.protection == ProtectionLevel.public_);
		assert(doubler.parameters.length == 1);
		assert(doubler.returnType == typeid(void));
		auto doublerParam = doubler.findParameter("x");
		assert(doublerParam.name == "x");
		assert(!doublerParam.hasDefaultValue);
		assert(doublerParam.modifiers == (ParameterStorageClass.lazy_ | ParameterStorageClass.scope_));
		MethodMetadata staticMethod = metadata.findMethod("staticMethod");
		assert(staticMethod.name == "staticMethod");
		assert(staticMethod.invoke(null) == 3);


		// Test derived classes:
		auto derivedData = createMetadata!ReflectionDerivedClass;
		Variant derived = derivedData.createInstance(); // Test as Variant.
		assert(derived.metadata == derivedData);
		assert(derivedData.parent is null);
		assert(derivedData.base == typeid(ReflectionTestClass));
		MethodMetadata fooData = derivedData.findMethod("foo", typeid(int));
		assert(fooData.name == "foo");
		assert(fooData.protection == ProtectionLevel.public_);
		assert(fooData.parameters.length == 1);
		ParameterMetadata param = fooData.findParameter("x");
		assert(param.name == "x");
		assert(param.hasDefaultValue);
		assert(param.defaultValue == 2);
		assert(param.modifiers & ParameterStorageClass.scope_);
		assert(param.type == typeid(int));
		assert(fooData.invoke(derived, 4) == 40);
		assert(derivedData.findMethod("foo", typeid(string)) == MethodMetadata.init);
		assert(derivedData.findMethod("foobar", typeid(string)) == MethodMetadata.init);
		ValueMetadata derivedField = derivedData.findValue("derivedField");
		assert(derivedField.name == "derivedField");
		assert(derivedField.kind == DataKind.field);
		auto derivedFieldData = derivedField.fieldData;
		assert(derivedFieldData.declaringType == typeid(ReflectionDerivedClass));
		assert(derivedFieldData.index == 0);
		ValueMetadata nonDerivedField = derivedData.findValue("_val");
		assert(nonDerivedField.name == "_val");
		assert(nonDerivedField.fieldData.declaringType == typeid(ReflectionTestClass));
		assert(metadata.findMethod("staticMethod") == staticMethod);
	}

	// Interface tests:
	unittest {
		TypeMetadata metadata = createMetadata!ReflectionTestInterface;
		ReflectionTestInterface instance = new ReflectionTestClass(10);
		auto val = metadata.findValue("val");
		assert(val.getValue(instance) == 10);
		auto clsMeta = createMetadata!ReflectionTestClass;
		assert(clsMeta.interfaces.length == 1);
		assert(clsMeta.interfaces[0] == typeid(ReflectionTestInterface));
		auto derived = new ReflectionDerivedClass();
		assert(val.getValue(derived) == 6);
	}

	// Enum tests:
	unittest {
		TypeMetadata metadata = createMetadata!ReflectionTestEnum;
		assert(metadata.name == "ReflectionTestEnum");
		assert(metadata.kind == TypeKind.enum_);
		assert(metadata.children.values.length == 3);
		ValueMetadata val = metadata.findValue("a");
		assert(val.fieldData.index == 0);
		assert(metadata.findValue("b").fieldData.index == 1);
		assert(metadata.findValue("c").fieldData.index == 2);
		assert(val.name == "a");
		assert(val.getValue(null) == 0);
		assert(metadata.getValue("b", null) == 1);
		assert(metadata.getValue("c", null) == 2);

		TypeMetadata baseData = createMetadata!ReflectionTestBaseEnum;
		assert(baseData.base == typeid(string));
		assert(baseData.getValue("myVal", null) == "mv");
		ValueMetadata myVal = baseData.findValue("myVal");
		ValueMetadata myValDup = baseData.findValue("myValDup");
		assert(myVal.getValue(null) == myValDup.getValue(null));
		assert(myVal.kind == DataKind.constant);
		assert(myVal.getValue(null) == "mv");
		assert(myVal.fieldData.index == 0);
		assert(myValDup.fieldData.index == 2);
	
	}

	// UDA tests
	unittest {
		auto metadata = createMetadata!ReflectionUdaTest;
		auto field = metadata.findValue("val");
		assert(field != ValueMetadata.init);
		assert(field.attributes.length == 1);
		assert(field.hasAttribute(typeid(ReflectionTestAttribute)));
		assert(!field.hasAttribute(typeid(int)));
		assert(field.findAttribute!int == 0);
		auto attr = field.findAttribute!ReflectionTestAttribute;
		assert(attr == ReflectionTestAttribute(3));
		auto prop = metadata.findValue("valProp");
		Symbol getterSymbol = prop.propertyData.getter;
		assert(getterSymbol.hasAttribute(typeid(int)));
		assert(getterSymbol.findAttribute!int == 6);
		/+auto checker = metadata.findValue("checkedVal").attributes[0];
		auto t = __traits(getAttributes, ReflectionUdaTest.checkedVal)[0];
		writeln(t.check(3));
		writeln("metadata = ", checker);
		writeln("type = ", checker.type);
		auto checkerData = checker.type.metadata;
		assert(checkerData.invokeMethod("check", checker, 4).get!bool);+/
	}

	// Nested type tests
	unittest {
		auto parentData = createMetadata!ReflectionTestNestedClass;
		assert(parentData.children.values.length == 1);
		assert(parentData.findValue("a") != ValueMetadata.init);
		assert(parentData.findValue("b") == ValueMetadata.init);
		assert(parentData.name == "ReflectionTestNestedClass");
		assert(parentData.children.types.length == 2);
		auto metadata = parentData.findType("Nested");
		assert(metadata.name == "Nested");
		assert(metadata.children.values.length == 2);
		auto instance = metadata.createInstance(5);
		assert(metadata.getValue("_b", instance) == 5);
		assert(metadata.invokeMethod("returnDouble", instance) == 10);
	}

	// Test various things with variants.
	unittest {
		auto structData = createMetadata!ReflectionTestStruct;
		ReflectionTestStruct s = ReflectionTestStruct("abc");
		Variant v = s;
		assert(v.metadata == structData);
		ValueMetadata stringVal = structData.findValue("stringVal");
		assert(stringVal != ValueMetadata.init);
		assert(stringVal.getValue(v) == "abc");

		auto derivedData = createMetadata!ReflectionDerivedClass;
		ReflectionTestInterface testInter = new ReflectionDerivedClass();
		Variant interVar = testInter;
		assert(derivedData.findMethod("foo", typeid(int)) != MethodMetadata.init);
		assert(derivedData.invokeMethod("foo", interVar, 5) == 50);
	
		auto largeData = createMetadata!ReflectionLargeStructTester;
		Variant largeVar = ReflectionLargeStructTester(4);
		assert(largeData == largeVar.metadata);
		assert(largeData.getValue("val", largeVar) == 4);
		assert(largeData.invokeMethod("returnDouble", largeVar) == 8);
	}

	// Test module header example.
	// Would be nice if documented unittests worked on module headers.
	unittest {
		class Foo {
			int _val;
			this(int val) {
				this._val = val;
			}
			
			@property int val() const {
				return _val;
			}
			
			@property void val(int value) {
				_val = value;
			}
			
			int getSquare(int input) {
				return input * input;
			}
		}
		
		// First, we need to generate metadata for Foo.
		auto metadata = createMetadata!Foo;
		// Then we can create an instance.
		// Note that creating an instance returns a Variant, so the result must be converted.
		Variant varInstance = metadata.createInstance(3);
		Foo instance = varInstance.get!Foo;
		// Can easily get any property or field through getValue.
		assert(metadata.getValue("val", instance) == 3);
		// Since the type may not be known at compile-time, we can also operate on the Variant directly.
		// Note that in this scenario the metadata will have to be created through createMetadata prior to this.
		// We created it above with createMetadata!Foo, so we're fine.
		assert(metadata == varInstance.metadata);
		assert(metadata.getValue("val", varInstance) == 3);
		// Of course, can set values and invoke methods as well.
		metadata.setValue("val", instance, 6);
		assert(metadata.getValue("val", instance) == 6);
		assert(metadata.invokeMethod("getSquare", instance, 4) == 16);
	}
}