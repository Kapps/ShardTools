/// $(RED This module is not yet implemented.)
module ShardTools.Property;
import std.traits;

// TODO: Observable property.

/+
/// Provides information on how to generate a property.
enum PropertyFlags {
	/// Default: The property will not have any change events generated, and will have a get and set accessor.
	ReadWrite = 0,
	ReadOnly = 1,
	WriteOnly = 2,
	GenerateChangeEvents = 4,
	RecursiveChangeEvents = 8,
	CustomAccessor = 16
}

/// Represents a property capable of readonly, writeonly, or read-write access, optionally with change events and custom implementations.
/// For a basic read-write property, one would simply do something like 'Property!int width'.
/// For a read-only property, 'Property!(int, PropertyFlags.ReadOnly) width' would be used.
/// For a read-write property with change tracking, 'Property!(int, PropertyFlags.GenerateChangeEvents) width' would be used.
/// For a custom implementation of a read-only property, 'Property!(int, PropertyFlags.ReadOnly, PropertyFlags.CustomAccessor, () => this.size.width) width' would be used.
struct Property(T, Flags = PropertyFlags.ReadWrite, U... CustomAccessors) {
	
	/// Creates a Property with an implicit backing variable initialized to InitValue.
	static typeof(this) createBacked(InitValue = T.init) {
		static if(is(T == class)) {
			this.Pointer = new T*();
			*T = null;
		} else {
			this.Pointer = new T();
		}
	}
	
	this(T* Backing) {
		this.Pointer = Backing;
	}
	
	static if(Flags & PropertyFlags.ReadWrite)
		alias TypeTuple!(T delegate(), void delegate(T)) ConstructorArgs;
	else static if(Flags & PropertyFlags.ReadOnly)
		alias T delegate() ConstructorArgs;
	else static if(Flags & PropertyFlags.WriteOnly)
		alias void delegate(T) ConstructorArgs;
	else
		static assert(0, "Unknown PropertyFlags.");
	
	alias T PropertyType;
	
	
	alias get this;
	
	/// Returns the value of this property.
	T get() {
		static assert((Flags & PropertyFlags.WriteOnly) == 0, "Unable to read a write-only property.");
	}
	/// Sets the value of this property to the given value.
	void set(T value) {
	
	}
	
	auto opDispatch(string name, T...)(T args) {
		alias identifier =  __traits(getMember, name);
		static if(isProperty!T) {
			
		}
	}
	
	private T* Pointer; /// The pointer to the value to read or write.
}


+/