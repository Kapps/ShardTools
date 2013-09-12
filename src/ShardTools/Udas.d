module ShardTools.Udas;

/+/// Indicates that a struct is a user-defined attribute and should not be used otherwise.
/// This attribute has no effect and is only for documentation purposes.
struct Uda { }+/

/// Provides a user defined attribute that can give a default description for a field.
struct Description {
	string value;

	alias value this;
}

/// Provides an alternative friendly name for the field to display.
struct DisplayName {
	string value;

	alias value this;
}

/// Indicates that this field should be ignored or not displayed to the user.
struct Ignore { 
	bool value;
	this(bool value = true) {
		this.value = value;
	}

	alias value this;
}

/// Indicates that this is a required field.
struct Required {
	bool value;
	this(bool value = true) {
		this.value = value;
	}

	alias value this;
}