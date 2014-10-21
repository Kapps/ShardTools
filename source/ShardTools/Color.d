/// Represents a Color in RGBA format, with one byte per channel.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Color;

version(Have_shardmath)
	version=ShardMath;

version(ShardMath) {
	private import ShardMath.Vector;
}

/// Represents a Color in RGBA format, with one byte per channel.
/// This struct is simple with no overhead, but lacks more complex operations.
/// The first byte is R, followed by G, B, then A.
struct Color {
	
	/**
	 * Instantiates a new instance of the Color struct.
	 * Params:
	 *	r = The red component for this color.
	 *	b = The blue component for this color.
	 *	g = The green component for this color.
	 *	a = The alpha component for this color.
	 */
	this(ubyte r, ubyte g, ubyte b, ubyte a = 255) {
		this.r = r;
		this.g = g;
		this.b = b;
		this.a = a;
	}

	// While this would be nice, making EVERY SINGLE library that imports ShardTools be forced to define ShardMath is just stupid.
	// Would be nice if only ShardMath had to define it...
	version(ShardMath) {
		
		/// Creates a new color from the given vector.
		///	Params:
		///		vector = The vector, with components ranging from 0 to 1, to create the Color from, where X is Red, and W is Alpha.
		this(Vector4f vector) {
			assert(vector.x <= 1 && vector.y <= 1 && vector.z <= 1 && vector.w <= 1 && vector.x >= 0 && vector.y >= 0 && vector.z >= 0 && vector.w >= 0);
			this.r = cast(ubyte)(vector.x * 255);
			this.g = cast(ubyte)(vector.y * 255);
			this.b = cast(ubyte)(vector.z * 255);
			this.a = cast(ubyte)(vector.w * 255);
		}
		
		/// Ditto
		this(Vector3f vector) {
			assert(vector.x <= 1 && vector.y <= 1 && vector.z <= 1 && vector.x >= 0 && vector.y >= 0 && vector.z >= 0);
			this.r = cast(ubyte)(vector.x * 255);
			this.g = cast(ubyte)(vector.y * 255);
			this.b = cast(ubyte)(vector.z * 255);
			this.a = 255;
		}
		
		/// Returns a Vector representation of this object, with components ranging from zero to one, where X is Red and W is Alpha.
		Vector3f toVector3() const {
			return Vector3f(r / 255f, g / 255f, b / 255f);
		}
		
		/// Ditto
		Vector4f toVector4() const {
			return Vector4f(r / 255f, g / 255f, b / 255F, a / 255f);
		}
		
	}
	
	/// Returns a pre-defined Color with this name.
	@property static Color aqua() {
		return Color(0, 255, 255);
	}
	
	/// Ditto
	@property static Color fuschia() {
		return Color(0, 255, 0, 255);
	}
	
	/// Ditto
	@property static Color black() {
		return Color(0, 0, 0, 255);
	}
	
	/// Ditto
	@property static Color blue() {
		return Color(0, 0, 255, 255);
	}
	
	/// Ditto
	@property static Color red() {
		return Color(255, 0, 0, 255);
	}
		
	/// Ditto
	@property static Color gray() {
		return Color(128, 128, 128);
	}
	
	/// Ditto
	@property static Color transparentBlack() {
		return Color(0, 0, 0, 0);
	}
	
	/// Ditto
	@property static Color lime() {
		return Color(0, 255, 0);
	}
	
	/// Ditto
	@property static Color maroon() {
		return Color(128, 0, 0);
	}
	
	/// Ditto
	@property static Color navy() {
		return Color(0, 0, 128);
	}
	
	/// Ditto
	@property static Color olive() {
		return Color(128, 128, 0);
	}
	
	/// Ditto
	@property static Color purple() {
		return Color(128, 0, 128);
	}
	
	/// Ditto
	@property static Color silver() {
		return Color(192, 192, 192);
	}
	
	/// Ditto
	@property static Color teal() {
		return Color(0, 128, 128);
	}
	
	/// Ditto
	@property static Color white() {
		return Color(255, 255, 255);
	}
	
	/// Ditto
	@property static Color yellow() {
		return Color(255, 255, 0);
	}

	/// Returns a pre-defined Color with this name.
	/// Note that the color returned by this does not have a Green component value
	///	of 255, but of 128 instead. Lime has a Green component of 255.	
	@property static Color green() {
		return Color(0, 128, 0);
	}
	
align(1):		
	/// The Blue component for this color.
	ubyte r;
	/// The Green component for this color.
	ubyte g;					
	/// The Red component for this color.
	ubyte b;
	/// The Alpha component for this color.
	ubyte a;
}