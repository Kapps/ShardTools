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
	 *	R = The red component for this color.
	 *	B = The blue component for this color.
	 *	G = The green component for this color.
	 *	A = The alpha component for this color.
	 */
	this(ubyte R, ubyte G, ubyte B, ubyte A = 255) {
		this.R = R;
		this.G = G;
		this.B = B;
		this.A = A;
	}

	// While this would be nice, making EVERY SINGLE library that imports ShardTools be forced to define ShardMath is just stupid.
	// Would be nice if only ShardMath had to define it...
	version(ShardMath) {
		
		/// Initializes a new instance of the Color struct.
		///	Params:
		///		Vector = The vector, with components ranging from 0 to 1, to create the Color from, where X is Red, and W is Alpha.
		this(Vector4f Vector) {
			//assert(Vector.X <= 1 && Vector.Y <= 1 && Vector.Z <= 1 && Vector.W <= 1 && Vector.X >= 0 && Vector.Y >= 0 && Vector.Z >= 0 && Vector.W >= 0);
			this.R = cast(ubyte)(Vector.X * 255);
			this.G = cast(ubyte)(Vector.Y * 255);
			this.B = cast(ubyte)(Vector.Z * 255);
			this.A = cast(ubyte)(Vector.W * 255);
		}
		
		/// Initializes a new instance of the Color struct.
		///	Params:
		///		Vector = The vector, with components ranging from 0 to 1, to create the Color from, where X is Red, and Z is Blue.
		this(Vector3f Vector) {
			//assert(Vector.X <= 1 && Vector.Y <= 1 && Vector.Z <= 1 && Vector.X >= 0 && Vector.Y >= 0 && Vector.Z >= 0);
			this.R = cast(ubyte)(Vector.X * 255);
			this.G = cast(ubyte)(Vector.Y * 255);
			this.B = cast(ubyte)(Vector.Z * 255);
			this.A = 255;
		}
		
		/// Returns a Vector2 representation of this object, with components ranging from zero to one, where X is Red and W is Alpha.
		Vector3f ToVector3() const {
			return Vector3f(R / 255f, G / 255f, B / 255f);
		}
		
		/// Returns a Vector4 representation of this object, with components ranging from zero to one, where X is Red and W is Alpha.
		Vector4f ToVector4() const {
			return Vector4f(R / 255f, G / 255f, B / 255F, A / 255f);
		}
		
	}
	
	/// Returns a pre-defined Color with this name.
	@property static Color Aqua() {
		return Color(0, 255, 255);
	}
	
	/// Returns a pre-defined Color with this name.
	@property static Color Fuschia() {
		return Color(0, 255, 0, 255);
	}
	
	/// Returns a pre-defined Color with this name.
	@property static Color Black() {
		return Color(0, 0, 0, 255);
	}
	
	/// Returns a pre-defined Color with this name.
	@property static Color Blue() {
		return Color(0, 0, 255, 255);
	}
	
	/// Returns a pre-defined Color with this name.
	@property static Color Red() {
		return Color(255, 0, 0, 255);
	}
	
	/// Returns a pre-defined Color with this name.
	/// Note that the color returned by this does not have a Green component value
	///	of 255, but of 128 instead. Lime has a Green component of 255.	
	@property static Color Green() {
		return Color(0, 128, 0);
	}
	
	/// Returns a pre-defined Color with this name.
	@property static Color Gray() {
		return Color(128, 128, 128);
	}
	
	/// Returns a pre-defined Color with this name.
	@property static Color TransparentBlack() {
		return Color(0, 0, 0, 0);
	}
	
	/// Returns a pre-defined Color with this name.	
	@property static Color Lime() {
		return Color(0, 255, 0);
	}
	
	/// Returns a pre-defined Color with this name.	
	@property static Color Maroon() {
		return Color(128, 0, 0);
	}
	
	/// Returns a pre-defined Color with this name.
	@property static Color Navy() {
		return Color(0, 0, 128);
	}
	
	/// Returns a pre-defined Color with this name.	
	@property static Color Olive() {
		return Color(128, 128, 0);
	}
	
	/// Returns a pre-defined Color with this name.	
	@property static Color Purple() {
		return Color(128, 0, 128);
	}
	
	/// Returns a pre-defined Color with this name.	
	@property static Color Silver() {
		return Color(192, 192, 192);
	}
	
	/// Returns a pre-defined Color with this name.	
	@property static Color Teal() {
		return Color(0, 128, 128);
	}
	
	/// Returns a pre-defined Color with this name.	
	@property static Color White() {
		return Color(255, 255, 255);
	}
	
	/// Returns a pre-defined Color with this name.	
	@property static Color Yellow() {
		return Color(255, 255, 0);
	}
	
align(1):		
	/// The Blue component for this color.
	ubyte R;
	/// The Green component for this color.
	ubyte G;					
	/// The Red component for this color.
	ubyte B;
	/// The Alpha component for this color.
	ubyte A;
}