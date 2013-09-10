module ShardTools.Noise;
private import std.math;
private import std.traits;
private import std.random;
private import std.array;

// TODO: This class just seems flat out wrong for a lot of things?

/// Provides generators for creating random noise, such as perlin noise.
/// $(RED The implementation of this class is poor, and thus is disabled until improved.)
@disable static class Noise  {
	
public:	
	
	private uint Next(ref Random rnd) {
		uint Result = rnd.front;
		rnd.popFront();
		return Result;
	}
	
	/// Generates random noise using the Diamond-Square Algorithm, usually used for fractal terrain.	
	/// Params:
	/// 	T = The type of the elements to generate.
	/// 	SizePower = The power of two to use for the size of the level. For example, 9 generates 513x513 noise (aka, 2^9 + 1).
	/// 	Range = Indicates how much the heights should vary.
	/// 	Seed = The random seed to use, or zero to randomly generate one using std.random.unpredictableSeed.
	/// 	CornerValue = The value of the initial four corner elements.
	/// Credits:
	///		Implementation ported and modified from http://stackoverflow.com/a/2773032/592845 and http://www.intelegance.net/content/codebase/DiamondSquare.pde
	static T[] DiamondSquare(T)(ptrdiff_t SizePower, T Range, T CornerValue, uint Seed = 0) if(isFloatingPoint!T) {
		ptrdiff_t Size = pow(2, SizePower) + 1;//1;
		ptrdiff_t Step = Size - 1;		
		T[] Elements = new T[Size * Size];
		if(Seed == 0)
			Seed = unpredictableSeed;
		Random rnd = Random(Seed);
		
		void Set(ptrdiff_t X, ptrdiff_t Y, T Value) {
			Elements[Y * Size + X] = Value;
		}
		T Get(ptrdiff_t X, ptrdiff_t Y) {
			return Elements[Y * Size + X];
		}
		
		Set(0, 0, CornerValue);
		Set(Size - 1, 0, CornerValue);
		Set(0, Size - 1, CornerValue);
		Set(Size - 1, Size - 1, CornerValue);
		/+for(ptrdiff_t SideLength = Size - 1; SideLength >= 2; SideLength /= 2, Roughness /= 2) {
		 ptrdiff_t HalfSide = SideLength / 2;
		 for(ptrdiff_t X = 0; X < Size - 1; X += SideLength) {
		 for(ptrdiff_t Y = 0; Y < Size - 1; Y += SideLength) {
		 T Avg = Get(X, Y) + Get(X + SideLength, Y) + Get(X, Y + SideLength) + Get(X + SideLength, Y + SideLength);
		 Avg /= cast(T)4.0;
		 T Rand = uniform(cast(T)0.0, cast(T)1.0, rnd);
		 Set(X + HalfSide, Y + HalfSide, Avg + (Rand * 2 * Roughness) - Roughness);
		 }
		 }
		 for(ptrdiff_t X = 0; X < Size - 1; X += HalfSide) {
		 for(ptrdiff_t Y = (X + HalfSide) % SideLength; Y < Size - 1; Y += SideLength) {
		 double Avg = 
		 Get((X - HalfSide + Size - 1) % (Size - 1), Y) +
		 Get((X + HalfSide) % (Size - 1), Y) +
		 Get(X, (Y + HalfSide) % (Size - 1)) +
		 Get(X, (Y - HalfSide + Size - 1) % (Size - 1));
		 Avg /= cast(T)4.0;
		 T Rand = uniform(cast(T)0.0, cast(T)1.0, rnd);
		 Avg = Avg + (Rand * 2 * Roughness) - Roughness;
		 Set(X, Y, Avg);
		 if(X == 0)
		 Set(Size - 1, Y, Avg);
		 if(Y == 0)
		 Set(X, Size - 1, Avg);
		 }
		 }
		 }+/
		void ComputeColor(ptrdiff_t X, ptrdiff_t Y, ref Vector2p[4] Points) {
			T C = 0;
			for(ptrdiff_t i = 0; i < 4; i++) {
				if(Points[i].X < 0)
					Points[i].X += (Size - 1);
				else if(Points[i].X > Size)
					Points[i].X -= (Size - 1);
				else if(Points[i].Y < 0)
					Points[i].Y += (Size - 1);
				else if(Points[i].Y > Size)
					Points[i].Y -= (Size - 1);
				C += Get(Points[i].X, Points[i].Y) / cast(T)4.0;
			}
			C += uniform(cast(T)0, cast(T)Range, rnd) - (Range / 2);
			if(C < 0) C = 0;
			if(C > 255) C = 255;
			Set(X, Y, C);
			if(X == 0)
				Set(Size - 1, Y, C);
			else if(X == Size - 1)
				Set(0, Y, C);
			else if(Y == 0)
				Set(X, Size - 1, C);
			else if(Y == Size - 1)
				Set(X, 0, C);			
		}
		
		while(Step > 1) {
			ptrdiff_t HalfStep = Step / 2;
			for(ptrdiff_t X = 0; X < Size - 1; X += Step) {
				for(ptrdiff_t Y = 0; Y < Size - 1; Y += Step) {
					ptrdiff_t SX = X + HalfStep, SY = Y + HalfStep;
					Vector2p[4] Points = void;
					Points[0] = Vector2p(X, Y);
					Points[1] = Vector2p(X + Step, Y);
					Points[2] = Vector2p(X, Y + Step);
					Points[3] = Vector2p(X + Step, Y + Step);
					ComputeColor(SX, SY, Points);
				}
			}
			for(ptrdiff_t X = 0; X < Size - 1; X += Step) {
				for(ptrdiff_t Y = 0; Y < Size - 1; Y += Step) {
					ptrdiff_t X1 = X + HalfStep, X2 = X;
					ptrdiff_t Y1 = Y, Y2 = Y + HalfStep;
					Vector2p[4] Points1 = void;
					Points1[0] = Vector2p(X1 - HalfStep, Y1);
					Points1[1] = Vector2p(X1, Y1 - HalfStep);
					Points1[2] = Vector2p(X1 + HalfStep, Y1);
					Points1[3] = Vector2p(X1, Y1 + HalfStep);
					Vector2p[4] Points2 = void;
					Points2[0] = Vector2p(X2 - HalfStep, Y2);
					Points2[1] = Vector2p(X2, Y2 - HalfStep);
					Points2[2] = Vector2p(X2 + HalfStep, Y2);
					Points2[3] = Vector2p(X2, Y2 + HalfStep);
					ComputeColor(X1, Y1, Points1);
					ComputeColor(X2, Y2, Points2);
				}
			}
			
			Range /= 2;
			Step /= 2;
		}
		return Elements;
	}
	
	/// Generates an array of floats from zero to Amplitude, using a perlin noise generator.
	/// Params:
	/// 	Width = The width of the noise map.
	/// 	Height = The height of the noise map.
	/// 	Frequency = How much the terrain flows, with a lower frequency being more flowing.
	/// 	Amplitude = Indicates the maximum height of the terrain.
	/// 	Persistence = 
	/// 	Octaves = The number of passes to make for each value.
	/// 	Seed = The random seed to use, or zero to randomly generate one.
	/// BUGS:
	/// 	At the moment, the output seems quite horribly wrong. Thus, disabled.
	/// Credits:
	/// 	Implementation from http://stackoverflow.com/a/4753123/592845
	@disable static T[] PerlinNoise(T)(size_t Width, size_t Height, double Frequency, double Amplitude, double Persistence, size_t Octaves, uint Seed = 0) if(isFloatingPoint!T) {
		
		static pure double Total(double i, double j) {
			double t = 0.0;		
			double amp = Amplitude;
			double freq = Frequency;			
			for(size_t k = 0; k < Octaves; k++)  {
				t += GetValue(j * freq + Seed, i * freq + Seed) * amp;
				amp *= Persistence;
				freq *= 2;
			}
			return t;
		}
		
		static pure double GetValue(double X, double Y) {
			int Xint = cast(int)X;
			int Yint = cast(int)Y;
			double Xfrac = X - Xint;
			double Yfrac = Y - Yint;
			
			//noise values
			double n01 = Noise(Xint-1, Yint-1);
			double n02 = Noise(Xint+1, Yint-1);
			double n03 = Noise(Xint-1, Yint+1);
			double n04 = Noise(Xint+1, Yint+1);
			double n05 = Noise(Xint-1, Yint);
			double n06 = Noise(Xint+1, Yint);
			double n07 = Noise(Xint, Yint-1);
			double n08 = Noise(Xint, Yint+1);
			double n09 = Noise(Xint, Yint);
			
			double n12 = Noise(Xint+2, Yint-1);
			double n14 = Noise(Xint+2, Yint+1);
			double n16 = Noise(Xint+2, Yint);
			
			double n23 = Noise(Xint-1, Yint+2);
			double n24 = Noise(Xint+1, Yint+2);
			double n28 = Noise(Xint, Yint+2);
			
			double n34 = Noise(Xint+2, Yint+2);
			
			//find the noise values of the four corners
			double x0y0 = 0.0625*(n01+n02+n03+n04) + 0.125*(n05+n06+n07+n08) + 0.25*(n09);  
			double x1y0 = 0.0625*(n07+n12+n08+n14) + 0.125*(n09+n16+n02+n04) + 0.25*(n06);  
			double x0y1 = 0.0625*(n05+n06+n23+n24) + 0.125*(n03+n04+n09+n28) + 0.25*(n08);  
			double x1y1 = 0.0625*(n09+n16+n28+n34) + 0.125*(n08+n14+n06+n24) + 0.25*(n04);  
			
			//interpolate between those values according to the x and y fractions
			double v1 = Interpolate(x0y0, x1y0, Xfrac); //interpolate in x direction (y)
			double v2 = Interpolate(x0y1, x1y1, Xfrac); //interpolate in x direction (y+1)
			double fin = Interpolate(v1, v2, Yfrac);  //interpolate in y direction
			
			return fin;
		}
		
		static pure double Interpolate(double X, double Y, double A) {
			double NA = 1.0 - A;
			double NAS = NA * NA;
			double F1 = 3.0 * NAS - 2.0 * (NAS * NA);
			double AS = A * A;
			double F2 = 3.0 * AS - 2.0 * (AS * A);
			return X * F1 + Y * F2;
		}
		
		static pure double Noise(int X, int Y) {
			int n = X + Y * 57;
			n = (n << 13) ^ n;
			double t = (n * (n * n * 15731 + 789221) + 1376312589) & 0x7FFFFFFF;
			return 1.0 - t * 0.931322574615478515625E-9;
		}
		
		T[] Elements = uninitializedArray!(T[])(Width * Height);
		if(Seed == 0)
			Seed = unpredictableSeed;
		Seed = 2 + Seed * Seed;
		for(size_t Y = 0; Y < Height; Y++) {
			for(size_t X = 0; X < Width; X++) {			
				Elements[Y * Width + X] = cast(T)(Amplitude * Total(X, Y, Frequency, Persistence, Octaves, Seed));
			}
		}
		return Elements;
	}
	
private:		
}