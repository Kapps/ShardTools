/// Provides simple bit manipulation utilities.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.BitOps;


/// Swaps the endian order of the given array, modifying the array in-place.
void SwapEndianOrder(ubyte[] Data) pure {
	ubyte stored;
	for(size_t i = 0; i < Data.length / 2; i++) {
		ubyte* swapped = &Data[$-1-i];
		ubyte* curr = &Data[i];
		stored = *swapped;
		*swapped = *curr;
		*curr = stored;
	}
}