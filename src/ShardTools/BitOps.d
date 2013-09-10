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