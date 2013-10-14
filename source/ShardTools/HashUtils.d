/// Provides a variety of hash algorithms to use on data.
module ShardTools.HashUtils;

static if(size_t.sizeof == 4) {
	enum size_t FNV_PRIME = 16777619U;
	enum size_t FNV_OFFSET_BASIS = 2166136261U;
} else static if(size_t.sizeof == 8) {
	enum size_t FNV_PRIME = 1099511628211UL;
	enum size_t FNV_OFFSET_BASIS = 14695981039346656037UL;
} else {
	static assert(0, "Unable to calculate an FNV Prime for the given architecture; expected size_t.sizeof to be either 4 or 8.");
}

/// Calculates an FNV-1A hash from the given data.
/// This is a general purpose hash that's fast to calculate, has low collision rates, and has high dispersion even amongst similar data.
/// This hash is not cryptographically secure, and is easy to brute force or guess a collision for.
/// For more information about the FNV hash, see http://www.isthe.com/chongo/tech/comp/fnv/index.html.
hash_t fnvHash(ubyte[] data) {
	hash_t result = FNV_OFFSET_BASIS;
	foreach(b; data) {
		result = result ^ b;
		result = result * FNV_PRIME;
	}
	return result;
}

unittest {

}