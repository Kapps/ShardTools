module ShardTools.NativeReference;
private import core.memory;
private import std.exception;
private import std.container;


/// A helper class used to keep a reference to a desired object, ensuring the garbage collector knows about it while it gets passed into non-GC code.
/// $(RED This class should be considered deprecated; consider using GC.addRoot instead.)
class NativeReference  {
	
	public static:
	
	/// Adds a reference to the given object.
	/// Params:
	/// 	Obj = The object to add a reference to.
	void AddReference(void* Obj) {
		/*synchronized(typeid(NativeReference)) {
		 if(Objects is null)
		 Objects = new Collection();
		 Objects.insert(Obj);
		 }*/
		GC.addRoot(Obj);
	}
	
	/// Removes a single reference to the given object.
	/// Params:
	/// 	Obj = The object to remove the reference to.
	void RemoveReference(void* Obj) {
		/*synchronized(typeid(NativeReference)) {
		 enforce(Objects !is null && Objects.removeKey(Obj) == 1, "The object to remove was not found.");
		 }*/
		GC.removeRoot(Obj);
	}
	
	//private static:
	//alias RedBlackTree!(void*) Collection;
	//__gshared Collection Objects;
}