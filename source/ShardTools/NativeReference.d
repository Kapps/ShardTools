/// A helper module used to keep a garbage collected reference to a desired object.
/// $(RED This module should be considered deprecated; consider using GC.addRoot instead.)
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.NativeReference;
private import core.memory;
private import std.exception;
private import std.container;


/// A helper module used to keep a reference to a desired object, ensuring the garbage collector knows about it while it gets passed into non-GC code.
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