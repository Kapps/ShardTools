/// Provides a class similar to Event but ordered and not thread-safe.
/// The implementation of this class is very poor and is generally not useable.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.PrioritizedEvent;

import ShardTools.SortedList;

/// An event that executes callbacks in a specified order, and stops executing callbacks if one returns true.
class PrioritizedEvent(T...) {
	
public:
	
	/// Initializes a new instance of the PrioritizedEvent class.
	this() { 
		List = new Collection(4);
	}		
	
	/**
	 * Executes the specified callback.
	 * Params: 
	 *	sender = The object invoking this callback.
	 *	Params = The parameters for the callback.
	 * Returns: Whether all of the callbacks were executed.
	*/
	bool Execute(T Params) {
		for(size_t i = 0; i < List.Count; i++) {
			if(List[i](Params))
				return false;
		}
		return true;
	}
	
	/** 
	 * Adds the specified callback to the collection.
	 * Params:
	 *	Priority = The priority for this callback. A lower value is executed first. These should be designed around the value of 0 being not caring about order, greater than zero meaning after those, and less than meaning before.
	 *	Callback = The callback to execute.
	*/
	void Add(int Priority, bool delegate(T) Callback) {
		List.Add(Callback, Priority);
	}
		
private:
	alias SortedList!(bool delegate(T)) Collection;
	Collection List;
}