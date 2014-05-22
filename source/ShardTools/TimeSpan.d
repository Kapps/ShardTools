/// Contains a struct for representing periods of time as floating point values.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.TimeSpan;
import core.time;

import std.conv;
import std.stdio;

/// A struct used to represent a total period of time as floating point values.
/// All operations return the total value, never the specific unit. 
/// For example 2 seconds 500 milliseconds would result in a $(D seconds) value of 2.5.
/// Where integer values are desired, $(D core.time.Duration) or ($D core.time.TickDuration) should be preferred.
struct TimeSpan {
	
public:	
	/// Instantiates a new instance of the TimeSpan object from either a given number of ticks or a TickDuration.
	this(long tickCount) {
		this(TickDuration(tickCount));
	}
	
	/// ditto
	this(TickDuration duration) {
		this.duration = duration;
	}
	
	
	/// Returns the total number of days in this TimeSpan.
	@property double days() const {
		return hours / 24;
	}
	
	/// Returns the total number of hours in this TimeSpan.
	@property double hours() const {
		return minutes / 60;
	}
	
	/// Returns the total number of minutes contained in this TimeSpan.
	@property double minutes() const {
		return seconds / 60;
	}
	
	/// Returns the total number of seconds contained in this TimeSpan.
	@property double seconds() const {		
		return duration.to!("seconds", double);
	}
	
	/// Returns the total number of milliseconds contained in this TimeSpan.
	@property double msecs() const {		
		return seconds * 1000;
	}
	
	/// Returns the total number of ticks contained in this TimeSpan.
	@property long ticks() const {
		return duration.length;
	}
	
	/// 
	int opCmp(in TimeSpan other) const {
		return duration.opCmp(other.duration);
	}
	
	///
	TimeSpan opBinary(string Op)(in TimeSpan other) const {
		TimeSpan result = this;
		mixin(binaryMixin("result", "other", Op));
		return result;
	}
	
	///
	TimeSpan opOpAssign(string op)(in TimeSpan other) {
		mixin(binaryMixin("this", "other", op));
		return this;
	}
	
	///
	TimeSpan opAssign(in TimeSpan other) {
		this.duration = other.duration;
		return this;
	}
	
	private static string binaryMixin(string left, string right, string op) {
		return left ~ ".duration " ~ op ~ "= " ~ right ~ ".duration;";
	}
	
	/// Returns a string representation of this object.
	string toString() const {
		return (cast(Duration)duration).text;
	}
	
	/**
	 * Creates a new TimeSpan from the specified number of ticks.
	 * Params: tickCount = The number of ticks to create the TimeSpan with.
	 */
	static TimeSpan fromTicks(long tickCount) {
		return TimeSpan(tickCount);
	}
	
	/**
	 * Creates a new TimeSpan from the specified number of milliseconds.
	 * Params: milliseconds = The number of milliseconds to create the TimeSpan with.
	 */
	static TimeSpan fromMilliseconds(double milliseconds) {	
		return TimeSpan.fromSeconds(milliseconds / 1000);
	}
	
	/**
	 * Creates a new TimeSpan from the specified number of seconds.
	 * Params: seconds = The number of seconds to create the TimeSpan with.
	 */
	static TimeSpan fromSeconds(double seconds) {
		return TimeSpan.fromTicks(cast(long)(seconds * TickDuration.ticksPerSec));
	}
	
private:	
	TickDuration duration;
}