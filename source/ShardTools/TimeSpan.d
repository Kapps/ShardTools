/// Contains a struct for representing periods of time.
/// This module is deprecated with the addition of std.datetime.Duration / core.time.TickDuration.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.TimeSpan;
import core.time;

import std.conv;
import std.stdio;

// TODO: Now that dur and such are in, phase this out?

/// A struct used to represent a period of time.
/// $(RED This struct is deprecated; new code should use std.datetime.Duration or core.time.TickDuration.)
struct TimeSpan {

public:	
	/// Instantiates a new instance of the TimeSpan object.
	/// Params:
	///		TickCount = The number of ticks in this TimeSpan.
	this(double TickCount) {
		this.TickCount = TickCount;	
	}
	
	/// Adds two TimeSpans together.
	///	Params:
	///		first = The first TimeSpan.
	///		second = The second TimeSpan.
	static TimeSpan Add(const TimeSpan first, const TimeSpan second) {
		return TimeSpan(first.TickCount + second.TickCount);
	}

	/// Returns the total number of days in this TimeSpan.
	@property double Days() const {
		return Hours / 24;
	}
	
	/// Returns the total number of hours in this TimeSpan.
	@property double Hours() const {
		return Minutes / 60;
	}

	/// Returns the total number of minutes contained in this TimeSpan.
	@property double Minutes() const {
		return Seconds / 60;
	}
	
	/// Returns the total number of seconds contained in this TimeSpan.
	@property double Seconds() const {		
		return TickCount / cast(double)TickDuration.ticksPerSec;
	}
	
	/// Returns the total number of milliseconds contained in this TimeSpan.
	@property double Milliseconds() const {		
		return Seconds * 1000;
	}
	
	/// Returns the total number of ticks contained in this TimeSpan.
	@property double Ticks() const {
		return TickCount;
	}
	
	/// Compares the specified TimeSpans.
	/// Params:
	///		other = The TimeSpan to compare to.	
	int opCmp(TimeSpan other) const {
		return TickCount > other.TickCount ? 1 : TickCount == other.TickCount ? 0 : -1;
	}
	
	TimeSpan opBinary(string Op)(in TimeSpan Other) const {
		TimeSpan Result = this;
		mixin(BinaryMixin("Result", "Other", Op));
		return Result;
	}

	TimeSpan opOpAssign(string Op)(in TimeSpan Other) {
		mixin(BinaryMixin("this", "Other", Op));
		return this;
	}

	TimeSpan opAssign(in TimeSpan Other) {
		this.TickCount = Other.TickCount;
		return this;
	}

	private static string BinaryMixin(string Left, string Right, string Op) {
		return Left ~ ".TickCount " ~ Op ~ "= " ~ Right ~ ".TickCount;";
	}
	
	/// Returns a string representation of this object.
	string toString() const {
		// TODO: Seconds > 1 ? 00:00:00.000 : 561 milliseconds
		if(Minutes > 1)
			return to!string(Minutes) ~ " minutes";
		if(Seconds > 1)
			return to!string(Seconds) ~ " seconds";
		return to!string(Milliseconds)  ~ " milliseconds";
	}
	
	/**
	 * Creates a new TimeSpan from the specified number of ticks.
	 * Params: TickCount = The number of ticks to create the TimeSpan with.
	*/
	static TimeSpan FromTicks(double TickCount) {
		return TimeSpan(TickCount);
	}
	
	/**
	 * Creates a new TimeSpan from the specified number of milliseconds.
	 * Params: Milliseconds = The number of milliseconds to create the TimeSpan with.
	*/
	static TimeSpan FromMilliseconds(double Milliseconds) {	
		return TimeSpan.FromSeconds(Milliseconds / 1000);
	}
	
	/**
	 * Creates a new TimeSpan from the specified number of seconds.
	 * Params: Seconds = The number of seconds to create the TimeSpan with.
	*/
	static TimeSpan FromSeconds(double Seconds) {
		return TimeSpan(Seconds * TickDuration.ticksPerSec);
	}
	
private:	
	double TickCount = 0;		
}