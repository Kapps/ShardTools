/// Contains a Timer class that is deprecated with the addition of std.datetime.StopWatch.
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.Timer;
import std.conv;
import std.datetime;
import std.algorithm;
import std.stdio : writefln;

public import ShardTools.TimeSpan;

/// A class used to provide high-precision cross-platform timing access.
/// $(RED With the existence of std.datetime.StopWatch, this class should be considered deprecated.)
class Timer {
	
	/// Initializes a new instance of the Timer object.
	this() {
		
	}
	
	/// Begins measuring time. This has no effect if the timer is already started.
	void Start() {
		if(HasStarted)
			return;
		HasStarted = true;		
		sw.start();		
	}
	
	/// Returns the total amount of time that has elapsed while the timer was tracking, from when this timer was created.
	/// This method includes times from previous starts and stops. To get the amount of time since the last start, use Elapsed.
	@property TimeSpan Total() const {
		return TimeSpan(_Elapsed.Ticks + Elapsed.Ticks);
	}
	
	/// Returns the total amount of time that has passed since the last call to Start.
	/// This method returns an empty TimeSpan if Start has not been called since the last call to Stop or if Start was never called.
	@property TimeSpan Elapsed() const {			
		if(!HasStarted)
			return TimeSpan(0);		
		TimeSpan Result = TimeSpan.FromSeconds(sw.peek().to!("seconds", double));	
		return Result;
	}
	
	/// Stops keeping track of time, returning the amount of time that has passed since the last call to Start.
	/// Returns an empty TimeSpan if the timer is not currently tracking time.
	TimeSpan Stop() {		
		if(!HasStarted)
			return TimeSpan(0);									
		TimeSpan current = TimeSpan.FromSeconds(sw.peek().to!("seconds", double));	
		sw.stop();			
		sw.reset();
		_Elapsed = TimeSpan.Add(current, _Elapsed);				
		return current;
	}
	
	/// Returns the amount of time that has passed since the last call to Start, while acting as if Stop and then Start were called,
	/// meaning that future calls to Stop, Elapsed, or Tick, will return the amount of time since this call instead of since the last Start.
	TimeSpan Tick() {
		if(!HasStarted)
			return TimeSpan(0);		
		TimeSpan ElapsedTime = TimeSpan.FromSeconds(sw.peek().to!("seconds", double));	
		sw.reset();			
		_Elapsed = TimeSpan.Add(_Elapsed, ElapsedTime);
		return ElapsedTime;
	}
	
	/// Returns a new Timer that has begun tracking time.
	static Timer StartNew() {
		Timer result = new Timer();
		result.Start();
		return result;
	}

	/// Stops the timer and prints the output to the console.
	/// Params:
	/// 	Identifier = An identifier to associate with this operation that was being timed.
	TimeSpan StopPrint(string Identifier) {
		TimeSpan Time = Stop();
		writefln(Identifier ~ " elapsed time was " ~ to!string(Time.Milliseconds) ~ " milliseconds.");
		return Time;
	}
	
private:
	TimeSpan _Elapsed;
	StopWatch sw;	
	bool HasStarted = false;	
}