/// Provides helpers for parsing specific date formats.
/// $(RED The implementation of this module is sub-par, and it should be considered deprecated.)
/// License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
/// Authors: Ognjen Ivkovic
/// Copyright: © 2013 Ognjen Ivkovic
module ShardTools.DateParse;
import std.conv;
import std.datetime;
import std.string;
import std.string;
import ShardTools.ArrayOps;

/// Provides helpers for parsing specific date formats.
class DateParse {

public:

	/// Converts a DateTime to an RFC822/1123 string.
	static string toHttp(DateTime DT) {		
		int DayWeekNum = to!int(DT.dayOfWeek);		
		string Result = WeekDayNames[DayWeekNum] ~ ", ";		
		Result ~= NTS(DT.day);		
		Result ~= ' ' ~ MonthNames[to!int(DT.month) - 1] ~ ' ';
		Result ~= to!string(DT.year) ~ ' ' ~ NTS(DT.timeOfDay.hour) ~ ':' ~ NTS(DT.timeOfDay.minute) ~ ':' ~ NTS(DT.timeOfDay.second) ~ " GMT";
		return Result;
	}

	private static string NTS(int Num) {
		if(Num <= 9)
			return '0' ~ to!string(Num);
		return to!string(Num);
	}

	/// Parses a DateTime value from the given http header text. Returns Default if parsing failed.
	static DateTime parseHttp(string Text, DateTime Default = DateTime.min) {		
		try {
			DateTime Result;
			string[] Split = split(Text, " ");			
			for(size_t i = 0; i < Split.length; i++) {
				string Value = Split[i];
				if(Value[Value.length - 1] == ',')
					Value = Value[0..$-1];	
				if(Value in Months)
					Result.month = to!Month(toLower(Value));
				else if(Value.IndexOf('-') != -1) {
					Result.day = to!int(Value[0..2]);
					Result.month = to!Month(toLower(Value[3..6]));
					int Year = to!int(Value[7..9]);
					if(Year >= 70)
						Year += 1900;
					else
						Year += 2000;
					Result.year = Year;
				} else if(Value.IndexOf(':') != -1) {
					Result.hour = to!int(Value[0..2]);
					Result.minute = to!int(Value[3..5]);
					Result.second = to!int(Value[6..8]);
				} else {
					bool IsNumeric = true;
					foreach(char c; Value)
						if(c < '0' || c > '9') {
							IsNumeric = false;
							break;
						}
					if(!IsNumeric)
						continue;
					int AsInt = to!int(Value);
					if(AsInt > 33)
						Result.year = AsInt;
					else
						Result.day = AsInt;
				}
			}
			return Result;
		} catch {
			return Default;
		}
	}

	shared static this() {		
		Months["Jan"] = 0;
		Months["Feb"] = 1;
		Months["Mar"] = 2;
		Months["Apr"] = 3;
		Months["May"] = 4;
		Months["Jun"] = 5;
		Months["Jul"] = 6;
		Months["Aug"] = 7;
		Months["Sep"] = 8;
		Months["Oct"] = 9;
		Months["Nov"] = 10;
		Months["Dec"] = 11;
		WeekDayNames = new string[7];		
		WeekDayNames[0] = "Mon";
		WeekDayNames[1] = "Tue";
		WeekDayNames[2] = "Wed";
		WeekDayNames[3] = "Thu";
		WeekDayNames[4] = "Fri";
		WeekDayNames[5] = "Sat";
		WeekDayNames[6] = "Sun";
		MonthNames = new string[12];		
		MonthNames[  0] = "Jan";
		MonthNames[  1] = "Feb";
		MonthNames[  2] = "Mar";
		MonthNames[  3] = "Apr";
		MonthNames[  4] = "May";
		MonthNames[  5] = "Jun";
		MonthNames[  6] = "Jul";
		MonthNames[  7] = "Aug";
		MonthNames[  8] = "Sep";		
		MonthNames[  9] = "Oct";
		MonthNames[ 10] = "Nov";
		MonthNames[ 11] = "Dec";
		/*WkDays[  0] = "Mon";
		WkDays[  1] = "Tue";
		WkDays[  2] = "Wed";
		WkDays[  3] = "Thu";
		WkDays[  4] = "Fri";
		WkDays[  5] = "Sat";
		WkDays[  6] = "Sun";
		WeekDays[0] = "Monday";
		WeekDays[1] = "Tuesday";
		WeekDays[2] = "Wednesday";
		WeekDays[3] = "Thursday";
		WeekDays[4] = "Friday";
		WeekDays[5] = "Saturday";
		WeekDays[6] = "Sunday";*/
	}

private:
	static __gshared string[] WeekDayNames;
	static __gshared string[] MonthNames;
	/*static __gshared int[string] WkDays;
	static __gshared int[string] WeekDays;*/
	static __gshared int[string] Months;
}
