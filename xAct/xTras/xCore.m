BeginPackage["xAct`xTras`xCore`", {
	"xAct`xCore`"
}]

(*********************)
(*                   *)
(*  Public messages  *)
(*                   *)
(*********************)

LevelSpecQ::usage = 
	"LevelSpecQ[levelspec] yields True if levelspec is a standard levelspec, and false otherwise.";

TimeString::usage = 
	"TimeString[seconds] nicely formats the amount of seconds as a \
string.";

Description::usage = "Option for MapTimed.";

MonitorSteps::usage = "Option for MapTimed.";

MapTimed::usage = 
	"MapTimed[f,expr] is similar to Map, except that it also prints \
the expected calculation time.";

MapTimedIfPlus::usage =
	"MapTimedIfPlus[f, expr] maps f on the elements of expr while displaying a timer \
if expr has head Plus, or returns f[expr] otherwise.";




Begin["`Private`"]


TimeString[0] = "0 seconds";
TimeString[seconds_] := Module[{s, m, h, d, M, y, list},
	s = Ceiling[seconds];
	y = Floor[s/31536000];
	s = s - y*31536000;
	M = Floor[s/2628000];
	s = s - M*2628000;
	d = Floor[s/86400];
	s = s - d*86400;
	h = Floor[s/3600];
	s = s - h*3600;
	m = Floor[s/60];
	s = s - m*60;
	list = Transpose[{
		{y, M, d, h, m, s},
		{"year", "month", "day", "hour", "minute", "second"}
	}];
	StringJoin@Riffle[
		ReleaseHold /@ Map[
			If[First@# > 0,
				ToString@First@# <> " " <> Last@# <> If[First@# > 1, "s", ""],
				HoldComplete[Sequence[]]
			] &,
			list
		],
		", "
	]
];

LevelSpecQ[{x_Integer, y_Integer}] /; x >= 0 && y >= x := True
LevelSpecQ[x_Integer] /; x >= 0 := True
LevelSpecQ[{x_Integer}] /; x >= 0 := True
LevelSpecQ[Infinity] := True
LevelSpecQ[___] := False


Options[MapTimed] ^= {
	Description -> "", 
	MonitorSteps -> All
};

MapTimed[func_, expr_, levelspec_: {1}, options___?OptionQ] /; LevelSpecQ[levelspec] := Module[
	{begintime, desc, ms, length, stringlength, timer, mon, ETA, steps, position},
	 
	(* Determine the options *)
	{desc, ms} = {Description, MonitorSteps} /. CheckOptions[options] /. Options[MapTimed];
	desc 			= StringTrim@ToString@desc;
	If[desc =!= "", 
		desc = " " <> desc;
		If[StringTake[desc,-1]=!=".",
			desc = desc <> ".";
		]
	];
	(* Initialize variables *)
	length 		= 0;
	position 	= 0;
	(* Do a test run to determine the length of the map. *)
	Map[(length++) &, expr, levelspec];
	stringlength = ToString[length];
	steps = If[
		ms === All, 
		1, 
		Ceiling[length/ms]
	];
	begintime 	= AbsoluteTime[];
	mon 		= " **" <> desc <> " " <> stringlength <> " parts.";
	
	ETA[pos_] 		:= Ceiling[(AbsoluteTime[] - begintime)*(length - pos)/pos];
	timer[part_] 	:= Module[{result},
		result = func@part;
		position++;
		If[
			ms === All || Mod[position, steps] == 0, 
			mon = " **" <> desc <> " Parts " 
				<> ToString@position <> "/" <> stringlength 
				<> " done. ETA in " <> TimeString@ETA@position <> "."
		];
		result
	];
	
	Monitor[
		Map[timer, expr, levelspec], 
		mon
	]
];

MapTimedIfPlus[f_, expr_Plus, rest___] := MapTimed[f, expr, rest];
MapTimedIfPlus[f_, expr_, rest___] := f[expr];



End[]
EndPackage[]