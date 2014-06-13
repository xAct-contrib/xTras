(*********************)
(*                   *)
(*  Public messages  *)
(*                   *)
(*********************)

xTrasHelp::usage = "xTrasHelp[] opens the \!\(\*StyleBox[\"xTras\",\"IT\"]\) documentation.";

LevelSpecQ::usage = 
	"LevelSpecQ[levelspec] yields True if levelspec is a standard levelspec, and False otherwise.";

ToLevelSpec::usage =
	"ToLevelSpec[levelspec] converts levelspec into the {min,max} format.";
ToLevelSpec::error = "`1` is not a a valid level spec.";

TimeString::usage = 
	"TimeString[seconds] nicely formats the amount of seconds as a string.";

Description::usage = "Description is an option for MapTimed.";

MapTimed::usage = "\
MapTimed[f, expr] maps f over expr while monitoring the progress and the estimated time remaining.\n\
MapTimed[f, expr, Description->\"text\"] gives a description to the monitor.\n\
MapTimed[f, expr, Parallelization->True] performs a ParallelMap instead of a Map.";

MapTimedIfPlus::usage =
	"MapTimedIfPlus[f, expr] maps f on the elements of expr while displaying a timer \
if expr has head Plus, or returns f[expr] otherwise.";

If[System`$VersionNumber < 8.,
	Parallelization::usage = "Parallelization is an option for MapTimed."
];


Begin["`Private`"]

xTrasHelp[] := 
	(* SystemOpen is MMA 7 and newer, but somehow it doesn't work in MMA 7. *)
	If[ System`$VersionNumber < 8.,
		Documentation`HelpLookupPacletURI["paclet:xTras/guide/xTras"],
		SystemOpen["paclet:xTras/guide/xTras"]
	];


TimeString[seconds_Integer]/;seconds >= 31536000:=TimeString1[seconds,31536000,2628000," year"," month"];
TimeString[seconds_Integer]/;seconds >= 2628000:=TimeString1[seconds,2628000,86400," month"," day"];
TimeString[seconds_Integer]/;seconds >= 86400:=TimeString1[seconds,86400,3600," day"," hour"];
TimeString[seconds_Integer]/;seconds >= 3600:=TimeString1[seconds,3600,60," hour"," minute"];
TimeString[seconds_Integer]/;seconds >= 60:=TimeString1[seconds,60,1," minute"," second"];
TimeString[seconds_Integer]/;seconds > 1:=StringJoin[ToString[seconds], " seconds"];
TimeString[1] := "1 second";
TimeString[0] := "0 seconds";

TimeString1[n_,t1_,t2_,s1_,s2_]:=StringJoin[
	TimeString2a[Quotient[n,t1],s1],
	TimeString2b[Quotient[Mod[n,t1],t2],s2]
];

TimeString2a[0,_]	:= Sequence[];
TimeString2a[1,s_]	:= Sequence["1",s]
TimeString2a[x_,s_]	:= Sequence[ToString[x],s,"s"];

TimeString2b[0,_]	:= Sequence[];
TimeString2b[1,s_]	:= Sequence[", 1",s]
TimeString2b[x_,s_]	:= Sequence[", ",ToString[x],s,"s"];



LevelSpecQ[{x_Integer, y_Integer}] /; x >= 0 && y >= x := True
LevelSpecQ[x_Integer] /; x >= 0 := True
LevelSpecQ[{x_Integer}] /; x >= 0 := True
LevelSpecQ[Infinity] = True
LevelSpecQ[{x_Integer, Infinity}] /; x >= 0 := True
LevelSpecQ[{Infinity, Infinity}] = True
LevelSpecQ[___] := False

ToLevelSpec[x_Integer] /; x >= 0 := {0,x};
ToLevelSpec[{x_Integer}] /; x >= 0 := {x,x};
ToLevelSpec[Infinity] = {0,Infinity};
ToLevelSpec[{Infinity}] = {Infinity,Infinity}; 
ToLevelSpec[All] = {0,Infinity};
ToLevelSpec[None] = {0,0};
ToLevelSpec[x_?LevelSpecQ] := x;
ToLevelSpec[x_] := Throw@Message[ToLevelSpec::error, x];


Options[MapTimed] ^= {
	Description -> None, 
	Parallelization -> False
};

MapTimed[func_,expr_,levelspec_: {1},options___?OptionQ]/;LevelSpecQ[levelspec] := Module[
	{
		desc, parallel,	timer, ETA,
		length = 0,
		position = 0
	},
	
	(* Determine the options. *)
	{desc,parallel} = {Description,Parallelization}  /. CheckOptions[options] /. Options[MapTimed];
	If[desc === None,
		desc = "", 
		desc = " " <> ToString@desc;
		If[StringTake[desc,-1]=!=".",
			desc = desc <> ".";
		]
	];
	
	(* Do a test run to determine the length of the map. *)
	Map[(length++)&,expr,levelspec];
	
	(* Set the helper functions ETA and timer. *)
	With[
		{
			begintime 	= AbsoluteTime[],
			l			= length,
			s1			= " **" <> desc <> " Parts ",
			s2			= "/" <> ToString[length] <> " done. "
		},
		ETA[pos_]	:= Ceiling[(AbsoluteTime[]-begintime)*(l-pos)/pos];
		timer[pos_]	:= s1 <> ToString@pos <> s2 <> TimeString@ETA@pos <> " remaining.";
	];
	timer[0] 		= " **" <> desc <> " " <> ToString[length] <> " parts.";
	timer[length] 	= " **" <> desc <> " Parts " <> ToString[length] <> "/" <> ToString[length] <> " done.";
	
	(* Do the mapping. *)
	If[TrueQ[parallel] && System`$VersionNumber >= 7.,
		SetSharedVariable[position];
		(UnsetShared[position];#)&@Monitor[ParallelMap[((position++;#)&@func[#])&,expr,levelspec],timer@position]
	,
		Monitor[Map[((position++;#)&@func[#])&,expr,levelspec],timer@position]
	]
];

MapTimedIfPlus[f_, expr_Plus, rest___] := MapTimed[f, expr, rest];
MapTimedIfPlus[f_, expr_, rest___] := f[expr];

ParallelxPermConnect[] := 
	If[ System`$VersionNumber >= 7 && Not[ And @@ ($xpermLink =!= # & /@ ParallelEvaluate[$xpermLink]) ],
		ParallelEvaluate @ Block[{Print},
			Needs["xAct`xPerm`"];
			$xpermQ = xAct`xPerm`Private`xpermConnect
		]
	];

End[]