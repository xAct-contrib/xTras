(*********************)
(*                   *)
(*    To-do list     *)
(*                   *)
(*********************)

(*

 * Add SyntaxInformation and argx messages to each function.
 * Undef hooks.
 * Add conditionals for when to define extra curvature tensors (e.g. when there's torsion etc).
 * Switch Modules to With's where approriate.

*)


(*********************)
(*                   *)
(*   Package setup   *)
(*                   *)
(*********************)

xAct`xTras`$Version = {"1.4.2", {2014, 10, 30}};
xAct`xTras`$xTensorVersionExpected = {"1.1.1", {2014, 9, 6}};
xAct`xTras`$SymManipulatorVersionExpected = {"0.8.5", {2013, 4, 13}};
xAct`xTras`$MathematicaVersionNeeded = 6.;

If[Unevaluated[xAct`xCore`Private`$LastPackage] === xAct`xCore`Private`$LastPackage, 
	xAct`xCore`Private`$LastPackage = "xAct`xTras`"
];

(* 
 * This is a (possibly dangerous) hack in order to enable the documentation tools in Workbench, 
 * which only works for top-level packages. Set this to "xTras" before loading the package,
 * and you can use the documentation tools.
 *)   
If[Unevaluated[xAct`xTras`Private`$xTrasContext] === xAct`xTras`Private`$xTrasContext,
	xAct`xTras`Private`$xTrasContext = "xAct`xTras`"
];

BeginPackage[xAct`xTras`Private`$xTrasContext, {
	"xAct`xCore`",
	"xAct`xPerm`",
	"xAct`xTensor`",
	"xAct`xPert`",
	"xAct`Invar`",
	"xAct`xCoba`",
	"xAct`SymManipulator`"
}]

(* Print info *)
Print[xAct`xCore`Private`bars];
Print["Package xAct`xTras`  version ", xAct`xTras`$Version[[1]],", ",xAct`xTras`$Version[[2]]];
Print["CopyRight (C) 2012-2014, Teake Nutma, under the General Public License."];


(* Check if we have the correct version of xAct. *)
If[Not@OrderedQ@Map[Last, {xAct`xTras`$xTensorVersionExpected, xAct`xTensor`$Version}], 
	Message[General::versions, "xTensor", xAct`xTensor`$Version, xAct`xTras`$xTensorVersionExpected];
	Abort[]
];

(* Check if we have the correct version of SymManipulator. *)
If[Not@OrderedQ@Map[Last, {xAct`xTras`$SymManipulatorVersionExpected, xAct`SymManipulator`$Version}], 
	Message[General::versions, "SymManipulator", xAct`SymManipulator`$Version, xAct`xTras`$SymManipulatorVersionExpected];
	Abort[]
];

(* Check for MMA versions. *)
If[System`$VersionNumber < xAct`xTras`$MathematicaVersionNeeded,
	Message[General::versions, "Mathematica", System`$VersionNumber, xAct`xTras`$MathematicaVersionNeeded];
	Abort[]
];

(* Reset some options. *)
ReportSet[$CovDFormat, "Prefix"];
ReportSetOption[DefCovD, CurvatureRelations -> True];



(* Load the code. *)
Get["xAct`xTras`xCore`"];
Get["xAct`xTras`xTensor`"];
Get["xAct`xTras`xPert`"];
Get["xAct`xTras`Invar`"];
Get["xAct`xTras`xCoba`"];
Get["xAct`xTras`Algebra`"];
Get["xAct`xTras`Combinatorics`"];

(* Load fancy usage messages. 
   These need to be loaded last because they overwrite plain usage messages
   defined in the files loaded above. *)
Get["xAct`xTras`Interface`"];

Begin["`Private`"]

(*
 * Add the xAct directory to the path of the PacletManager.
 * This allows it to find the documentation in xAct/xTras/Documentation.
 * We could have used xAct`xCore`$xActDirectory, but this is a bit more general.
 *)

If[
	System`$VersionNumber < 7.
, 
	xActDir = StringTake[
		First@Select[
			(* Find files / directories with "xTras" in their name *)
			FileNames["xTras", {$UserBaseDirectory, $BaseDirectory, $InstallationDirectory}, Infinity], 
			(* Select the xTras directory *)
			StringMatchQ[#, "*xAct/xTras"] &
		], 
		(* Strip the "/xTras" from it *)
		{1, -7}
	]
,
	xActDir = FileNameJoin@Drop[FileNameSplit@FindFile["xAct`xTras`"], -3]
];

(* MMA 9 needs a Kernel extension in the PacletInfo.m file, whereas this chokes MMA 6. So remove it for MMA 6. *)
If[
	System`$VersionNumber < 7.
,	
	pacletFile 	= ToFileName[{xActDir,"xTras"},"PacletInfo.m"];
	paclet 		= Get[pacletFile];
	pacletNew 	= paclet /. {"Kernel", ___} -> Sequence[];
	Put[pacletNew, pacletFile];
]

PacletManager`PacletDirectoryAdd[xActDir];
(* Rebuild the PacletData. Necessary on MMA 6. *)
PacletManager`RebuildPacletData[];

(* Restore the old paclet file. *)
If[
	System`$VersionNumber < 7.
,	
	Put[paclet, pacletFile];
]


(*
 * The following is a dirty hack to get URI links in the Information output of all usage messages.
 * For "top-level" packages (i.e. packages whose symbols are all in one top-level context,
 * like "myPackage`") this isn't necessary, as then everythings works out of the box
 * But because we had to hack our way around the documentation tools because xTras lives 
 * in "xAct`xTras`", the links do not work by themselves.
 * Therefore, we set for each public symbol the paclet link manually.
 *) 

(* Only do this when MMA can find the documentation. (Otherwise it's pointless). *)
If[
	(* Search for the AllContractions documentation. If it's there, good chance the rest is too. *)
	Documentation`CreateMessageLink["xTras`", "AllContractions", "usage", "English"] == "paclet:xTras/ref/AllContractions"
,
	(* First, unprotect Documentation`CreateMessageLink. *)
	Unprotect[Documentation`CreateMessageLink];
	
	(* For each public symbol, overwrite its output. *)
	Map[
		Function[
			symbol,
			Documentation`CreateMessageLink[
				xAct`xTras`Private`$xTrasContext, 
				ToString@symbol, 
				"usage", 
				"English"
			] = "paclet:xTras/ref/" <> ToString@symbol
		],
		Names[xAct`xTras`Private`$xTrasContext <> "*"]
	];
	
	(* For xTrasHelp, set the link to the xTras guide. *)
	Documentation`CreateMessageLink[
		xAct`xTras`Private`$xTrasContext,
		"xTrasHelp",
		"usage",
		"English"
	] = "paclet:xTras/guide/xTras";
	
	(* Finally, protect Documentation`CreateMessageLink again. *)
	Protect[Documentation`CreateMessageLink];
];

End[]

(* Print license *)
If[xAct`xCore`Private`$LastPackage === "xAct`xTras`",
	Unset[xAct`xCore`Private`$LastPackage];
	Print[xAct`xCore`Private`bars];
	Print["These packages come with ABSOLUTELY NO WARRANTY; for details \
type Disclaimer[]. This is free software, and you are welcome to \
redistribute it under certain conditions. See the General Public \
License for details."];
	Print[xAct`xCore`Private`bars]
];


EndPackage[]