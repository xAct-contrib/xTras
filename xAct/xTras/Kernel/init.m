(*********************)
(*                   *)
(*    To-do list     *)
(*                   *)
(*********************)

(*

 * Undef hooks.
 * Add conditionals for when to define extra curvature tensors (e.g. when there's torsion etc).
 * Switch Modules to With's where approriate.

*)


(*********************)
(*                   *)
(*   Package setup   *)
(*                   *)
(*********************)

xAct`xTras`$Version = {"1.1.3pre", {2013, 4, 18}};
xAct`xTras`$xTensorVersionExpected = {"1.0.5", {2013, 1, 27}};
xAct`xTras`$SymManipulatorVersionExpected = {"0.8.5", {2013, 4, 13}};
xAct`xTras`$MathematicaVersionNeeded = 6.;

If[Unevaluated[xAct`xCore`Private`$LastPackage] === xAct`xCore`Private`$LastPackage, 
	xAct`xCore`Private`$LastPackage = "xAct`xTras`"
];

Get["xAct`xTras`xCore`"];
Get["xAct`xTras`xTensor`"];
Get["xAct`xTras`xPert`"];
Get["xAct`xTras`Invar`"];
Get["xAct`xTras`xCoba`"];
Get["xAct`xTras`Algebra`"];
Get["xAct`xTras`Combinatorics`"];

BeginPackage["xAct`xTras`"]


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

(* Print info *)
Print[xAct`xCore`Private`bars];
Print["Package xAct`xTras`  version ", xAct`xTras`$Version[[1]],", ",xAct`xTras`$Version[[2]]];
Print["CopyRight (C) 2012-2013, Teake Nutma, under the General Public License."];

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