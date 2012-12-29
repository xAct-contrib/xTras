(*********************)
(*                   *)
(*    To-do list     *)
(*                   *)
(*********************)

(*

 * Write proper unit tests.
 * Split into multiple packages.
 * Change DummyIn to GetIndicesOfVBundle where appropriate (not everywhere!).
 * Switch Modules to With's where approriate.

*)


(*********************)
(*                   *)
(*   Package setup   *)
(*                   *)
(*********************)

xAct`xTras`$Version = "1.1.0pre";
xAct`xTras`$xTensorVersionExpected = {"1.0.4", {2012, 5, 5}};

If[Unevaluated[xAct`xCore`Private`$LastPackage] === xAct`xCore`Private`$LastPackage, 
	xAct`xCore`Private`$LastPackage = "xAct`xTras`"
];

BeginPackage["xAct`xTras`",{
	"xAct`xTras`xCore`",
	"xAct`xTras`xTensor`",
	"xAct`xTras`xPert`",
	"xAct`xTras`Invar`",
	"xAct`xTras`xCoba`",
	"xAct`xTras`Algebra`",
	"xAct`xTras`Combinatorics`"
}]

(* Check if we have the correct version of xAct. *)
If[Not@OrderedQ@Map[Last, {xAct`xTras`$xTensorVersionExpected, xAct`xTensor`$Version}], 
	Message[General::versions, "xTensor", xAct`xTensor`$Version, xAct`xTras`$xTensorVersionExpected];
	Abort[]
];

(* Print info *)
Print[xAct`xCore`Private`bars];
Print["Package xAct`xTras`  version ", xAct`xTras`$Version];
Print["Written by Teake Nutma."];
Print["https://code.google.com/p/xact-xtras/"];


If[xAct`xCore`Private`$LastPackage === "xAct`xTras`",
	Unset[xAct`xCore`Private`$LastPackage];
	Print[xAct`xCore`Private`bars];
	Print["These packages come with ABSOLUTELY NO WARRANTY; for details \
type Disclaimer[]. This is free software, and you are welcome to \
redistribute it under certain conditions. See the General Public \
License for details."];
	Print[xAct`xCore`Private`bars]
];

(*********************)
(*                   *)
(*   Begin private   *)
(*                   *)
(*********************)


Begin["`Private`"]


(*********************)
(*                   *)
(*    End package    *)
(*                   *)
(*********************)

End[]

EndPackage[]
