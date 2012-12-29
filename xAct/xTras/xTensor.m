BeginPackage["xAct`xTras`xTensor`", {
	"xAct`xCore`",
	"xAct`xPerm`",
	"xAct`xTensor`",
	"xAct`xTras`xCore`"
}]

ConstantExprQ::usage = 
  "ConstantExprQ[expr] returns True if expr only contains contains \
constants (i.e. constant symbols and integers, fractions, etc), and \
False otherwise.";

(* ClearAutomaticRules et. al. *)

ClearAutomaticRules::usage = 
  "ClearAutomaticRules[symbol,rules] tries to remove rules from the \
upvalues and downvalues of symbol.";

ClearCurvatureRelations::usage = 
  "ClearCurvatureRelations[CD] removes the automatic curvature \
relations for the covariant derivative CD.";

SetCurvatureRelations::usage = 
  "SetCurvatureRelations[CD] sets the automatic curvature relations \
for the covariant derivative CD.";

CurvatureRelationsQ::usage = 
  "CurvatureRelationsQ[CD] returns True if all of the curvature \
relations for the covariant derivative CD have been set as automatic \
rules, and False otherwise.";

(* Other stuff *)

DerivativeOrder::usage =
	"DerivativeOrder[expr,CD] gives the order of derivatives of expr.";

SortedCovDsQ::usage =
	"SortedCovDsQ[expr] returns True if the expression has all its covariant \
derivatives sorted, and False otherwise. \
\n\nSortedCovDsQ[expr,CD] only checks the covariant derivative CD.";


(* Extra curvature tensors *)
	
Schouten::usage = 
  "Schouten is a reserved word in xTras. It is used to generate the \
name of the Schouten curvature tensor associated to a connection \
acting on a tangent bundle.";

SchoutenToRicci::usage = "SchoutenToRicci[expr,cd] converts Schouten tensors to Ricci tensors.";

RicciToSchouten::usage = "RicciToSchouten[expr,cd] converts Ricci tensors to Schouten tensors.";

SchoutenCC::usage = 
  "SchoutenCC is a reserved word in xTras. It is used to generate the \
name of the cosmological Schouten curvature tensor associated to a connection \
acting on a tangent bundle.";

SchoutenCCToRicci::usage = "SchoutenCCToRicci[expr,cd] converts cosmological Schouten tensors to Ricci tensors.";

RicciToSchoutenCC::usage = "RicciToSchoutenCC[expr,cd] converts Ricci tensors to cosmological Schouten tensors.";

EinsteinCC::usage = 
  "EinsteinCC is a reserved word in xTras. It is used to generate the \
name of the cosmological Einstein curvature tensor associated to a connection \
acting on a tangent bundle.";

EinsteinCCToRicci::usage = "EinsteinCCToRicci[expr,cd] converts cosmological Einstein tensors to Ricci tensors.";

RicciToEinsteinCC::usage = "RicciToEinsteinCC[K][expr,cd] converts Ricci tensors into cosmological Einstein tensors.";



(* Simplifying *)

PreferBoxOfRule::usage = 
  "PreferBoxOfRule[tensor,CD] gives rules for rewriting an expression \
with boxes on the given tensor with respect to the given CD.";

PreferBoxOf::usage =
	"PreferBoxOf[tensor,CD][expr] commutes the covariant derivatives CD in expr \
such that all possible boxes (CD[-a]@CD[a]) act on the specified tensor.";

PreferDivOfRule::usage = 
  "PreferDivOfRule[tensor,CD] gives rules for rewriting an expression \
with divergences of the given tensor with respect to the given CD.";

PreferDivOf::usage =
	"PreferDivOf[tensor,CD][expr] commutes the covariant derivatives CD in expr \
such that any covariant derivatve contracted with tensor acts directy on tensor.";

DivFreeQ::usage = 
  "DivFreeQ[expr,tensor,CD] returns True if expr does not contain a \
divergence of the given tensor w.r.t. the given CD after arbritrary \
commutations of the covariant derivatives."; 

RicciDivRule::usage = 
  "RicciDivRule[CD] gives rules for rewriting the divergence of the \
Ricci tensor of the given covariant derivative in terms of the Ricci \
scalar.";

RiemannDivRule::usage = 
  "RiemannDivRule[CD] gives rules for rewriting the divergence of the \
Riemann tensor of the given covariant derivative in terms of the \
Ricci tensor.";


(* Killing vectors *)

KillingVectorQ::usage = 
  "KillingVectorQ[tensor] return True if the tensor is defined to be a Killing vector";

KillingVectorOf::usage = 
  "Option for DefTensor. If the tensor is to be a Killing vector, the \
option should be a metric. (i.e. KillingVectorOf -> metric)";


Begin["`Private`"]

ConstantExprQ[(Plus | Times | _?ScalarFunctionQ)[args__]] := And @@ Map[ConstantExprQ, List@args];
ConstantExprQ[x_] := ConstantQ[x];

(*************************)
(* Clear automatic rules *)
(*************************)


SetAttributes[ClearAutomaticRules, HoldFirst];

ClearAutomaticRules[symbol_Symbol, rules_List, options___?OptionQ] := Module[{
	pos, upvs, downvs, othervs, unknownvs, up, down, other, 
    verbose = Verbose /. CheckOptions[options] /. Options[AutomaticRules]
}, 
	pos[list1_, list2_] := Position[Map[Intersection[list1, {#}] =!= {} &, list2], True];

	(* Get the positions for verbose info. *)
	downvs 		= Flatten@pos[DownValues[symbol], rules];
	upvs 		= Flatten@pos[UpValues[symbol], rules];
	othervs 	= Flatten@pos[$Rules, rules];
	unknownvs 	= Complement[Range[Length[rules]], downvs, upvs, othervs];

	(* Get the positions for deleting rules. *)
	down 	= pos[rules, DownValues[symbol]];
	up 		= pos[rules, UpValues[symbol]];
	other 	= pos[rules, $Rules];
	
	(* The actual work. *)
	DownValues[symbol] 	= Delete[DownValues[symbol], down];
	UpValues[symbol] 	= Delete[UpValues[symbol], up];
	$Rules 				= Delete[$Rules, other];

	(* Verbose output. *)
	If[Length[downvs] > 0 && verbose, 
		Print["   Rules ", Shallow[downvs], " have been removed as DownValues for ", symbol, "."]];
	If[Length[upvs] > 0 && verbose, 
		Print["   Rules ", Shallow[upvs], " have been removed as UpValues for ", symbol, "."]];
	If[Length[othervs] > 0 && verbose, 
		Print["   Rules ", Shallow[othervs], " have been removed as generic rules."]];
	If[Length[unknownvs] > 0 && verbose, 
		Print["   Rules ", Shallow[unknownvs], " were not automatic rules for ", symbol, "."]];

];


ClearCurvatureRelations[cd_?CovDQ, options___?OptionQ] := (
	ClearAutomaticRules[Evaluate[GiveSymbol[Ricci, cd]], CurvatureRelations[cd, Ricci], options];
	ClearAutomaticRules[Evaluate[GiveSymbol[Riemann, cd]], CurvatureRelations[cd, Riemann], options];
);

SetCurvatureRelations[cd_?CovDQ, options___?OptionQ] := (
	AutomaticRules[Evaluate[GiveSymbol[Ricci, cd]], CurvatureRelations[cd, Ricci], options];
	AutomaticRules[Evaluate[GiveSymbol[Riemann, cd]], CurvatureRelations[cd, Riemann], options];
);

CurvatureRelationsQ[cd_?CovDQ] := Module[
	{
		ricci 	= GiveSymbol[Ricci, cd], 
		riemann	= GiveSymbol[Riemann, cd]
	},
	Complement[
		CurvatureRelations[cd, Ricci], 
		DownValues[Evaluate[ricci]], UpValues[Evaluate[ricci]]
	] === {} &&	Complement[
		CurvatureRelations[cd, Riemann], 
		DownValues[Evaluate[riemann]], 
		UpValues[Evaluate[riemann]]
	] === {}
];



(****************)
(* Other stuff  *)
(****************)

DerivativeOrder[expr_] := Total[DerivativeOrder[expr, #] & /@ $CovDs];

DerivativeOrder[expr_,cd_?CovDQ] /; FreeQ[expr, Plus | Power] := Module[
	{
		ordertwo 	= GiveSymbol[#,cd]& /@ {Riemann,Ricci,RicciScalar,Einstein,Weyl,Schouten,EinsteinCC,SchoutenCC},
		christoffel = GiveSymbol[Christoffel,cd]
	},	
 	2 Count[expr, (Alternatives@@ordertwo)[___], {0, Infinity}, Heads -> True] 
 		+ Count[expr, (cd[_][_])|(christoffel[___]), {0, Infinity}, Heads -> True]
];

DerivativeOrder[Power[y_, z_Integer],cd_?CovDQ] := z DerivativeOrder[y,cd];

DerivativeOrder[x_ * y_,cd_?CovDQ] := DerivativeOrder[x,cd] + DerivativeOrder[y,cd];

DerivativeOrder[x_?ConstantExprQ,cd_?CovDQ] := 0



SortedCovDsQ[expr_,cd_?CovDQ] := FreeQ[expr, cd[b_]@cd[a_]@_ /; DisorderedPairQ[a, b]];

SortedCovDsQ[expr_] := And @@ ( SortedCovDsQ[expr, #]& /@ DeleteCases[$CovDs,PD] );



(**********************)
(* Curvature tensors  *)
(**********************)

xTension["xTras`xTensor`", DefMetric, "End"] := xTrasxTensorDefMetric;

xTrasxTensorDefMetric[signdet_, metric_[-a_, -b_], cd_, options___]:= Module[
	{
		M,D,einsteincc,rs
	},	
	M 			= ManifoldOfCovD[cd];
	D 			= DimOfManifold[M];
	einsteincc 	= GiveSymbol[EinsteinCC,cd];
	rs 			= GiveSymbol[RicciScalar,cd];
	
	(* Define the new curvature tensors. *)
	DefTensor[GiveSymbol[Schouten,cd][-a, -b], 
		M, Symmetric[{-a, -b}], PrintAs -> GiveOutputString[Schouten,cd], Master->cd];
	DefTensor[GiveSymbol[SchoutenCC,cd][LI[_],-a, -b], 
		M, Symmetric[{-a, -b}], PrintAs -> GiveOutputString[Schouten,cd], Master->cd];	
	DefTensor[einsteincc[LI[_],-a, -b], 
		M, Symmetric[{-a, -b}], PrintAs -> GiveOutputString[Einstein,cd], Master->cd];
	
	(* Some identities for the cosmological Einstein tensor. *)
	cd[c_]@einsteincc[LI[_],___,d_,___] /; c === ChangeIndex[d] ^= 0;
	einsteincc[LI[K_], c_, d_] /; c === ChangeIndex[d] := (1/ 2 (D-2)(D-1) D K + (1-D/2) rs[]);
];

(* TODO: undefmetric hook *)

GiveOutputString[Schouten, covd_] := StringJoin["S", "[", SymbolOfCovD[covd][[2]], "]"];

SchoutenToRicci[expr_, cd_?CovDQ] := Module[{d, ricci, rs, schouten, metric},
	ricci 		= GiveSymbol[Ricci, cd];
	rs 			= GiveSymbol[RicciScalar, cd];
	schouten 	= GiveSymbol[Schouten, cd];
	metric 		= MetricOfCovD@cd;
	d 			= DimOfManifold@ManifoldOfCovD@cd;
	expr /. schouten[inds__] :> ricci[inds]/(d - 2) - metric[inds] rs[] / (2 (d - 1) (d - 2))
];

SchoutenToRicci[expr_] := Fold[SchoutenToRicci, expr, DeleteCases[$CovDs, PD]];

RicciToSchouten[expr_, cd_?CovDQ] := Module[{d, ricci, rs, schouten, metric},
	ricci 		= GiveSymbol[Ricci, cd];
	rs 			= GiveSymbol[RicciScalar, cd];
	schouten	= GiveSymbol[Schouten, cd];
	metric 		= MetricOfCovD@cd;
	d 			= DimOfManifold@ManifoldOfCovD@cd;
	expr /. ricci[inds__] :> schouten[inds] (d - 2) + metric[inds] rs[] /(2 (d - 1))
];

RicciToSchouten[expr_] := Fold[RicciToSchouten, expr, DeleteCases[$CovDs, PD]];

SchoutenCCToRicci[expr_, cd_?CovDQ] := Module[{d, ricci, rs, schouten, metric},
	ricci 		= GiveSymbol[Ricci, cd];
	rs 			= GiveSymbol[RicciScalar, cd];
	schouten 	= GiveSymbol[SchoutenCC, cd];
	metric 		= MetricOfCovD@cd;
	d 			= DimOfManifold@ManifoldOfCovD@cd;
	expr /. schouten[LI[K_],inds__] :> ricci[inds]/(d - 2) - metric[inds] rs[] / (2 (d - 1) (d - 2)) - 1/2 K  metric[inds]
];

SchoutenCCToRicci[expr_] := Fold[SchoutenCCToRicci, expr, DeleteCases[$CovDs, PD]];

RicciToSchoutenCC[K_][expr_, cd_?CovDQ] := Module[{d, ricci, rs, schouten, metric},
	ricci 		= GiveSymbol[Ricci, cd];
	rs 			= GiveSymbol[RicciScalar, cd];
	schouten 	= GiveSymbol[SchoutenCC, cd];
	metric 		= MetricOfCovD@cd;
	d 			= DimOfManifold@ManifoldOfCovD@cd;
	expr /. ricci[inds__] :> schouten[LI[K],inds] (d - 2) + metric[inds] rs[] /(2 (d - 1)) + 1/2 (d-2) K metric[inds]
];

RicciToSchoutenCC[K_][expr_] := Fold[RicciToSchoutenCC[K], expr, DeleteCases[$CovDs, PD]];

EinsteinCCToRicci[expr_, cd_?CovDQ] := Module[{ricci, rs, einsteincc, d, metric},
	d 			= DimOfManifold@ManifoldOfCovD@cd;
	metric 		= MetricOfCovD[cd];
	ricci 		= GiveSymbol[Ricci, cd];
	rs 			= GiveSymbol[RicciScalar, cd];
	einsteincc 	= GiveSymbol[EinsteinCC, cd];
	expr /. einsteincc[LI[K_], inds__] :> (ricci[inds] + 1/2 metric[inds] (-rs[] + (d - 2) (d - 1) K))
];

EinsteinCCToRicci[expr_] := Fold[EinsteinCCToRicci, expr, DeleteCases[$CovDs, PD]];

RicciToEinsteinCC[K_][expr_, cd_?CovDQ] := Module[{ricci, rs, einsteincc, d, metric},
	d 			= DimOfManifold@ManifoldOfCovD@cd;
	metric 		= MetricOfCovD[cd];
	ricci 		= GiveSymbol[Ricci, cd];
	rs 			= GiveSymbol[RicciScalar, cd];
	einsteincc 	= GiveSymbol[EinsteinCC, cd];
	expr /. ricci[inds__] :> (einsteincc[LI[K], inds] - 1/2 metric[inds] (-rs[] + (d - 2) (d - 1) K))
];

RicciToEinsteinCC[K_][expr_] := Fold[RicciToEinsteinCC[K], expr, DeleteCases[$CovDs, PD]];


(***************)
(* Simplifying *)
(***************)

PreferBoxOfRule[tensor_] := Flatten[PreferBoxOfRule[tensor, #] & /@ $CovDs ];
PreferBoxOfRule[tensor_, CD_?CovDQ] := 
{
	bigexpr : CD[-b_]@CD[b_]@CD[a_][expr_] 
		/; (! FreeQ[expr, tensor] && FreeQ[expr, CD[ChangeIndex[a]][_]]) 
		:> CommuteCovDs[bigexpr, CD, {a, b}],
	bigexpr : CD[b_]@CD[-b_]@CD[a_][expr_] 
		/; (! FreeQ[expr, tensor] && FreeQ[expr, CD[ChangeIndex[a]][_]]) 
		:> CommuteCovDs[bigexpr, CD, {a, -b}],
	bigexpr : CD[b_]@CD[a_][expr_] 
		/; (! FreeQ[expr, tensor] && FreeQ[expr, CD[ChangeIndex[a]][_]] && !FreeQ[expr, CD[ChangeIndex[b]][_]]) 
		:> CommuteCovDs[bigexpr, CD, {a, b}]
};

PreferBoxOf[tensor_][expr_] := expr //. PreferBoxOfRule[tensor];
PreferBoxOf[tensor_, CD_?CovDQ][expr_] := expr //. PreferBoxOfRule[tensor, CD]; 


(* The next bit is thanks to Leo Stein, see 
   http://groups.google.com/group/xact/browse_thread/thread/31e959cbee8d1848/690def9618ff519c *)
PreferDivOfRule[tens_] := Flatten[PreferDivOfRule[tens, #] & /@ $CovDs ];
PreferDivOfRule[tens_, CD_?CovDQ] := 
{	
	bigexpr : CD[b_]@CD[a_][expr_] 
		/; !FreeQ[expr, tens[___, ChangeIndex[b], ___]] && FreeQ[expr, tens[___, ChangeIndex[a], ___]] 
		:> CommuteCovDs[bigexpr, CD, {a, b}]
};

PreferDivOf[tensor_][expr_] := expr //. PreferDivOfRule[tensor];
PreferDivOf[tensor_, CD_?CovDQ][expr_] := expr //. PreferDivOfRule[tensor, CD]; 


DivFreeQ[expr_, tens_] := And @@ (DivFreeQ[expr, tens, #] & /@ $CovDs);
DivFreeQ[expr_, tens_, CD_?CovDQ] := 
	FreeQ[expr, CD[a_][inner_] /; ! FreeQ[inner, tens[___, ChangeIndex[a], ___]]];

RicciDivRule[cd_?CovDQ] := Module[
	{
		ricci	= GiveSymbol[Ricci,cd],
		rs		= GiveSymbol[RicciScalar,cd],
		vb		= First@VBundlesOfCovD[cd],
		a,b,pa,pb,MR
	},
	{a,b}	= Table[DummyIn[vb],{2}];
	pb = With[{vbpmq=GiveSymbol[vb,"`pmQ"]},
		PatternTest[Pattern[Evaluate@b,Blank[]],vbpmq]
	];
	pa = With[{vbq=GiveSymbol[vb,"`Q"]},
		PatternTest[Pattern[Evaluate@a,Blank[Symbol]],vbq]
	];
	
	MR[der_,head_,i1_,i2_,i3_] := RuleDelayed[
		HoldPattern[der[i1]@head[i2,i3]],
		Evaluate[1/2 cd[b]@rs[]]
	];
	FoldedRule[
		PreferDivOfRule[ricci, cd],
		{
			MR[cd,ricci,pa,-pa,pb],
			MR[cd,ricci,-pa,pa,pb],
			MR[cd,ricci,pa,pb,-pa],
			MR[cd,ricci,-pa,pb,pa]
		}
	]
];


RiemannDivRule[cd_?CovDQ] := Module[
	{
		ricci	= GiveSymbol[Ricci,cd],
		riemann	= GiveSymbol[Riemann,cd],
		vb		= First@VBundlesOfCovD[cd],
		a,b,c,d,pa,pb,pc,pd,MR
	},
	
	{a,b,c,d}	= Table[DummyIn[vb],{4}];
	{pa,pb,pc}	= With[{vbpmq=GiveSymbol[vb,"`pmQ"]},
		PatternTest[Pattern[#,Blank[]],vbpmq]&/@{a,b,c}
	];
	pd = With[{vbq=GiveSymbol[vb,"`Q"]},
		PatternTest[Pattern[Evaluate@d,Blank[Symbol]],vbq]
	];
	
	MR[der_,head_,i1_,i2_,i3_,i4_,i5_,sign_] := RuleDelayed[
		HoldPattern[der[i1]@head[i2,i3,i4,i5]],
		Evaluate[sign*$RicciSign*(cd[b]@ricci[a,c]-cd[c]@ricci[a,b])]
	];
	FoldedRule[
		PreferDivOfRule[riemann, cd],
		{
			MR[cd,riemann,-pd,pd,pa,pb,pc,1],
			MR[cd,riemann,pd,-pd,pa,pb,pc,1],
			MR[cd,riemann,-pd,pa,pd,pb,pc,-1],
			MR[cd,riemann,pd,pa,-pd,pb,pc,-1],
			MR[cd,riemann,-pd,pb,pc,pd,pa,1],
			MR[cd,riemann,pd,pb,pc,-pd,pa,1],
			MR[cd,riemann,-pd,pb,pc,pa,pd,-1],
			MR[cd,riemann,pd,pb,pc,pa,-pd,-1]
		}
	]
];


(*******************)
(* Killing vectors *)
(*******************)


KillingVectorQ[expr_] := False
KillingVectorOf[expr_] := None

Unprotect[xAct`xTensor`DefTensor];
If[FreeQ[Options[xAct`xTensor`DefTensor], KillingVectorOf], 
	Options[xAct`xTensor`DefTensor] ^= Append[Options[xAct`xTensor`DefTensor], KillingVectorOf -> None];
, 
	Null;
];
Protect[xAct`xTensor`DefTensor];


xTension["xTras`", DefTensor, "End"] := xTrasDefTensor;


xTrasDefTensor[head_[indices___], dependencies_, sym_, options___] :=
	DefKillingVector[head[indices], KillingVectorOf /. CheckOptions[options] /. Options[DefTensor]];

(* The pattern only matches if the index belongs to the correct tangent bundle. *)
DefKillingVector[xi_[L1:(-LI[___]|LI[___])...,ind_,L2:(-LI[___]|LI[___])...], metric_?MetricQ] := Module[
	{vb, cd,riemann,l1patt,l2patt},

	If[$DefInfoQ,
		Print[
			"** Defining ", PrintAs@xi, 
			" to be a Killing vector of the metric ", PrintAs@metric, "."
		];
	];

	(* Set up some variables. *)
	vb 		= VBundleOfMetric@metric;
	cd 		= CovDOfMetric[metric];
	riemann = GiveSymbol[Riemann, cd];
	l1patt 	= PatternSequence[L1]/.LI[___]->LI[___];
	l2patt 	= PatternSequence[L2]/.LI[___]->LI[___];

	(* Set the symmetry. Thanks to JMM for pointing out how this works. *)
	SymmetryOf[cd[x_][xi[l1:l1patt,y_,l2:l2patt]]] ^:= Symmetry[
		2,
		cd[xAct`xTensor`Private`slot[2]][xi[l1,xAct`xTensor`Private`slot[1],l2]], 
		{xAct`xTensor`Private`slot[1] -> y, xAct`xTensor`Private`slot[2] -> x},
		Antisymmetric[{1, 2}]
	]; 

	(* Attach the rules and the rest. *)
	cd[c_]@cd[b_]@xi[l1:l1patt,a_,l2:l2patt] := Module[
		{d = DummyIn@vb}, 
		riemann[a,b,c,d] xi[l1,-d,l2]
	];
	Unprotect[xAct`xTensor`LieD];
	LieD[xi[l1patt,_,l2patt]][metric[__]] = 0;
	LieD[xi[l1patt,_,l2patt],_][metric[__]] = 0;
	Protect[xAct`xTensor`LieD];
 
	KillingVectorOf[xi] ^= metric;
	KillingVectorQ[xi] ^= True;
] /; AIndexQ[ind, VBundleOfMetric@metric];


End[]
EndPackage[]