ConstantSymbolsOf::usage =
	"ConstantSymbolsOf[expr] returns a list of all non-numeric constant symbols in expr.";

DefNiceConstantSymbol::usage =
	"DefNiceConstantSymbol[ head[i1,-i2,...] ] defines the constant symbol headi1i2 \
that prints as head^i1_i2.";

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

DerivativeOrder::usage = "\
DerivativeOrder[expr] gives the order of derivatives of expr.\n\
DerivativeOrder[expr,cd] only counts the covariant derivative cd.";

SortedCovDsQ::usage =
	"SortedCovDsQ[expr] returns True if the expression has all its covariant \
derivatives sorted, and False otherwise. \
\n\nSortedCovDsQ[expr,CD] only checks the covariant derivative CD.";


(* Extra curvature tensors *)

ToRiemann::usage =
	"ToRiemann[expr] converts Ricci tensors, Kretschmann scalars, Weyl tensors, \
symmetrized Riemann tensors, any Riemann tensor with down indices of a frozen \
metric, and gradients of Christoffel symbols to Riemann tensors.";

RicciToRiemann::usage =
	"RicciToRiemann[expr] converts Ricci tensors and scalars in expr to contractions of \
Riemann tensors.";

KretschmannToRiemann::usage =
	"KretschmannToRiemann[expr] converts Kretschmann scalars in expr to contractions \
of Riemann tensors.";

SymRiemann::usage =
	"SymRiemann is a reserved word in xTras. It is used to generated the name \
of the symmetrized Riemann tensor.";

RiemannToSymRiemann::usage =
	"RiemannToSymRiemann[expr,cd] converts Riemann tensors of cd to symmetrized Riemann tensors.";

SymRiemannToRiemann::usage =
	"SymRiemannToRiemann[expr,cd] converts symmetrized Riemann tensors of cd to Riemann tensors.";

ToRicci::usage = "\
ToRicci[expr] converts all curvature tensors of rank two in expr to Ricci tensors and scalars. \n\
ToRicci[expr, cd] converts only for curvature tensors of the covariant derivative cd."; 

Schouten::usage = 
  "Schouten is a reserved word in xTras. It is used to generate the \
name of the Schouten curvature tensor associated to a connection \
acting on a tangent bundle.";

SchoutenToRicci::usage = "\
SchoutenToRicci[expr] converts all Schouten tensors in expr to Ricci tensors and scalars.\n\
SchoutenToRicci[expr,cd] converts only Schouten tensors of the covariant derivative cd.";

RicciToSchouten::usage = "\
RicciToSchouten[expr] converts all Ricci tensors in expr to Schouten tensors.\n\
RicciToSchouten[expr,cd] converts only Ricci tensors of the covariant derivative cd.";

SchoutenCC::usage = 
  "SchoutenCC is a reserved word in xTras. It is used to generate the \
name of the cosmological Schouten curvature tensor associated to a connection \
acting on a tangent bundle.";

SchoutenCCToRicci::usage = "\
SchoutenCCToRicci[expr] converts all cosmological Schouten tensors in expr to Ricci tensors and scalars.\n\
SchoutenCCToRicci[expr,cd] converts only cosmological Schouten tensors of the covariant derivative cd.";

RicciToSchoutenCC::usage = "\
RicciToSchoutenCC[K][expr] converts all Ricci tensors of in expr to cosmological Schouten tensors with cosmological constant K.\n\
RicciToSchoutenCC[K][expr,cd] converts only Ricci tensors of the covariant derivative cd.";

EinsteinCC::usage = 
  "EinsteinCC is a reserved word in xTras. It is used to generate the \
name of the cosmological Einstein curvature tensor associated to a connection \
acting on a tangent bundle.";

EinsteinCCToRicci::usage = "\
EinsteinCCToRicci[expr] converts all cosmological Einstein tensors in expr to Ricci tensors and scalars.\n\
EinsteinCCToRicci[expr,cd] converts only cosmological Einstein tensors of the covariant derivative cd.";

RicciToEinsteinCC::usage = "\
RicciToEinsteinCC[K][expr] converts all Ricci tensors in expr to cosmological Einstein tensors with cosmological constant K.\n\
RicciToEinsteinCC[K][expr,cd] converts only Ricci tensors of the covariant derivative cd.";



(* Simplifying *)

CurvatureRelationsBianchi::usage = "\
CurvatureRelationsBianchi[cd] gives the contracted Bianchi identities for the curvature tensor\
of the covariant derivative cd.\n\
CurvatureRelationsBianchi[cd, Riemann] gives only the identities for the Riemann tensor.\n\
CurvatureRelationsBianchi[cd, Ricci] gives only the identities for the Ricci tensor.";

SortCovDsToBox::usage = "\
SortCovDsToBox[tensor][expr] commutes derivatives in expr such that all possible boxes act on the specified tensor.\n\
SortCovDsToBox[tensor,cd][expr] only commutes the covariant derivative cd.";

SortCovDsToDiv::usage = "\
SortCovDsToDiv[tensor][expr] commutes derivatives in expr such that any derivatve contracted with tensor acts directy on the specified tensor.\n\
SortCovDsToDiv[tensor,cd][expr] only commutes the covariant derivative cd.";

DivFreeQ::usage = 
  "DivFreeQ[expr,tensor,CD] returns True if expr does not contain a \
divergence of the given tensor w.r.t. the given CD after arbritrary \
commutations of the covariant derivatives."; 



(* Killing vectors *)

KillingVectorQ::usage = "\ 
KillingVectorQ[tensor,metric] returns True if the tensor is defined to be a Killing vector of\
the given metric, and False otherwise.\n\
KillingVectorQ[tensor] returns KillingVectorQ[tensor, MetricOfKillingVector[tensor]].";

KillingVectorOf::usage = 
  "KillingVectorOf is an option for DefTensor. Setting the value of KillingVectorOf to a metric while defining a vector \
defines the vector as a Killing vector of that metric (e.g. \"DefTensor[\[Xi][a], KillingVectorOf->metric]\").";

MetricOfKillingVector::usage =
	"MetricOfKillingVector[vector] returns the metric of which the given vector is a Killing vector,\
and None if it is not a Killing vector.";


(* Pseudo index-free notation. *)

IndexFree::usage =
	"IndexFree[expr] indicates that expr is in pseudo index-free notation.";

FromIndexFree::usage =
	"FromIndexFree[expr] inserts indices in the index-free expression expr.";
	
ToIndexFree::usage =
	"ToIndexFree[expr] removes indices from the index-full expression expr.";

TermsOf::usage = 
	"TermsOf[expr] gives all the different tensorial terms of expr in index-free notation.";

GradChristoffelToRiemann::usage =
	"GradChristoffelToRiemann[expr] rewrites partial derivatives of Christoffel symbols \
to Riemann tensors.";


(* Symmetrized covariant derivatives *)

SymCovDQ::usage =
	"SymCovD[cd] gives True is cd is a covariant derivative that can be symetrized, \
and False otherwise. \
It is also a boolean option for DefCovD specifying whether the derivative \
can be symmetrized or not. The default is False.";

ExpandSymCovDs::usage =
	"ExpandSymCovDs[expr] expands all symmetric covariant derivatives in terms of ordinary ones.\
ExpandSymCovDs[expr,cd] expands only symmetric covariant derivatives associated \
to the covariant derivative cd.";

SymmetrizeCovDs::usage =
	"SymmetrizeCovDs[expr] converts all covariant derivatives in expr into  \
symmetrized covariant derivatives by commuting them. \
SymmetrizeCovDs[expr, cd] only convert the covariant derivative cd.";

$AutoSymmetrizeCovDs::usage =
	"$AutoSymmetrizeCovDs is a boolean setting determining whether symmetrizable \
covariant derivatives are automatically symmetrized or not.";

$SymCovDCache::usage =
	"$SymCovDCache stores rules for symmetrizing covariant derivatives.";

ClearSymCovDCache::usage =
	"ClearSymCovDCache clear the cache of SymmetrizeCovDs by setting $SymCovDCache to {}.";

(* Create RemoveConstants in the non-private context, because we need it here. *)
RemoveConstants::usage = "bla";
CollectTensors::usage = "bla";
CollectMethod::usage = "bla";
SimplifyMethod::usage = "bla"; 
RemoveTensorWrapper::usage = "bla"; 
TensorWrapper::usage = "bla";
TableauSymmetric::usage = "bla";
ManifestSymmetry::usage = "bla";



Begin["`Private`"]

ConstantExprQ[(Plus | Times | _?ScalarFunctionQ)[args__]] := And @@ Map[ConstantExprQ, List@args];
ConstantExprQ[x_] := ConstantQ[x];


ConstantSymbolsOf[expr__] := 
	(* Get all constant symbols, and select the non-numeric ones (we don't want things like Pi) *)
	Select[
		Union@Cases[{expr}, _Symbol?ConstantSymbolQ, {0,Infinity}, Heads->True],
		!NumericQ[#]&
	]

DefNiceConstantSymbol[ symbol_ ] := 
	DefNiceConstantSymbol[ symbol, {}, {} ];

DefNiceConstantSymbol[ symbol_, down_List ] := 
	DefNiceConstantSymbol[ symbol, down, {} ];

DefNiceConstantSymbol[ symbol_, down_ ] /; Head[down] =!= List := 
	DefNiceConstantSymbol[ symbol, {down}, {} ];

DefNiceConstantSymbol[ symbol_, down_ , up_ ] /; Head[down] =!= List && Head[up] =!= List := 
	DefNiceConstantSymbol[ symbol, {down}, {up} ]; 

DefNiceConstantSymbol[ symbol_, down_List, up_List ] := 
	With[
		{
			upstring 	= StringJoin[ToString/@up],
			downstring	= StringJoin[ToString/@down]
		},
		Block[{$DefInfoQ = False},
			Quiet[
				DefConstantSymbol[
					SymbolJoin[symbol, downstring, upstring],
					PrintAs -> StringJoin[
						"\!\(\*SubsuperscriptBox[\(",
						ToString@symbol,
						"\), \(",
						downstring,
						"\), \(",
						upstring,
						"\)]\)"
					]
				],
				{ValidateSymbol::used}
			] (* Quiet *)
		]; (* Block *)
		SymbolJoin[symbol, downstring, upstring]
	]; (* With *)

(**********************************)
(*      Index-free notation       *)
(**********************************)


IndexFree@IndexFree[x_] := IndexFree[x];
IndexFree[list_List] := IndexFree /@ list;

(* Formatting. *)
MakeBoxes[IndexFree[expr_], StandardForm] := 
	ToBoxes[ 
		expr /. x_?xTensorQ :> PrintAs[x] //. cd_?CovDQ :> Last@SymbolOfCovD[cd], 
		StandardForm
	] /. "["|"]" -> Sequence[];

FromIndexFree[expr_] :=
	Block[
		{
			$AutoSymmetrizeCovDs = False
		},
		expr /. HoldPattern[IndexFree[sub_]] :> FromIndexFree1[sub]
	];

FromIndexFree1[expr_] := ScreenDollarIndices[ FromIndexFreeTensors@FromIndexFreeSymCovDs@FromIndexFreeCovDs@expr ];
	
FromIndexFreeTensors[expr_] := 
	expr /. RuleDelayed[
		(x_?xTensorQ)^Optional[n_Integer],
		Product[x @@ DummyIn /@ SlotsOfTensor@x, {n}] 
	];

FromIndexFreeSymCovDs[expr_]:=
	expr //. cd_?SymCovDQ[a__][cd_[b__][x_]] :> cd[a,b][x];

FromIndexFreeCovDs[expr_] :=
	expr /.RuleDelayed[
				(cd_?CovDQ[x_])^Optional[n_Integer],
				Product[cd[-DummyIn@First@VBundlesOfCovD@cd][FromIndexFreeCovDs@x], {n}]
	]; 

ToIndexFree[expr_] :=
	IndexFree[expr //.cd_?SymCovDQ[_,rest__][x_]:>cd@cd[rest][x] //. cd_?CovDQ[_][x_] :> cd[x] /. x_?xTensorQ[___] :> x];


ClearAll[TermsOf]
TermsOf[expr_List] 	:= Union@Flatten[TermsOf /@ expr];
TermsOf[expr_Plus] 	:= Union@TermsOf[List @@ expr];
TermsOf[expr_]		:=
	Union @ Cases[
		CollectTensors[
			expr, 
			CollectMethod -> Identity,
			SimplifyMethod -> Identity, 
			RemoveTensorWrapper -> False
		],
		tw:HoldPattern[TensorWrapper[_]] :> ToIndexFree @ RemoveTensorWrapper @ tw,
		{0, Infinity},
		Heads -> True
	];



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
	ClearAutomaticRules[Evaluate[GiveSymbol[SymRiemann, cd]], CurvatureRelations[cd, SymRiemann], options];
);

SetCurvatureRelations[cd_?CovDQ, options___?OptionQ] := (
	AutomaticRules[Evaluate[GiveSymbol[Ricci, cd]], CurvatureRelations[cd, Ricci], options];
	AutomaticRules[Evaluate[GiveSymbol[Riemann, cd]], CurvatureRelations[cd, Riemann], options];
	AutomaticRules[Evaluate[GiveSymbol[SymRiemann, cd]], CurvatureRelations[cd, SymRiemann], options];
);

CurvatureRelationsQ[cd_?CovDQ] := With[
	{
		ricci 	 = GiveSymbol[Ricci, cd], 
		riemann	 = GiveSymbol[Riemann, cd],
		sriemann = GiveSymbol[SymRiemann, cd]
	},
	Complement[
		CurvatureRelations[cd, Ricci], 
		DownValues[ricci], UpValues[ricci]
	] === {} &&	Complement[
		CurvatureRelations[cd, Riemann], 
		DownValues[riemann], 
		UpValues[riemann]
	] === {} && Complement[
		CurvatureRelations[cd, SymRiemann], 
		DownValues[sriemann], 
		UpValues[sriemann]
	] === {}
];



(****************)
(* Other stuff  *)
(****************)

DerivativeOrder[x_Plus, cd_?CovDQ] := Max[DerivativeOrder[#,cd]& /@ List@@x];

DerivativeOrder[Power[y_, z_Integer], cd_?CovDQ] := z DerivativeOrder[y,cd];

DerivativeOrder[x_ * y_,cd_?CovDQ] := DerivativeOrder[x,cd] + DerivativeOrder[y,cd];

DerivativeOrder[x_?ConstantExprQ,cd_?CovDQ] := 0

DerivativeOrder[x_List, cd___?CovDQ] := DerivativeOrder[#,cd]& /@ x;

DerivativeOrder[expr_, cd_?CovDQ] /; FreeQ[expr, Plus | Power] := Module[
	{
		orderfour = GiveSymbol[#,cd]& /@ {Kretschmann},
		ordertwo  = GiveSymbol[#,cd]& /@ {Riemann,Ricci,RicciScalar,Einstein,Weyl,Schouten,EinsteinCC,SchoutenCC},
		orderone  = GiveSymbol[#,cd]& /@ {Christoffel, Torsion}
	},
	4 Count[expr, (Alternatives@@orderfour)[___], {0, Infinity}, Heads -> True]
	+ 2 Count[expr, (Alternatives@@ordertwo)[___], {0, Infinity}, Heads -> True] 
 		+ Count[expr, (cd[_][_])|((Alternatives@@orderone)[___]), {0, Infinity}, Heads -> True]
];
DerivativeOrder[expr_] := Max[DerivativeOrder[expr, #] & /@ $CovDs];



SortedCovDsQ[expr_,cd_?CovDQ] := FreeQ[expr, cd[b_]@cd[a_]@_ /; DisorderedPairQ[a, b]];

SortedCovDsQ[expr_] := And @@ ( SortedCovDsQ[expr, #]& /@ DeleteCases[$CovDs,PD] );



(**********************)
(* Curvature tensors  *)
(**********************)

CurvatureRelationsBianchi[] := Apply[Join, Map[CurvatureRelationsBianchi, $CovDs]];
CurvatureRelationsBianchi[covd_Symbol?CovDQ] := 
  Join[CurvatureRelationsBianchi[covd, Riemann], CurvatureRelationsBianchi[covd, Ricci]];
CurvatureRelationsBianchi[__] = {};

xTension["xTras`xTensor`", DefCovD, "End"] := xTrasxTensorDefCovD;

xTrasxTensorDefCovD[cd_[ind_], vbundles_, options___?OptionQ] := With[
	{
		tbundle 	= VBundleOfIndex[ind], 
		M 			= BaseOfVBundle@VBundleOfIndex[ind],
		indices 	= GetIndicesOfVBundle[VBundleOfIndex[ind], 9],
		metricQ		= (FromMetric	/. CheckOptions[options] /. Options[DefCovD]) =!= Null,
		torsionQ	= Torsion		/. CheckOptions[options] /. Options[DefCovD],
		cd2			= ExtendedFrom	/. CheckOptions[options] /. Options[DefCovD],
		definfo		= DefInfo 		/. CheckOptions[options] /. Options[DefCovD],
		ot			= OrthogonalTo 	/. CheckOptions[options] /. Options[DefCovD],
		pw			= ProjectedWith	/. CheckOptions[options] /. Options[DefCovD],
		curvrels	= CurvatureRelations /. CheckOptions[options] /. Options[DefCovD]
	},
	With[
		{
			info		= If[definfo =!= False, $DefInfoQ, False],
			symQ		= metricQ && !torsionQ,
			curvQ		= CurvatureQ[cd, tbundle],
			extQ		= cd2 =!= Null && MetricOfCovD[cd2] =!= Null,
			D 			= DimOfManifold@M,
			einsteincc 	= GiveSymbol[EinsteinCC,cd],
			rs 			= GiveSymbol[RicciScalar,cd],
			riemann		= GiveSymbol[Riemann,cd],
			sriemann	= GiveSymbol[SymRiemann,cd],
			ricci		= GiveSymbol[Ricci,cd],
			torsion		= GiveSymbol[Torsion,cd],
			orthogonalQ = (ot =!= {}),
			projectedQ	= (pw =!= {}),
			a			= indices[[1]],
			b			= indices[[2]],
			c			= indices[[3]],
			d			= indices[[4]],
			e			= indices[[5]],
			i1d 		= indices[[6]],
			i2d 		= indices[[7]],
			i3d 		= indices[[8]],
			i4d 		= indices[[9]]
		},
		With[
			{
				vanishQ = ! curvQ || If[IntegerQ[D], D < 2, False],
				sym		= If[symQ, Symmetric[{-a, -b}], StrongGenSet[{}, GenSet[]]],
				symstr	= If[symQ, "symmetric ", "non-symmetric "],
				cpat 	= PatternIndex[c, AIndex, Null, tbundle],
				dpat 	= PatternIndex[d, AIndex, Null, tbundle],
				vector 	= If[orthogonalQ, ot[[1, 0]]],
				projector = If[projectedQ, pw[[1, 0]]]
			},
			
			(* Define the symmetrized Riemann tensor only if it has full symmetries,
			   i.e. if there's a metric and no torsion. 
			   Although it is still possible to define it without full symmetries,
			   it doesn't make much sense to do so because the symmetrized Riemann
			   tensor is most commonly used when it has full symmetries. *)
			If[metricQ && !torsionQ,
				If[extQ,
					Evaluate @ GiveSymbol[SymRiemann,cd] = GiveSymbol[SymRiemann,cd2];
					,
					DefTensor[
						sriemann[-a,-b,-c,-d],M,
						TableauSymmetric[{{-a,-b},{-c,-d}}, ManifestSymmetry -> Symmetric],
						PrintAs -> GiveOutputString[SymRiemann,cd], VanishingQ -> vanishQ,
						Master  -> cd, DefInfo -> If[info, {"symmetrized Riemann tensor",""}, False],
						OrthogonalTo 	:> If[orthogonalQ, {vector[a], vector[b], vector[c], vector[d]}, {}],
						ProjectedWith 	:> If[projectedQ, {projector[a, -i1d], projector[b, -i2d], projector[c, -i3d], projector[d, -i4d]}, {}]
					];
				]
			];			
			
			(* The 3 new curvature tensors can only be defined when there is a metric. *)
			If[extQ,
				(* Extend them from another covd. *)
				( Evaluate @ GiveSymbol[#,cd] = GiveSymbol[#,cd2] )& /@ {Schouten, SchoutenCC, EinsteinCC};
				,
				If[metricQ,
					(* Define the new curvature tensors. *)
					DefTensor[
						GiveSymbol[Schouten,cd][-a, -b], M, sym, 
						PrintAs -> GiveOutputString[Schouten,cd], VanishingQ -> vanishQ, 
						Master  -> cd, DefInfo -> If[info, {symstr <> "Schouten tensor",""}, False],
						OrthogonalTo 	:> If[orthogonalQ, {vector[a], vector[b]}, {}],
						ProjectedWith 	:> If[projectedQ, {projector[a, -i1d], projector[b, -i2d]}, {}]
					];
					DefTensor[
						GiveSymbol[SchoutenCC,cd][LI[_],-a, -b], M, sym, 
						PrintAs -> GiveOutputString[Schouten,cd], VanishingQ -> False,
						Master  -> cd, DefInfo -> If[info, {symstr <> "cosmological Schouten tensor",""}, False],
						OrthogonalTo 	:> If[orthogonalQ, {vector[a], vector[b]}, {}],
						ProjectedWith 	:> If[projectedQ, {projector[a, -i1d], projector[b, -i2d]}, {}]
					];	
					DefTensor[
						einsteincc[LI[_],-a, -b], M, sym, 
						PrintAs -> GiveOutputString[Einstein,cd], VanishingQ -> False,
						Master  -> cd, DefInfo -> If[info, {symstr <> "cosmological Einstein tensor",""}, False],
						OrthogonalTo 	:> If[orthogonalQ, {vector[a], vector[b]}, {}],
						ProjectedWith 	:> If[projectedQ, {projector[a, -i1d], projector[b, -i2d]}, {}]
					];
					
					(* Some identities for the cosmological Einstein tensor. *)
					cd[cpat]@einsteincc[LI[_],___,dpat,___] /; c === ChangeIndex[d] ^= 0;
					einsteincc[LI[K_], cpat, dpat] /; c === ChangeIndex[d] := (1/ 2 (D-2)(D-1) D K + (1-D/2) rs[]);
				]
			];		
			
			(* Contracted Bianchi identities. *)
			If[curvQ,
				cd /: CurvatureRelationsBianchi[cd, Riemann] = MakeRule[
					{
						cd[-d]@riemann[-a,-b,-c,d],
						$RicciSign   * ( cd[-b]@ricci[-a,-c] - cd[-a]@ricci[-b,-c] ) + 
						$TorsionSign * ( riemann[-a,-d,-c,e] torsion[d,-b,-e] - riemann[-b,-d,-c,e] torsion[d,-a,-e] - ricci[-d,-c] torsion[d,-a,-b] )
					},
					MetricOn -> If[metricQ, All, None],
					UseSymmetries -> True 
				]
			];
			If[!vanishQ && metricQ,
				cd /: CurvatureRelationsBianchi[cd, Ricci] = MakeRule[
					{
						cd[-b]@ricci[a,b], 
						1/2 cd[a]@rs[] + $TorsionSign * ( ricci[b,c] torsion[-b,a,-c] - 1/2 riemann[a,b,c,d] torsion[-b,-c,-d] )
					},
					MetricOn -> All,
					UseSymmetries -> True
				]
			];
			(* Curvature rules for the symmetrized Riemann tensor. *)
			If[metricQ && !torsionQ,
				cd /: CurvatureRelations[cd, SymRiemann] = Join[
					MakeRule[
						{
							sriemann[a,-a,b,c],
							$RicciSign * ricci[b,c]
						},
						MetricOn -> All,
						UseSymmetries -> True
					],
					MakeRule[
						{
							sriemann[a,b,-a,c],
							-1/2 * $RicciSign * ricci[b,c]
						},
						MetricOn -> All,
						UseSymmetries -> True
					]
				];
				If[curvrels,
					AutomaticRules[Evaluate[GiveSymbol[SymRiemann, cd]], CurvatureRelations[cd, SymRiemann], Verbose->False];
				];
			];
		]
	]
];

(* TODO: undefmetric hook *)

GiveOutputString[Schouten, covd_]   := StringJoin["S", "[", SymbolOfCovD[covd][[2]], "]"];
GiveOutputString[SymRiemann, covd_] := StringJoin["P", "[", SymbolOfCovD[covd][[2]], "]"];


ToRicci[expr_, cd_?CovDQ] := 
	Composition[
		TFRicciToRicci[#, cd]&,
		EinsteinToRicci[#, cd]&,
		EinsteinCCToRicci[#, cd]&,
		SchoutenToRicci[#, cd]&,
		SchoutenCCToRicci[#, cd]&
	][expr /. CurvatureRelations[cd]];
ToRicci[expr_] := Fold[ToRicci, expr, $CovDs];

SchoutenToRicci[expr_, cd_?CovDQ]  /; MetricOfCovD[cd] =!= Null := With[
	{
		ricci 		= GiveSymbol[Ricci, cd],
		rs 			= GiveSymbol[RicciScalar, cd],
		schouten 	= GiveSymbol[Schouten, cd],
		metric 		= MetricOfCovD@cd,
		d 			= DimOfManifold@ManifoldOfCovD@cd
	},
	expr /. schouten[inds__] :> ricci[inds]/(d - 2) - metric[inds] rs[] / (2 (d - 1) (d - 2))
];
SchoutenToRicci[expr_,_] := expr;
SchoutenToRicci[expr_] := Fold[SchoutenToRicci, expr, $CovDs];

RicciToSchouten[expr_, cd_?CovDQ] /; MetricOfCovD[cd] =!= Null := With[
	{
		ricci 		= GiveSymbol[Ricci, cd],
		rs 			= GiveSymbol[RicciScalar, cd],
		schouten	= GiveSymbol[Schouten, cd],
		metric 		= MetricOfCovD@cd,
		d 			= DimOfManifold@ManifoldOfCovD@cd
	},
	expr /. ricci[inds__] :> schouten[inds] (d - 2) + metric[inds] rs[] /(2 (d - 1))
];
RicciToSchouten[expr_,_] := expr;
RicciToSchouten[expr_] := Fold[RicciToSchouten, expr, $CovDs];

SchoutenCCToRicci[expr_, cd_?CovDQ] /; MetricOfCovD[cd] =!= Null := With[
	{
		ricci 		= GiveSymbol[Ricci, cd],
		rs 			= GiveSymbol[RicciScalar, cd],
		schouten	= GiveSymbol[SchoutenCC, cd],
		metric 		= MetricOfCovD@cd,
		d 			= DimOfManifold@ManifoldOfCovD@cd
	},
	expr /. schouten[LI[K_],inds__] :> ricci[inds]/(d - 2) - metric[inds] rs[] / (2 (d - 1) (d - 2)) - 1/2 K  metric[inds]
];
SchoutenCCToRicci[expr_,_] := expr;
SchoutenCCToRicci[expr_] := Fold[SchoutenCCToRicci, expr, $CovDs];

RicciToSchoutenCC[K_][expr_, cd_?CovDQ] /; MetricOfCovD[cd] =!= Null := With[
	{
		ricci 		= GiveSymbol[Ricci, cd],
		rs 			= GiveSymbol[RicciScalar, cd],
		schouten	= GiveSymbol[SchoutenCC, cd],
		metric 		= MetricOfCovD@cd,
		d 			= DimOfManifold@ManifoldOfCovD@cd
	},
	expr /. ricci[inds__] :> schouten[LI[K],inds] (d - 2) + metric[inds] rs[] /(2 (d - 1)) + 1/2 (d-2) K metric[inds]
];
RicciToSchoutenCC[K_][expr_,_] := expr;
RicciToSchoutenCC[K_][expr_] := Fold[RicciToSchoutenCC[K], expr, $CovDs];


EinsteinCCToRicci[expr_, cd_?CovDQ] /; MetricOfCovD[cd] =!= Null := With[
	{
		ricci 		= GiveSymbol[Ricci, cd],
		rs 			= GiveSymbol[RicciScalar, cd],
		einsteincc	= GiveSymbol[EinsteinCC, cd],
		metric 		= MetricOfCovD@cd,
		d 			= DimOfManifold@ManifoldOfCovD@cd
	},
	expr /. einsteincc[LI[K_], inds__] :> (ricci[inds] + 1/2 metric[inds] (-rs[] + (d - 2) (d - 1) K))
];
EinsteinCCToRicci[expr_,_] := expr;
EinsteinCCToRicci[expr_] := Fold[EinsteinCCToRicci, expr, $CovDs];


RicciToEinsteinCC[K_][expr_, cd_?CovDQ] /; MetricOfCovD[cd] =!= Null := With[
	{
		ricci 		= GiveSymbol[Ricci, cd],
		rs 			= GiveSymbol[RicciScalar, cd],
		einsteincc	= GiveSymbol[EinsteinCC, cd],
		metric 		= MetricOfCovD@cd,
		d 			= DimOfManifold@ManifoldOfCovD@cd
	},
	expr /. ricci[inds__] :> (einsteincc[LI[K], inds] - 1/2 metric[inds] (-rs[] + (d - 2) (d - 1) K))
];
RicciToEinsteinCC[K_][expr_,_] := expr;
RicciToEinsteinCC[K_][expr_] := Fold[RicciToEinsteinCC[K], expr, $CovDs];



ToRiemann[expr_, cd_?CovDQ] := 
	Composition[
		If[
			!CurvatureRelationsQ[cd],
			Composition[
				RicciToRiemann[#, cd]&,
				ToRicci[#, cd]&
			],
			Identity
		],
		KretschmannToRiemann[#, cd]&,
		WeylToRiemann[#, cd]&,
		SymRiemannToRiemann[#, cd]&,
		RiemannDownToRiemann[#, cd]&,
		GradChristoffelToRiemann[#, cd]&
	][expr];
ToRiemann[expr_] := Fold[ToRiemann, expr, $CovDs];


RiemannToSymRiemann[expr_, cd_?CovDQ] /; MetricOfCovD[cd] =!= Null && !TorsionQ[cd] := With[
	{
		riemann		= GiveSymbol[Riemann, cd],
		symriemann	= GiveSymbol[SymRiemann, cd]
	},
	expr /. riemann[a_,c_,b_,d_] :> 2/3 symriemann[a,b,c,d] - 2/3 symriemann[a,d,b,c]
];
RiemannToSymRiemann[expr_, _] := expr;
RiemannToSymRiemann[expr_] := Fold[RiemannToSymRiemann, expr, $CovDs];


SymRiemannToRiemann[expr_, cd_?CovDQ] /; MetricOfCovD[cd] =!= Null && !TorsionQ[cd] := With[
	{
		riemann		= GiveSymbol[Riemann, cd],
		symriemann	= GiveSymbol[SymRiemann, cd]
	},
	expr /. symriemann[a_,b_,c_,d_] :> 1/2 riemann[a,c,b,d] + 1/2 riemann[a,d,b,c]
];
SymRiemannToRiemann[expr_, _] := expr;
SymRiemannToRiemann[expr_] := Fold[SymRiemannToRiemann, expr, $CovDs];



RicciToRiemann[expr_, PD] := 
	expr;
RicciToRiemann[expr_, cd_?CovDQ] /; CurvatureRelationsQ[cd] :=
	expr; 
RicciToRiemann[expr_, cd_?CovDQ] := 
	With[{indices = GetIndicesOfVBundle[First@VBundlesOfCovD@cd, 2]},
		With[{a = First@indices, b = Last@indices},
			expr /.	{
				Ricci[cd][c_, d_] :> Module[{e = DummyIn@First@VBundlesOfCovD@cd}, $RicciSign Riemann[cd][c, -e, d, e]],
				RicciScalar[cd][] :> $RicciSign Scalar[ Riemann[cd][-a,-b,a,b] ]
			}
		]
	];
RicciToRiemann[expr_] := Fold[RicciToRiemann, expr, $CovDs];


KretschmannToRiemann[expr_, cd_?CovDQ] /; MetricOfCovD[cd] =!= Null :=
	With[{indices = GetIndicesOfVBundle[First@VBundlesOfCovD@cd, 4]},
		With[{
			a = indices[[1]], 
			b = indices[[2]], 
			c = indices[[3]], 
			d = indices[[4]]
		},
			(* Definition of Kretschmann corresponds to that in xCoba (section 7.2.6 and 7.3.7). *)
			expr /. Kretschmann[cd][] :> Scalar[ Riemann[cd][-a,-b,c,d] Riemann[cd][-c,-d,a,b] ]
		]
	];
KretschmannToRiemann[expr_, _] := expr;
KretschmannToRiemann[expr_] := Fold[KretschmannToRiemann, expr, $CovDs];



(*************************)
(* Commuting derivatives *)
(*************************)

(* The next bit is thanks to Leo Stein, see 
   http://groups.google.com/group/xact/browse_thread/thread/31e959cbee8d1848/690def9618ff519c *)
SortCovDsToDiv[tensor_][x_] := Fold[SortCovDsToDiv[tensor, #2][#1]&, x, $CovDs];
SortCovDsToDiv[tensor_, CD_?CovDQ][x_] := x //. {	
	bigexpr : CD[b_]@CD[a_][expr_] 
		/; !FreeQ[expr, tensor[___, ChangeIndex[b], ___]] && FreeQ[expr, tensor[___, ChangeIndex[a], ___]] 
		:> CommuteCovDs[bigexpr, CD, {a, b}]
}; 

SortCovDsToBox[tensor_][x_] := Fold[SortCovDsToBox[tensor, #2][#1]&, x, $CovDs];
SortCovDsToBox[tensor_, CD_?CovDQ][x_] := x //. {
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


(* TODO: symmetrized derivatives. *)
DivFreeQ[expr_, tens_] := And @@ (DivFreeQ[expr, tens, #] & /@ $CovDs);
DivFreeQ[expr_, tens_, CD_?CovDQ] := 
	FreeQ[expr, CD[a_][inner_] /; ! FreeQ[inner, tens[___, ChangeIndex[a], ___]]];



(*******************)
(* Killing vectors *)
(*******************)


KillingVectorQ[_,_] := False
KillingVectorQ[v_] 	:= KillingVectorQ[v,MetricOfKillingVector@v];

MetricOfKillingVector[_] := None

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
	{
		vb 		= VBundleOfMetric[metric],
		cd 		= CovDOfMetric[metric],
		riemann = GiveSymbol[Riemann, CovDOfMetric@metric],
		l1patt 	= PatternSequence[L1]/.LI[___]->LI[___],
		l2patt 	= PatternSequence[L2]/.LI[___]->LI[___]
	},

	If[$DefInfoQ,
		Print[
			"** Defining ", PrintAs@xi, 
			" to be a Killing vector of the metric ", PrintAs@metric, "."
		];
	];

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
 
	MetricOfKillingVector[xi] ^= metric;
	KillingVectorQ[xi,metric] ^= True;
] /; AIndexQ[ind, VBundleOfMetric@metric];



GradChristoffelToRiemann[expr_] :=
	Fold[GradChristoffelToRiemann[#1, #2] &, expr, $CovDs];

GradChristoffelToRiemann[expr_, cd_?CovDQ] :=
	expr /. GradChristoffelToRiemannRules[cd];

GradChristoffelToRiemannRules[_] =
  {};

GradChristoffelToRiemannRules[cd_?CurvatureQ] :=
	With[{
		riemann 	= GiveSymbol[Riemann, cd],
		christoffel = GiveSymbol[Christoffel, cd],
		vb 			= TangentBundleOfManifold@ManifoldOfCovD@cd,
		indices 	= GetIndicesOfVBundle[
			TangentBundleOfManifold@ManifoldOfCovD@cd, 
			5
		]
    },
		With[{
			a = indices[[1]],
			b = indices[[2]],
			c = indices[[3]],
			d = indices[[4]],
			e = indices[[5]],
			apat = PatternIndex[indices[[1]], AIndex, Up, vb],
			bpat = PatternIndex[indices[[2]], AIndex, Up, vb],
			cpat = PatternIndex[indices[[3]], AIndex, Up, vb],
			dpat = PatternIndex[indices[[4]], AIndex, Up, vb],
			module = Module
		},
			RuleDelayed[
				HoldPattern[PD[-bpat]@christoffel[dpat, -apat, -cpat]] /; OrderedQ[{a, b}],
				module[{e}, 
					+ (1 / $RiemannSign) * riemann[-a, -b, -c, d] 
					+ PD[-a]@christoffel[d, -b, -c] 
					- christoffel[d, -b, -e] christoffel[e, -a, -c] 
					+ christoffel[d, -a, -e] christoffel[e, -b, -c]
				]
			]
		]
	];



(*****************************************)
(*                                       *)
(*   Symmetrized covariant derivatives   *)
(*                                       *)
(*****************************************)


(********************)
(* Helper functions *)
(********************)


DummiesToFrees::usage = 
	"DummiesToFrees[expr] replaces all down dummy indices in expr with \
new indices in the same vector bundle.";
 
DummiesToFrees[expr_] :=
	ReplaceIndex[
		expr, 
		Inner[Rule, #, DummyAs /@ #, List]& @ IndicesOf[Dummy, Down][expr]
	];

(* 
 *  UnorderedPartitionedPermutations.
 *  I'm keeping this private for the moment as I don't see any obvious 
 *  additional applications.
 *)


UnorderedPartitionedPermutations::usage =
	"UnorderedPartitionedPermutations[list, partition] gives all permutations of \
list whose sub-parts according the partition are unordered. For example, \
UnorderedPartitionedPermutations[list,Table[1,{Length@list}]] gives all ordinary \
permutations of list, and UnorderedPartitionedPermutations[list,{Length@list}] \
returns the list itself.";

(* The general case: do a recursion into the partition. The workhorse is Subsets. *)
UnorderedPartitionedPermutations[set_List,partition:{i1_Integer,rest__Integer}] /; Total@partition === Length@set :=
	Flatten[
		Function[
			subset,
			Map[
				Join[{subset}, #]&,
				UnorderedPartitionedPermutations[
					Complement[set,subset],
					{rest}
				]
			]
		] /@ Subsets[set,{i1}]
		,
		1
	];

(* The limiting case where the partition is just one number. *)
UnorderedPartitionedPermutations[set_List,partition:{i1_Integer}] /; Total@partition === Length@set :=
	{{set}};

(* The limiting case where both the partition and the set are empty. *)
UnorderedPartitionedPermutations[{},{}]:=
	{};


(* The first argument to PartitionedSymmetrize has to be held (we don't need
   HoldFirst for this, as the first argument is already a pure function)
   because replacement of contractions of tensors can remove some indices 
   we want to symmetrize over.
   One example of this is
   
   		PartitionedSymmetrized[ CD[a]@RiemannCD[-a,-b,b,c], {a,b}, {1,1} ]
   		
   which should symmetrize the indices a and b. The correct result is zero, but
   if we replace the contraction of the Riemann tensor with the Ricci tensor,
   the result becomes non-sensical:
   
   		CD[a]@RicciCD[-a,c] -> 1/2 ( CD[a]@RicciCD[-a,c] + CD[b]@RicciCD[-a,c] )
   
   which has inconsistent indices and is clearly wrong.
   This means we cannot evaluate all expressions we want to symmetrize beforehand.
   This is a pity, since some of the things we want to symmetrize are recursive
   functions. 
   The best we can do is check for each case if all indices
   are still present in the first summand of the symmetrization, and if so,
   use it to generate tu full symmetrization with re-evaluating it again.
   *)
PartitionedSymmetrize[f_Function, indices : (List | IndexList)[], partition : {}, evaluateonce_:False] = 
	0;

PartitionedSymmetrize[f_Function, indices : (List | IndexList)[__], partition : {parts__Integer}, evaluateonce_:False] /; Total@partition === Length@indices := 
	1 / Multinomial[parts] With[
		{
			permutations = UnorderedPartitionedPermutations[indices, partition]
		},
		With[
			{
				ffirst = f @@ First @ permutations,
				pfirst = Flatten @ First @ permutations
			},
			(* See if all indices are still present. *)
			If[
				(* The indices to be symmetrized should be a subset of 
				   the list of free indices of the expression. *)				  
				(TrueQ @ evaluateonce) && (Complement[ IndexList @@ indices, IndicesOf[Free][ffirst] ] === IndexList[]),
				(* If so, use ReplaceIndex to replace stuff. *)
				ffirst + Total[
					ReplaceIndex[
						ffirst,
						Thread[pfirst -> Flatten@#]
					]& /@ Rest[permutations]
				],
				(* If not, evaluate the function f each time. *)
				ffirst + Total[ f @@ # & /@ Rest[permutations] ]
			]
		]
	];


(* 
 *  CovDCommutator is a helper function that defines the commutator. 
 *  The code is copy-pasted from a private function in xTensor that commutes covds 
 *  (xAct`xTensor`Private`makeCommuteCovDs).
 *) 

CovDCommutator[expr_,{a_,b_},covd_?CovDQ] :=
	Plus[
		xAct`xTensor`Private`addCurvature[expr,covd,{b,a},Select[FindFreeIndices[expr],AIndexQ],VBundlesOfCovD[covd]],
		xAct`xTensor`Private`addTorsion[expr,covd,{b,a}],
		xAct`xTensor`Private`addOther[expr,covd,{b,a}]
	];


(***********************)
(* Generic definitions *)
(***********************)

$AutoSymmetrizeCovDs = False;

SymCovDQ[_] := False;

Unprotect[delta];
delta /: HoldPattern[(_?SymCovDQ)[__][delta[-_Symbol, _Symbol]]] := 0;
delta /: HoldPattern[(_?SymCovDQ)[__][delta[_Symbol, -_Symbol]]] := 0;
Protect[delta];


(***************)
(* Formatting. *)
(***************)

(* Helper function. *)
AddSymBraces[{"",""}] :=
	{"",""};
AddSymBraces[{downstring_, upstring_}] :=
	With[
		{
			openbraceupQ 	= StringTake[downstring,1]	===" ",
			closebraceupQ 	= StringTake[downstring,-1]	===" "
		},
		{
			If[openbraceupQ, " ", "("] <> downstring <> If[closebraceupQ, " ", ")"],
			If[openbraceupQ, "(", " "] <> upstring   <> If[closebraceupQ, ")", " "]
		}
	];

(* 
 *  Prefix notation.
 *  This is differs from the xTensor code by just one insertion of AddSymBraces. 
 *)
xAct`xTensor`Private`MakeBoxesCovD[der_?SymCovDQ[inds__][MBexpr_], "Prefix"] /; Length@{inds} > 1 := 
	Block[
		{$WarningFrom="CovD Formatting"}
	,
		xAct`xTensor`Private`FlattenRowBox@RowBox[
			{
				Apply[
					SubsuperscriptBox[Last@SymbolOfCovD@der, #1, #2]&,
					AddSymBraces@xAct`xTensor`Private`SSSBinds@{inds}
				],
				MBexpr
			}
		]
	];

(* TODO: postfix notation. *)


(**************)
(* Expanding. *)
(**************)

(* No second argument: fold over $CovDs. *)
ExpandSymCovDs[expr_] :=
	Fold[ExpandSymCovDs[#1,#2]&, expr, $CovDs];

(* Second argument is a symcovd: do the expansion. *) 
(* Only expand if $AutoSymmetrizeCovDs is False in order to prevent infinite recursion. *)
ExpandSymCovDs[expr_, cd_?SymCovDQ] /; Not @ TrueQ @ $AutoSymmetrizeCovDs :=
	expr //. HoldPattern[cd[inds__][x_]] /; Length@{inds} > 1 :> Symmetrize[
		Fold[cd[#2][#1]&, x, {inds}],
		{inds}
	];

(* Second argument is generic or $AutoSymmetrizeCovDs is True: return the same expression. *)
ExpandSymCovDs[expr_, _] :=
	expr;


(****************)
(* Symmetrizing *)
(****************)

Options[SymmetrizeCovDs] ^=
	{
		"UseCache" -> True
	};

(* No second argument: fold over $CovDs. *)
SymmetrizeCovDs[expr_, options___?OptionQ] :=
	Fold[SymmetrizeCovDs[#1, #2, options]&, expr, $CovDs];

(* Second argument is a symcovd: symmetrize. *) 
(* TODO: simple algorithm for no curvature case. *)
SymmetrizeCovDs[expr_, cd_?SymCovDQ, options___?OptionQ] :=
	If[
		TrueQ [ "UseCache" /. CheckOptions[options] /. Options[SymmetrizeCovDs] ]
		,
		expr //. p:HoldPattern[cd[__][cd[__][_]]] :> SymmetrizeCovDsReplace[p, cd, options]
		,
		expr //. p:HoldPattern[cd[__][cd[__][_]]] :> SymmetrizeCovDs2[p, options]
	];

(* Second argument is generic: return the same expression. *)
SymmetrizeCovDs[expr_, _, options___?OptionQ] :=
	expr;

SetAttributes[SymmetrizeCovDs, HoldFirst];

(* Because symmetric imploding is a time consuming process (mainly because it is
   recurvise), we use caching to speed things up. This is done by storing 
   symmetric implosions as replacement rules in the list $SymImplodedRules.*)
ClearSymCovDCache[] :=
	(
		Unprotect @ $SymCovDCache;
		$SymCovDCache = {};
		Protect @ $SymCovDCache;
	);
ClearSymCovDCache[]


(* If there are already rules for expr, return the replaced. 
   It is important to only replace if the full expression matches any of the 
   stored rules. Because of the recursive nature of symmetric imploding, 
   we don't want to replace partial matches. 
   Note that ReplaceAll first tries to replace the whole expression and then 
   moves to subexpressions, so we don't need to take precautions to prevent 
   replacing subexpressions if one of the rules already matches the whole 
   expression. *)
SymmetrizeCovDsReplace[expr_, cd_, options___] /; MatchQ[expr, Alternatives@@First /@ $SymCovDCache] := 
	expr /. $SymCovDCache;

(* If there are no previously computed rules, compute the imploded expression. *)
SymmetrizeCovDsReplace[expr_, cd_, options___] := 
	With[
		{
			(* Make a rule for symmetrizing the input expression without dummies. *)
			(* The dummies are stripped to prevent doing this for every dummy
			   index configuration. *)
			rule = MakeRule[
				Evaluate @ {
					#,
					ToCanonical @ ContractMetric @ SymmetrizeCovDs1[ #, cd, options]
				},
				MetricOn -> All, UseSymmetries -> False
			]& @ DummiesToFrees[ expr ]
		}
		,
		If[ 
			(* Match the new rule(s) the input expression? *) 
			! MatchQ[expr, Alternatives@@First /@ rule]
			,
			(* If not, throw an error. *)
			Message[SymmetrizeCovDs::error, "Cached rule does not match input expression."];
			Throw[];
			,
			(* If they do, save them and return. *)
			(* Save the rules. *)
			Unprotect @ $SymCovDCache;
			$SymCovDCache = Union[
				$SymCovDCache,
				rule
			];
			(* Return the replace expression. *)
			expr /. rule
		]
	];


SymmetrizeCovDs1[expr_, cd_, options___] :=
	(* The recursion to SymmetrizeCovDs is there to ensure that any
	   unsymmetrized derivatives get symmetrized before caching. *)
	SymmetrizeCovDs[ SymmetrizeCovDs2[expr, options], cd, options];


(* Worker function *)

(* Eating one derivative from the left. *)
SymmetrizeCovDs2[HoldPattern[cd_[b_][cd_[inds__][expr_]]], options___] :=
	cd[inds,b][expr] - CommutatorOperator[cd,(-1+#1/(#2+1))&][inds,b][expr];

(* Eating one derivative from the right. *)
SymmetrizeCovDs2[HoldPattern[cd_[inds__][cd_[b_][expr_]]], options___] :=
	cd[inds,b][expr] - CommutatorOperator[cd,(#1/(#2+1))&][inds,b][expr];

(* General situation: do a recursion. 
   We break up one of the two derivatives into a single derivative plus the rest,
   after which the single derivative gets eaten by the other derivative.
   We have to be careful to always let the bigger derivative eat the smaller
   derivative; doing it the other way around is inefficient. *)
(* Case 1: the outer derivative has less (or an equal amout of) indices 
   than the inner derivative. The inner derivative eats the outer derivative 
   step-by-step. *)
SymmetrizeCovDs2[HoldPattern[cd_[outer__][cd_[inner__][x_]]], options___] :=
	PartitionedSymmetrize[
		(* We need the call to SymmetrizeCovDs inside the leftover of the
		   outer derivative, otherwise it will eat the single derivative 
		   we've just split off in the next recursion. *)
		(cd@@#1) @ SymmetrizeCovDs[ (cd@@#2) @ cd[inner] @ x, cd, options] &,
		{outer},
		{Length@{outer}-1,1},
		True
	] /; Length@{outer} > 1 && Length@{inner} > 1 && Length@{outer} <= Length@{inner};
(* Case 2: the outer derivative has more indices than the inner one. 
   The outer derivative eats the inner derivative step-by-step. *)	
SymmetrizeCovDs2[HoldPattern[cd_[outer__][cd_[inner__][x_]]], options___] :=
	PartitionedSymmetrize[
		(* The pattern matcher will match the outer derivative eating the 
		   single derivative split off from the innermost derivative,
		   so the recursive call to SymmetrizeCovDs can take everything
		   as its argument. *)
		SymmetrizeCovDs[ cd[outer] @ (cd@@#1) @ (cd@@#2) @ x, cd, options] &,
		{inner},
		{1, Length@{inner}-1},
		True
	] /; Length@{outer} > 1 && Length@{inner} > 1 && Length@{outer} > Length@{inner};

(* Commutator operator function *)

(* The special case: no torsion, a metric derivative, and only the tangent bundle. *)
(* This code might be a bit unreadable, but its action is documented in the 
   tutorial on symmetrized derivatives. *)
CommutatorOperator[cd_, coefffunction_:(#1/(#2+1)&)][inds___, a_, b_][expr_] :=
	Module[
		{
			newcoeffunction,
			n = Length@{inds} + 1,
			i,j
		},
		(* There are three contributions ... *)
		Plus[
			(* The contribution from when the commutator acts on the indices
			   of the input expression. *)
			Sum[
				Sum[
					coefffunction[j,n] Binomial[j-1,i],
					{j,i+1,n}
				] *
				With[
					{
						inew = i
					},
					PartitionedSymmetrize[
						CommutatorOperatorHelper[cd, addCurvatureList[expr, cd, {b,Sequence@@#3}, All], Join[#1,#2], inew]&,
						{inds,a},
						{i,n-i-1,1},
						True
					]
				]
				,
				{i,0,n-1}
			]
			,
			(* The contribution from when the commutator acts on an index of 
			   a symmetrized derivative. *)
			Sum[
				Sum[
					coefffunction[j, n] (n - j) Binomial[j-1, i],
					{j,i+1,n-1}
				] *
				With[
					{
						inew = i
					},
					PartitionedSymmetrize[
						CommutatorOperatorHelper[
							cd, 
							addCurvatureList[(cd@@Join[#2,#3]) @ expr, cd, {b,Sequence@@#4}, #3], 
							#1, 
							inew
						]&,
						{inds,a},
						{i,n-i-2,1,1},
						True
					]
				]
				,
				{i,0,n-2}
			]
			,
			(* And finally the recursive part: the contribution from when the 
			   commutator acts on an index of a symmetrized derivative, 
			   compensated for making the derivative completely symmetric. *)
			-1 * Sum[
				(* First construct the new coefficient function. *)
				newcoeffunction = With[ (* "With" is needed because Function (&) is HoldAll. *)
					{
						inew = i,
						nnew = n
					},
					Module[{k}, (* Question: do we need Module to make k unique in each summand, or is it overkill? *)
						Sum[
							coefffunction[k,nnew] * (nnew - k) * Binomial[k-1, inew] *
							(
								( #1 / (nnew - inew - 1) ) - UnitStep[#1 - k + inew] * ( #1 + inew - k + 1) / (nnew - k)
							)
							,
							{k,inew+1,nnew -1}
						]&
					]
				];
				With[
					{
						newcoeffunctionnew = newcoeffunction
					},
					PartitionedSymmetrize[
						Function[
							curvlist,
							(cd@@#1)[First @ curvlist] * 
							CommutatorOperator[
								cd, newcoeffunctionnew
							][
								Sequence@@#2, Last @ curvlist
							][expr]
						][ First @ addCurvatureList[(cd@@Join[#2,#3]) @ expr, cd, {b,Sequence@@#4}, #3] ] &,
						{inds,a},
						{i,n-i-2,1,1},
						True
					]
				]
				,
				{i,0,n-3}
			]		
		]
	] /; !TorsionQ[cd] && MetricOfCovD[cd] =!= Null && VBundlesOfCovD[cd] === {VBundleOfMetric[MetricOfCovD[cd]]};


(* Generic case. Much simpler, but much slower. *)
CommutatorOperator[cd_, coefffunction_:(#1/(#2+1)&)][inds___, b_][expr_] :=
	Module[
		{
			n = Length@{inds},
			i
		},
		Sum[
			coefffunction[i,n] * PartitionedSymmetrize[
				(cd@@#1) @ CovDCommutator[(cd@@#3) @ expr, {b,Sequence@@#2}, cd] &,
				{inds},
				{i-1,1,n-i},
				True
			]
			,
			{i,1,n}
		]
	];
	
(* Helper function *)
CommutatorOperatorHelper[cd_, curvaturelist_List, indices_List, i_Integer] :=
	Total @ Apply[
		Times[ (cd@@(indices[[1;;i]]))[#1] , (cd@@(indices[[i+1;;-1]]))[#2] ]&,
		curvaturelist,
		{1}
	];
	
(* addCurvatureList comes more or less straight from xTensor.nb,
   except for the interchange of indices {a,b}->{b,a} and the list structure. *)
addCurvatureList[expr_,covd_,{a_,b_}, onIndices_:All] := 
	With[
		{
			dummy	= DummyIn @ First @ VBundlesOfCovD @ covd,
			vbQ		= xAct`xTensor`Private`VBundleIndexQ[First @ VBundlesOfCovD @ covd],
			curv	= Riemann[covd],
			frees	= If[onIndices === All,
						Select[FindFreeIndices[expr],AIndexQ],
						onIndices
					  ]			
		},
		List@@Join[
			Map[
				{$RiemannSign curv[b,a,-dummy,#],ReplaceIndex[expr,#->dummy], dummy}&,
				Select[frees,vbQ]
			],
			Map[
				{-$RiemannSign curv[b,a,-#,dummy],ReplaceIndex[expr,-#->-dummy], -dummy}&,
				Select[ChangeIndex/@frees,vbQ]
			]
		]
	];


(***************)
(* ObjectOrder *)
(***************)

(* xSort calls ObjectOrder to determine the ordering of objects,
 * which in turn calls SortingName to determine the name for sorting objects.
 * Sorting name has a rule for single derivatives, but we need to 
 * add a rule for symmetrized derivatives.
 * If we don't, xSort doesn't see that products of the same tensor with the
 * same number of symmetrized derivatives commute, and hence these products
 * will not be fully canonicalized upon calling ToCanonical.
 *)

(* We simply overwrite the function SortingName. *)
xAct`xTensor`Private`SortingName[xAct`xTensor`Private`name_List] := 
	xAct`xTensor`Private`name /. 
	{
		(* This is the existing rule in xTensor.nb *)
		xAct`xTensor`Private`MyDer[xAct`xTensor`Private`covd_?CovDQ[_]] :> 
			xAct`xTensor`Private`MyDer[xAct`xTensor`Private`covd],
		(* This is the new rule for symmetrized derivatives. *)
		xAct`xTensor`Private`MyDer[xAct`xTensor`Private`covd_?SymCovDQ[x__]] :>
			xAct`xTensor`Private`MyDer[xAct`xTensor`Private`covd, Length@{x}]
	};


(**************)
(* ChangeCovD *)
(**************)

(* 
 *  We simply expand all symmetrized covds before changing. 
 *  Perhaps not efficient when covd2 is also symmetrizable, but it is simple.
 *  Note: this overwrites xTensor code, instead of extending it.
 *  TODO: write more efficient code for this.
 *)

Unprotect[ChangeCovD];

ChangeCovD[expr_, covd_?CovDQ, covd2_:PD] :=
	Block[
		{$AutoSymmetrizeCovDs = False},
		If[
			xAct`xTensor`Private`CompatibleCovDsQ[covd,covd2]
			,
			xAct`xTensor`Private`changeCovD[
				ExpandSymCovDs[expr,covd],
				covd,covd2,xAct`xTensor`Private`Identity1,Identity
			]
			,
			expr
		]
	];

Protect[ChangeCovD];


(******************)
(* ContractMetric *)
(******************)

(* 
 *  This is a simple modification of xTensor's ContractMetric1. 
 *  It currently ignores OverDerivatives and only contracts when the 
 *  derivative is metric compatible.
 *  TODO: support OverDerivatives.
 *)

(CM:xAct`xTensor`Private`ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. der_?SymCovDQ[inds__][expr_]met:metric_[b_,c_]]:=
	Module[{dm=der[inds][met],result},
		If[
			(dm===0)&&xAct`xTensor`Private`differentexpressionsQ[result=CM[expr met],{expr,met}]
			,
			CM[rest der[inds][result]]
			,
			CM[rest met]der[inds][expr]
		]
	] /; (MemberQ[FindFreeIndices[expr],ChangeIndex[c]|ChangeIndex[b]]&&Head[expr]=!=metric);



(********)
(* VarD *)
(********)

(* Same connection. *)
VarD[tensor_,covd_][covd_?SymCovDQ[inds__][expr_],rest_] := 
	(-1)^Length[{inds}]VarD[tensor,covd][expr,covd[inds][rest]]

(* Different connection. *)
VarD[tensor_, covd1_?CovDQ][expr : covd2_?SymCovDQ[__][_], rest_] := 
	VarD[tensor, covd1][ChangeCovD[expr, covd2, covd1], rest] /; xAct`xTensor`Private`CompatibleCovDsQ[covd1, covd2];


(*****************)
(* Perturbations *)
(*****************)

(* This is literally copy-pasted from xPert.nb section 3.5, with the
   only modification that the derivatives now have a SymCovD pattern to match. *)
xAct`xPert`Private`ExpandPerturbation1[Perturbation[expr_,n_.],options___] := 
	Module[
		{sepmetric,od,tmp}
		,
		{sepmetric,od} = {SeparateMetric, OverDerivatives} /. CheckOptions[options] /. Options[ExpandPerturbation];
		
		(* Separate metrics *)
		tmp = Perturbation[If[sepmetric,SeparateMetric[][expr],expr], n];
		(* Derivatives *)
		If[
			od,
			tmp = tmp /. expr1:HoldPattern[Perturbation[_Symbol?CovDQ[_][_]|_Symbol?SymCovDQ[__][_]|LieD[_][_]|Bracket[_,_][_],_.]] :> xAct`xPert`Private`ExpandPerturbationDer[expr1]
		];
		(* Reexpand *)
		If[
			tmp =!= Perturbation[expr,n],
			tmp = ExpandPerturbation[tmp,options]
		];
		
		(* Return result *)
		tmp
	];

(* Expansion of symmetrized derivatives: peel off one derivative on the outside,
   and pass it back to ExpandPerturbation so that the existing xPert code can expand
   the single derivative. *)
xAct`xPert`Private`ExpandPerturbationDer[Perturbation[cd_Symbol?SymCovDQ[-a_,rest__][expr_],n_.]]:=
	Block[
		{$AutoSymmetrizeCovDs=False},
		PartitionedSymmetrize[
			ExpandPerturbation@Perturbation[(cd@@#1)@(cd@@#2)@expr,n]&,
			{-a,rest},
			{1,Length@{rest}},
			False
		]
	];


(****************)
(*  Validation  *)
(****************)

(* The validation code comes more or less straight from section 11.3 of xTensor.nb,
   with a few modifications. *)

Unprotect[xAct`xTensor`Private`UncatchedValidate];

xAct`xTensor`Private`UncatchedValidate[covd : _?SymCovDQ[__][_]] := 
	ValidateSymCovD[covd];
	
Protect[xAct`xTensor`Private`UncatchedValidate];

ValidateSymCovD[x:der_[inds__][expr_]] := 
	Catch[
		(* Check derivative name *)
		If[!CovDQ[der],
			Throw[Message[Validate::unknown,"covariant derivative",der];ERROR[x]]
		];
		(* Check index. Patterns are not accepted *)
		Map[
			If[!GIndexQ[#],
				Throw[Message[Validate::unknown,"g-index",#];ERROR[x]]
			]&,
			{inds}
		];
		(* Check expression *)
		xAct`xTensor`Private`UncatchedValidate[Unevaluated[expr]];
		(* Check that derivative and index are compatible *)
		Map[
			If[!xAct`xTensor`Private`IndexOnQ[#,xAct`xTensor`Private`TangentBundleOfCovD[der[#]]],
				Throw[Message[Validate::invalid,#,"index for derivative"<>ToString[der]];ERROR[x]]
			]&,
			{inds}
		];
		(* Check that there is a metric if index is an up-index *)
		Map[
			If[UpIndexQ[#]&&!MetricEndowedQ[VBundleOfIndex[#]],
				Throw[Message[Validate::invalid,#,"index for a derivative because ther is no metric in its vector bundle"];ERROR[x]]
			]&,
			{inds}
		];
		(* Return the expression. *)
		x
	];

(****************)
(* DefCovD hook *)
(****************)

(* Insert the SymCovD option into the option list of DefCovD. *)
Unprotect[xAct`xTensor`DefCovD];
If[FreeQ[Options[xAct`xTensor`DefCovD], SymCovDQ], 
	Options[xAct`xTensor`DefCovD] ^= Append[Options[xAct`xTensor`DefCovD], SymCovDQ -> False];
, 
	Null;
];
Protect[xAct`xTensor`DefCovD];

xTension["xTras`xTensor2`", DefCovD, "End"] := xTrasDefSymCovD;

xTrasDefSymCovD[covd_[ind_], vbundles_, options___?OptionQ] := With[
	{
		cd			= covd,
		tbundle 	= VBundleOfIndex[ind], 
		metric		= FromMetric	/. CheckOptions[options] /. Options[DefCovD],
		metricQ		= (FromMetric	/. CheckOptions[options] /. Options[DefCovD]) =!= Null,
		ot			= OrthogonalTo 	/. CheckOptions[options] /. Options[DefCovD],
		symq		= SymCovDQ		/. CheckOptions[options] /. Options[DefCovD]
	},
	If[symq,
		With[
			{
				leibnitzQ	= ot === {},
				frozenQ 	= If[metricQ, xAct`xTensor`Private`FrozenMetricQ[metric], False],
				tbQ			= SymbolJoin[tbundle,"`Q"],
				tbpmQ		= SymbolJoin[tbundle,"`pmQ"]
			},
			With[
				{
					invmetric = If[metricQ, If[frozenQ, GiveSymbol[Inv, metric], metric], False]
				},
			
				(* Inform the user that cd will be symmetrizable. *)
				xAct`xTensor`Private`MakeDefInfo[DefCovD, covd[ind], {"covariant derivative","to be symmetrizable"}];
				
				(* Set the correct SGS. *)
				cd /: SymmetryGroupOfCovD[HoldPattern[cd[inds__]]] := 
					Symmetric[ Range@Length@{inds} ];
					
				(* Register as a symmetrized derivative. *)
				cd /: SymCovDQ[cd] =
					True;
					
				(* Collapse zero indices to the identity. *)
				cd[] = 
					Identity;
					
				(* Map over lists. *)
				HoldPattern[cd[inds__][x_List]] :=
  					cd[inds] /@ x;
  					
  				(* Map over sums. *)
				HoldPattern[cd[inds__][x_Plus]]:=
					cd[inds] /@ x;
					
				(* Action on constants. *)
				HoldPattern[cd[__][_?ConstantQ]] :=
  					0;

				(* On Scalars. *)
				HoldPattern[cd[inds__][Scalar[x_]]] :=
					cd[inds][ReplaceDummies[x]];
					
				(* Leibnitz behaviour. *)
				If[leibnitzQ,
					(* Simple expression for when one of the factors is constant. *)
					HoldPattern[cd[inds__][x_?ConstantQ * y_]] :=
						x cd[inds][y];
					(* Generic expression. *)
					HoldPattern[cd[inds__][x_ * y_]] :=
						Module[
							{
								i, l = Length@{inds}
							},
							Sum[
								Binomial[l,i] PartitionedSymmetrize[
									(cd@@#1)[x] * (cd@@#2)[y] &,
									{inds},
									{i,l-i},
									False
								],
								{i,0,l} 
							]
						];
					(* Scalar functions.
					   The code below is a bit unreadable, but the formula can
					   be found in the tutorial on symmetrized derivatives. *)
					HoldPattern[cd[inds__][f_?ScalarFunctionQ[args___]]] :=
						Block[
							{
								slot, apply
							},
							Module[
								{
									i,j,k
								},
								Sum[
									Sum[
										With[
											{
												product = Product[apply[cd, slot[k]][{args}[[j[k]]]], {k,1,i}]
											},
											Apply[
												Derivative,
												Count[Table[j[k],{k,1,i}], #]& /@ Range@Length@{args} 
											][f][args] *
											Total @ Map[
												Function[
													partition,
													Multinomial@@partition / Times @@ Factorial[Last /@ Tally[partition]] *
													PartitionedSymmetrize[
														product& /. {slot -> Slot, apply -> Apply}
														,
														{inds},
														partition,
														False
													]
												],
												IntegerPartitions[Length@{inds},{i}]
											]
										]
										,
										##
									]& @@ Table[{j[k],1,Length@{args}}, {k, 1, i}]
									,
									{i,1,Length@{inds}}
								]
							]
						];
					,
					(* Not Leibnitz. *)
					(* On scalar functions. *)
					HoldPattern[cd[inds__,a_][f_?ScalarFunctionQ[args___]]] :=
						Block[{$AutoSymmetrizeCovDs = False},
							PartitionedSymmetrize[
								(cd@@#1) @ (cd@@#2) @ f[args],
								{inds,a},
								{Length@{inds},1},
								False
							]
						];							
				];
				
				(* Metric compatibility. *)
				If[
					metricQ
					,
					If[
						frozenQ
						,
						HoldPattern[cd[__][invmetric[_Symbol?tbQ,_Symbol?tbQ]]]  := 0;
						HoldPattern[cd[__][metric[-_Symbol?tbQ,-_Symbol?tbQ]]]   := 0;
						,
						HoldPattern[cd[__][metric[_?tbpmQ,_?tbpmQ]]] := 0;	
					]
				];
				
				(* Automatic symmetrization. *)
				HoldPattern[cd[x__][cd[y__][z_]]] /; TrueQ @ $AutoSymmetrizeCovDs :=
					Block[
						{$AutoSymmetrizeCovDs = False},
						SymmetrizeCovDs[cd[x]@cd[y]@z]
					];
			]
		]
	]
];




End[]