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

(* Symmetric imploding and exploding *)

SymExplode::usage =
	"SymExplode[expr] explodes any symmetrically imploded tensors in expr.";

SymImplode::usage =
	"SymImplode[expr] symmetircally implodes any derivative in expr."; 

GiveSymImplodedName::usage =
	"GiveSymImplodedName[tensor, cd] returns the name of the symmetrically \ 
imploded tensor cdtensor."; 

DefSymImplodedTensor::usage =
	"DefSymImplodedTensor[tensor,cd,n] defines the n-fold symmetrically imploded tensor.";
	
SymImplodedQ::usage =
	"SymImplodedQ[tensor] gives True if tensor is a symmetrically imploded tensor, \
and False otherwise."


(* Create RemoveConstants in the non-private context, because we need it here. *)
RemoveConstants::usage = "bla";
CollectTensors::usage = "bla";
CollectMethod::usage = "bla";
SimplifyMethod::usage = "bla"; 
RemoveTensorWrapper::usage = "bla"; 
TensorWrapper::usage = "bla";



Begin["`Private`"]

ConstantExprQ[(Plus | Times | _?ScalarFunctionQ)[args__]] := And @@ Map[ConstantExprQ, List@args];
ConstantExprQ[x_] := ConstantQ[x];


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
	expr /. HoldPattern[IndexFree[sub_]] :> FromIndexFree1[sub];

FromIndexFree1[expr_] := ScreenDollarIndices[ FromIndexFreeTensors@FromIndexFreeCovDs@expr ];
	
FromIndexFreeTensors[expr_] := 
	expr /. RuleDelayed[
		(x_?xTensorQ)^Optional[n_Integer],
		Product[x @@ DummyIn /@ SlotsOfTensor@x, {n}] 
	];

FromIndexFreeCovDs[expr_] :=
	expr /.RuleDelayed[
				(cd_?CovDQ[x_])^Optional[n_Integer],
				Product[cd[-DummyIn@First@VBundlesOfCovD@cd][FromIndexFreeCovDs@x], {n}]
	]; 

ToIndexFree[expr_] :=
	IndexFree[expr //. cd_?CovDQ[_][x_] :> cd[x] /. x_?xTensorQ[___] :> x];


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
);

SetCurvatureRelations[cd_?CovDQ, options___?OptionQ] := (
	AutomaticRules[Evaluate[GiveSymbol[Ricci, cd]], CurvatureRelations[cd, Ricci], options];
	AutomaticRules[Evaluate[GiveSymbol[Riemann, cd]], CurvatureRelations[cd, Riemann], options];
);

CurvatureRelationsQ[cd_?CovDQ] := With[
	{
		ricci 	= GiveSymbol[Ricci, cd], 
		riemann	= GiveSymbol[Riemann, cd]
	},
	Complement[
		CurvatureRelations[cd, Ricci], 
		DownValues[ricci], UpValues[ricci]
	] === {} &&	Complement[
		CurvatureRelations[cd, Riemann], 
		DownValues[riemann], 
		UpValues[riemann]
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
		indices 	= GetIndicesOfVBundle[VBundleOfIndex[ind], 7],
		metricQ		= (FromMetric	/. CheckOptions[options] /. Options[DefCovD]) =!= Null,
		torsionQ	= Torsion		/. CheckOptions[options] /. Options[DefCovD],
		cd2			= ExtendedFrom	/. CheckOptions[options] /. Options[DefCovD],
		definfo		= DefInfo 		/. CheckOptions[options] /. Options[DefCovD],
		ot			= OrthogonalTo 	/. CheckOptions[options] /. Options[DefCovD],
		pw			= ProjectedWith	/. CheckOptions[options] /. Options[DefCovD]
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
			i2d 		= indices[[7]]
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
		]
	]
];

(* TODO: undefmetric hook *)

GiveOutputString[Schouten, covd_] := StringJoin["S", "[", SymbolOfCovD[covd][[2]], "]"];


ToRicci[expr_, cd_?CovDQ] := Composition[
	TFRicciToRicci[#, cd]&,
	EinsteinToRicci[#, cd]&,
	EinsteinCCToRicci[#, cd]&,
	SchoutenToRicci[#, cd]&,
	SchoutenCCToRicci[#, cd]&
][expr];
 
ToRicci[expr_] := Fold[ToRicci, expr, DeleteCases[$CovDs, PD]];

SchoutenToRicci[expr_, cd_?CovDQ] := With[
	{
		ricci 		= GiveSymbol[Ricci, cd],
		rs 			= GiveSymbol[RicciScalar, cd],
		schouten 	= GiveSymbol[Schouten, cd],
		metric 		= MetricOfCovD@cd,
		d 			= DimOfManifold@ManifoldOfCovD@cd
	},
	expr /. schouten[inds__] :> ricci[inds]/(d - 2) - metric[inds] rs[] / (2 (d - 1) (d - 2))
];

SchoutenToRicci[expr_] := Fold[SchoutenToRicci, expr, DeleteCases[$CovDs, PD]];

RicciToSchouten[expr_, cd_?CovDQ] := With[
	{
		ricci 		= GiveSymbol[Ricci, cd],
		rs 			= GiveSymbol[RicciScalar, cd],
		schouten	= GiveSymbol[Schouten, cd],
		metric 		= MetricOfCovD@cd,
		d 			= DimOfManifold@ManifoldOfCovD@cd
	},
	expr /. ricci[inds__] :> schouten[inds] (d - 2) + metric[inds] rs[] /(2 (d - 1))
];

RicciToSchouten[expr_] := Fold[RicciToSchouten, expr, DeleteCases[$CovDs, PD]];

SchoutenCCToRicci[expr_, cd_?CovDQ] := With[
	{
		ricci 		= GiveSymbol[Ricci, cd],
		rs 			= GiveSymbol[RicciScalar, cd],
		schouten	= GiveSymbol[SchoutenCC, cd],
		metric 		= MetricOfCovD@cd,
		d 			= DimOfManifold@ManifoldOfCovD@cd
	},
	expr /. schouten[LI[K_],inds__] :> ricci[inds]/(d - 2) - metric[inds] rs[] / (2 (d - 1) (d - 2)) - 1/2 K  metric[inds]
];

SchoutenCCToRicci[expr_] := Fold[SchoutenCCToRicci, expr, DeleteCases[$CovDs, PD]];

RicciToSchoutenCC[K_][expr_, cd_?CovDQ] := With[
	{
		ricci 		= GiveSymbol[Ricci, cd],
		rs 			= GiveSymbol[RicciScalar, cd],
		schouten	= GiveSymbol[SchoutenCC, cd],
		metric 		= MetricOfCovD@cd,
		d 			= DimOfManifold@ManifoldOfCovD@cd
	},
	expr /. ricci[inds__] :> schouten[LI[K],inds] (d - 2) + metric[inds] rs[] /(2 (d - 1)) + 1/2 (d-2) K metric[inds]
];

RicciToSchoutenCC[K_][expr_] := Fold[RicciToSchoutenCC[K], expr, DeleteCases[$CovDs, PD]];

EinsteinCCToRicci[expr_, cd_?CovDQ] := With[
	{
		ricci 		= GiveSymbol[Ricci, cd],
		rs 			= GiveSymbol[RicciScalar, cd],
		einsteincc	= GiveSymbol[EinsteinCC, cd],
		metric 		= MetricOfCovD@cd,
		d 			= DimOfManifold@ManifoldOfCovD@cd
	},
	expr /. einsteincc[LI[K_], inds__] :> (ricci[inds] + 1/2 metric[inds] (-rs[] + (d - 2) (d - 1) K))
];

EinsteinCCToRicci[expr_] := Fold[EinsteinCCToRicci, expr, DeleteCases[$CovDs, PD]];

RicciToEinsteinCC[K_][expr_, cd_?CovDQ] := With[
	{
		ricci 		= GiveSymbol[Ricci, cd],
		rs 			= GiveSymbol[RicciScalar, cd],
		einsteincc	= GiveSymbol[EinsteinCC, cd],
		metric 		= MetricOfCovD@cd,
		d 			= DimOfManifold@ManifoldOfCovD@cd
	},
	expr /. ricci[inds__] :> (einsteincc[LI[K], inds] - 1/2 metric[inds] (-rs[] + (d - 2) (d - 1) K))
];

RicciToEinsteinCC[K_][expr_] := Fold[RicciToEinsteinCC[K], expr, DeleteCases[$CovDs, PD]];


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



(*************************************)
(* Symmetric imploding and exploding *)
(*************************************)


(* Main function for imploding: *)
SymImplode[expr_] := 
	expr /. p:cd_?CovDQ[_][_] :> CovDToSymmetricImplodedReplace[p];


(* Because symmetric imploding is a time consuming process (mainly because it is
   recurvise), we use caching to speed things up. This is done by storing 
   symmetric implosions as replacement rules in the list $SymImplodedRules.*)
Unprotect@$SymImplodedRules;
$SymImplodedRules={};
Protect@$SymImplodedRules;


ClearAll[CovDToSymmetricImplodedReplace];

(* If there are already rules for expr, return the replaced. 
   It is important to only replace if the full expression matches any of the 
   stored rules. Because of the recursive nature of symmetric imploding, 
   we don't want to replace partial matches. 
   Note that ReplaceAll first tries to replace the whole expression and then 
   moves to subexpressions, so we don't need to take precautions to prevent 
   replacing subexpressions if one of the rules already matches the whole 
   expression. *)
CovDToSymmetricImplodedReplace[expr_] /; MatchQ[expr,Alternatives@@First/@$SymImplodedRules] := 
	expr /. $SymImplodedRules;

(* If there are no previously compute rules, compute the imploded expression. *)
CovDToSymmetricImplodedReplace[expr_] := 
	Module[
		{
			imploded = CovDToSymmetricImploded[expr]
		}
		,
		(* Make rules and save them. *)
		Unprotect@$SymImplodedRules;
		$SymImplodedRules = Union[
			$SymImplodedRules,
			MakeRule[Evaluate@{expr,imploded},MetricOn->None,UseSymmetries->False]
		];
		Protect@$SymImplodedRules;
		(* Return. *)
		imploded
	];

	
(* Worker function for imploding: *)

(* Strip of covariant derivatives. *)
CovDToSymmetricImploded[cd_?CovDQ[i_][inside_]] := 
	CovDToSymmetricImploded[inside,{i},{cd}];

CovDToSymmetricImploded[cd_?CovDQ[i_][inside_],{previousIndices___},{previousCovDs___}] := 
	CovDToSymmetricImploded[inside,{previousIndices,i},{previousCovDs,cd}];

(* Function for reconstructing the original expression. *)
CovDToSymmetricImplodedReconstruct[tensor_[inds___],covdindices_List,covds_List] :=
	Fold[
		Last[#2][First[#2]][#1]&,
		tensor[inds],
		Reverse@Transpose@{covdindices,covds}
	];


(* Implode and symmetrize. *)
CovDToSymmetricImploded[tensor_?xTensorQ[inds___],covdindices_List,covds_List] :=
	With[
		{
			imploded = GiveSymImplodedTensor[covds,tensor],
			origexpr = CovDToSymmetricImplodedReconstruct[tensor[inds],covdindices,covds]
		}
		,
		imploded[inds,Sequence@@covdindices] + SymImplode@ToCanonical@ContractMetric@SortCovDs@SymExplode[ origexpr - imploded[inds,Sequence@@covdindices]]
	];
	
	
(* Returns the name of the imploded tensor and defines it if necessary: *)
ClearAll[GiveSymImplodedTensor];

(* Recursive part for adding derivatives one by one. 
   Note the reverse order because the inner derivatives are last. *)
GiveSymImplodedTensor[{rest__,cd_?CovDQ},tensor_?xTensorQ] :=
	GiveSymImplodedTensor[{rest},GiveSymImplodedTensor[{cd},tensor]]

(* Main part. *)
GiveSymImplodedTensor[{cd_?CovDQ},tensor_?xTensorQ]:=
	(* Construct the new name. *)
	With[
		{
			newname = GiveSymImplodedName[tensor,cd]
		}
		,
		(* Check if we need to define the new tensor. *)
		DefSymImplodedTensor[tensor,cd];
		(* Return the new name. *)
		newname
	];


(* Function that defines an imploded tensor: *)
GiveSymImplodedName[tensor_?xTensorQ,cd_?CovDQ] :=
	SymbolJoin["S",cd,tensor];


(* Defines an imploded tensor. *)
ClearAll[DefSymImplodedTensor];

DefSymImplodedTensor[ tensor_?xTensorQ,cd_?CovDQ,n_Integer] /; n > 1 :=
	(
		DefSymImplodedTensor[tensor,cd];
		DefSymImplodedTensor[GiveSymImplodedName[tensor,cd],cd,n-1];
	);

DefSymImplodedTensor[tensor_?xTensorQ,cd_?CovDQ,1] :=
	DefSymImplodedTensor[tensor,cd];

DefSymImplodedTensor[tensor_?xTensorQ,cd_?CovDQ] :=
	With[
		{
			newname 	=  GiveSymImplodedName[tensor,cd],
			symlist		= Append[SymCovDListOfTensor@tensor, {1+Length@SlotsOfTensor@tensor,cd}],
			slots 		= Append[SlotsOfTensor@tensor,-First@VBundlesOfCovD@cd],
			unimploded 	= UnimplodedTensor@tensor
		},
		If[
			!xTensorQ[newname]
			,
			DefTensor[
				newname@@(DummyIn/@slots),
				Union@DependenciesOfTensor@tensor,
				JoinSGS[
					SymmetryGroupOfTensor[unimploded],
					Symmetric[First/@symlist,Cycles]
				],
				PrintAs-> StringJoin["(",Sequence@@(Last/@SymbolOfCovD/@Last/@symlist),")",PrintAs@unimploded]
			];
			SymCovDListOfTensor[newname]	^= symlist;
			UnimplodedTensor[newname]		^= unimploded;
			SymImplodedQ[newname]			^= True;
		];
	];

(* Stores information about which slots are symmetrically imploded with which covariant derivative. *)
(* The form is {{slotnumber,covd},....}, so e.g. SymCovDListOfTensor[SCDSCDphi] = {{4,CD},{5,CD}} . *)
SymCovDListOfTensor[_] = 
	{};

(* Stores information about the original unimploded tensor. *)
(* E.g. if the imploded tensor is SCDSCDSCDorigtensor, this gives UnimplodedTensor[SCDSCDSCDorigtensor] = origtensor .*)
UnimplodedTensor[tensor_?xTensorQ] := 
	tensor;
	
(* Stores if a tensor is a symmetrically imploded tensor or not. *)
SymImplodedQ[_] = 
	False;
	
	
(* Symmetric exploding: *)

(* Main function for symmetrically exploding. *)
SymExplode[expr_] := 
	expr /. p:tensor_?SymImplodedQ[___] :> SymmetricImplodedToCovD[p];

(* Worker function for symmetrically exploding. *)
SymmetricImplodedToCovD[tensor_?SymImplodedQ[inds___]] :=
	With[
		{
			(* Indices belonging to symmetrized covariant derivatives. *)
			syminds = {inds}[[First/@SymCovDListOfTensor@tensor]],
			(* Indices belonging to the original tensor. *)
			teninds = {inds}[[DeleteCases[Range@Length@SlotsOfTensor@tensor, Alternatives@@(First/@SymCovDListOfTensor@tensor)] ]]
		}
		,
		(* Symmetrize over the cd indices. *)
		Symmetrize[
			(* Reconstruct the expression with derivatives action on the original tensor. *)
			Fold[
				#2[#1]&,
				(* Original tensor with indices. *)
				UnimplodedTensor[tensor]@@teninds,
				(* List of covariant derivatives with indices, e.g. {CD[e],CD[f],...} *)
				MapThread[
					#1[#2]&,
					{Last/@SymCovDListOfTensor@tensor,syminds}
				]
			],
			(* Indices to symmetrize over. *)
			syminds
		]
	];



End[]