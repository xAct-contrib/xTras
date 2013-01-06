BeginPackage["xAct`xTras`Invar`", {
	"xAct`xCore`",
	"xAct`xTensor`",
	"xAct`Invar`",
	"xAct`xTras`xTensor`"
}]


EulerDensity::usage = 
  "EulerDensity[CD] give the Euler density of the curvature tensor of \
the covariant derivative CD. If the manifold has generic dimension, \
you can use EulerDensity[CD,dim] to specify a dimension. Note that it \
omits the square root of the metric determinant, so technically it's not a density.";

InvarWrapper::usage = 
  "InvarWrapper[invarFunction,g][expr,otherargs] wraps an Invar \
function specified by invarFunction s.t. you can use your own options \
for ToCanonical, ContractMetric, and CurvatureRelations.";

(* Simplifying *)

RiemannSimplification::usage = 
  "RiemannSimplification[metric,level][expr] works similarly to \
RiemannSimplify, except that it also works for generic options for \
ToCanonical etc, and works on more general expressions. \
\nNote that it only simplifies expression consisting of Riccis and Riemanns, \
and not of other curvature tensors.";

RicciDivRule::usage = 
  "RicciDivRule[CD] gives rules for rewriting the divergence of the \
Ricci tensor of the given covariant derivative in terms of the Ricci \
scalar.";

RiemannDivRule::usage = 
  "RiemannDivRule[CD] gives rules for rewriting the divergence of the \
Riemann tensor of the given covariant derivative in terms of the \
Ricci tensor.";

FS::usage = 
  "FS is an alias of FullSimplification. Kept for backwards compatibility.";

FullSimplification::usage =
	"FullSimplification[metric][expr] tries to simplify expr as much as possible, \
taking Bianchi identities into account and sorting covariant derivatives. \
It also uses to power of the Invar package to simplify scalar invariants of  \
Riccis and Riemanns (but not of other curvature tensors like the Weyl tensor).";


(* Monomials *)

IncludeDuals::usage = 
  "Option for SingleInvariants, ProductInvariants, RangeInvariants, \
and InvarLagrangian whether to include dual invariants or not.";

Coefficients::usage = "Option for InvarInvariants.";

OrderParameter::usage = "Options for InvarLagrangian.";

SingleInvariants::noduals = 
  "Omitting dual invariants because dimension is other than 4 (due to \
Invar limitation).";

SingleInvariants::oddorder = 
  "The order can only be even, not odd (due to Invar limitation).";

SingleInvariants::usage = 
  "SingleInvariants[metric,order] gives the single invariants of the \
Riemann tensor at that given order (which has the standard level \
specification)";

ProductInvariants::usage = 
  "ProductInvariants[metric,order] gives all product invariants of \
the Riemann tensor at that given order (which has the standard level \
specification)";

InvarLagrangian::usage = 
  "InvarLagrangian[metric, maxorder] gives the most general Lagrangian up to \
maxorder in derivatives of the metric, consisting solely of curvature tensors.";

OrderCoefficient::usage = 
  "OrderCoefficient[order,n] gives a constant symbol for InvarLagrangian.";




Begin["`Private`"]

(********************)
(* Invar extensions *)
(********************)


EulerDensity[cd_?CovDQ] := EulerDensity[cd, DimOfManifold[ManifoldOfCovD[cd]]];

EulerDensity[cd_?CovDQ, D_?EvenQ] := Module[{indices, e1, e2, riemann, n, e},
	indices 	= Table[DummyIn@VBundleOfMetric@MetricOfCovD@cd, {2 D}];
	e 			= GiveSymbol[epsilon, MetricOfCovD[cd]];
	e1 			= e @@ (-indices[[1 ;; D]]);
	e2 			= e @@ (-indices[[D + 1 ;; 2 D]]);
	riemann[i_] := GiveSymbol[Riemann, cd][
		indices[[2 i - 1]],
		indices[[2 i]],
		indices[[2 i + D - 1]],
		indices[[2 i + D]]
	];
	1/2^(D/2) e1 e2 Product[riemann[n], {n, 1, D/2}] // ContractMetric // ToCanonical
];

InvarWrapper[invarFunction_, g_?MetricQ][expr_, otherargs___] := Module[
	{
		cd, i1, i2, i3, ricciscalar, ricci, riemann, rules,
		result, curvrel, monvb, uppder,commutescalars
	},
	
	(* Initialize *)
	cd 			= CovDOfMetric[g];
	{i1,i2,i3} 	= Table[DummyIn@VBundleOfMetric@g, {3}];
	ricci 		= GiveSymbol[Ricci, cd];
	ricciscalar = GiveSymbol[RicciScalar, cd];
	riemann 	= GiveSymbol[Riemann, cd];
   
	(* Store old config values *)
	curvrel 		= CurvatureRelationsQ[cd];
	monvb 			= Options[ToCanonical, UseMetricOnVBundle];
	uppder 			= Options[ContractMetric, AllowUpperDerivatives];
	commutescalars 	= xAct`xTensor`$CommuteCovDsOnScalars;
   
	(* Set config values to Invar compatible settings *)
	SetOptions[
		ToCanonical, 
		UseMetricOnVBundle -> All
	];
	SetOptions[
		ContractMetric, 
		AllowUpperDerivatives -> True
	];
	ClearCurvatureRelations[cd, Verbose -> False];
	xAct`xTensor`$CommuteCovDsOnScalars = False;
   
	(* Make rules to convert the Ricci scalar and Ricci tensor to Riemanns *)
	rules = Join[
		MakeRule[
			Evaluate@
			{
				ricciscalar[], 
				$RicciSign * Scalar@riemann[i1, i2, -i1, -i2]
			},	
			MetricOn -> All
		],
		MakeRule[
			Evaluate@
			{
				ricci[i1, i2], 
				$RicciSign * riemann[i1, i3, i2, -i3]
			},	
			MetricOn -> All
		]
	];
	
	(* Apply them to the expression and run the Invar function *)
	result = Block[{Print},invarFunction[g, expr /. rules, otherargs]];

	(* Reapply the settings as they were before *)
	xAct`xTensor`$CommuteCovDsOnScalars = commutescalars;
	SetOptions[
		ToCanonical, 
		monvb // First
	];
	SetOptions[
		ContractMetric, 
		uppder // First
	];
	If[curvrel, SetCurvatureRelations[cd, Verbose -> False]];
	
	(* Return *)
	result
];

RiemannSimplification[level_Integer][expr_] := 
	Fold[RiemannSimplification[#2,level][#1] &, expr, $Metrics];
	
RiemannSimplification[][expr_] := 
	Fold[RiemannSimplification[#2][#1] &, expr, $Metrics];
	
RiemannSimplification[metric_?MetricQ][expr_] := If[
	MemberQ[Range[6],$InvSimplifyLevel] && !(DimOfManifold@ManifoldOfCovD@CovDOfMetric@metric =!= 4 && $InvSimplifyLevel > 4),
	RiemannSimplification[metric, $InvSimplifyLevel][expr],
	RiemannSimplification[metric, 4][expr]
];

RiemannSimplification[metric_?MetricQ, level_Integer][expr_] := Module[
	{scalar, curvatureTensors,cd},
	
	cd = CovDOfMetric@metric;
	curvatureTensors = {
  		metric,
		GiveSymbol[Ricci,cd],
 		GiveSymbol[RicciScalar,cd],
 		GiveSymbol[Riemann,cd]
	};
	scalar = PutScalar[expr];
	
	(* TODO: if the result contains Cycles something went wrong, and we should return the original expression. *)
	
	(* RiemannSimplify returns expression with a Scalar head, so removing the scalars with the rule
	   is safe. *) 
	NoScalar[scalar /. 
		Scalar[subexpr_] /; SameQ[
			{},
			Complement[
				FindAllOfType[subexpr, Tensor] /. t_?xTensorQ[___] :> t, 
	     		curvatureTensors
	     	] 
		] :> InvarWrapper[RiemannSimplify[#2, Release@level, CurvatureRelationsQ@cd, #1] &, metric][ContractMetric@subexpr]
	]
];



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


FS = FullSimplification;

Options[FullSimplification] ^= {SortCovDs -> True};

FullSimplification[options___?OptionQ][expr_] := Fold[FullSimplification[#2, options][#1] &, expr, $Metrics];
FullSimplification[metric_?MetricQ, options___?OptionQ][expr_] := Module[
	{cd, oldmonv, olduppder, tmp, sortcd, riemannDivRule, ricciDivRule},

	{sortcd} 	= {SortCovDs} /. CheckOptions[options] /. Options[FullSimplification];
	cd 			= CovDOfMetric[metric];
	oldmonv 	= Options[ToCanonical, UseMetricOnVBundle];
	olduppder 	= Options[ContractMetric, AllowUpperDerivatives];
	SetOptions[
		ToCanonical,  
		UseMetricOnVBundle -> If[FreeQ[expr, PD], All, None]
	];
	SetOptions[
		ContractMetric,
		AllowUpperDerivatives -> True
	];
	
	tmp = ToCanonical@ContractMetric[expr, metric];
	tmp = RiemannSimplification[metric][tmp];
	
	(* TODO: Apply dimensional dependent identities *)

	(* Sort covariant derivatives *)
	If[sortcd,
		riemannDivRule 	= RiemannDivRule[cd];
		ricciDivRule 	= RicciDivRule[cd];
		While[!DivFreeQ[tmp, GiveSymbol[Riemann, cd]], 
			tmp = ContractMetric[tmp //. riemannDivRule, metric]
		];
		While[!DivFreeQ[tmp, GiveSymbol[Ricci, cd]], 
			tmp = ContractMetric[tmp //. ricciDivRule, metric]
		];
		tmp = SortCovDs[tmp];
		(* Question: why do we need to do this twice? *)
		While[!DivFreeQ[tmp, GiveSymbol[Riemann, cd]], 
			tmp = ContractMetric[tmp //. riemannDivRule, metric]
		];
		While[!DivFreeQ[tmp, GiveSymbol[Ricci, cd]], 
			tmp = ContractMetric[tmp //. ricciDivRule, metric]
		];
	];
	
	tmp = ToCanonical@ContractMetric[tmp, metric];
	 
	(* TODO: Apply dimensional dependent identities again *)
	(* TODO: Apply bianchi identities *)
	SetOptions[ToCanonical, oldmonv // First];
	SetOptions[ContractMetric, olduppder // First];
	
	tmp
];



Options[SingleInvariants] ^= {IncludeDuals -> False};

SingleInvariants[metric_?MetricQ, maxOrder_Integer, options___?OptionQ] := 
	SingleInvariants[metric, {0, maxOrder}, options];
	
SingleInvariants[metric_?MetricQ, {minOrder_Integer, maxOrder_Integer}, options___?OptionQ] /; minOrder <= maxOrder := 
	SingleInvariants[metric, {#}, options] & /@ Range[minOrder, maxOrder];
	
SingleInvariants[metric_?MetricQ, {order_Integer}, options___?OptionQ] /; order > 0 := Module[
	{duals, dim, cases, steps, dcases, invs, dinvs},
	
	duals = IncludeDuals /. CheckOptions[options] /. Options[SingleInvariants];
	
	If[OddQ[order],
		Message[SingleInvariants::oddorder]; Return[{}];
	];
	
	Block[{Print},
		dim 	= DimOfManifold@ManifoldOfCovD@CovDOfMetric@metric;
		steps 	= If[dim === 4, 6, 4];
		cases 	= InvarCases[order];
		invs 	= RInvs[metric][4, #] & /@ cases;
		If[duals,
			If[dim === 4,
				dcases 	= InvarDualCases[order];
				dinvs 	= DualRInvs[metric][steps, #] & /@ dcases;
				invs = Join[invs, dinvs];
			,
				Message[SingleInvariants::noduals];
			]
		];
		Flatten[InvToRiemann[invs]]
	]
];

SingleInvariants[_?MetricQ, {0}, ___?OptionQ] := {1};

ProductInvariants[metric_?MetricQ, maxOrder_Integer, options___?OptionQ] := 
	ProductInvariants[metric, {0, maxOrder}, options];
	
ProductInvariants[metric_?MetricQ, {minOrder_Integer, maxOrder_Integer}, options___?OptionQ] /; minOrder <= maxOrder := 
	ProductInvariants[metric, {#}, options] & /@ Range[minOrder, maxOrder];
	
ProductInvariants[metric_?MetricQ, {order_Integer}, options___?OptionQ] /; order > 0 := Module[
	{partitions, npinvs, combine},
	
	(* Note we can only partition into even integers, 
	   because the Invar package does not give odd invariants *)
	partitions 	= IntegerPartitions[order];
	npinvs 		= Quiet[
		SingleInvariants[metric, {#}, options] & /@ Range[order],
		{SingleInvariants::oddorder}
	];

	combine[partition_] := Union@Flatten[Outer[Times, Sequence @@ npinvs[[partition]]], 1];
	combine /@ partitions // Flatten // Union
];

ProductInvariants[_?MetricQ, {0}, ___?OptionQ] := {1};

Options[InvarLagrangian] ^= {
	Coefficients -> OrderCoefficient, 
	OrderParameter -> 1
};

InvarLagrangian[metric_?MetricQ, maxOrder_Integer, options___?OptionQ] := 
	InvarLagrangian[metric, {0, maxOrder}, options];
	
InvarLagrangian[metric_?MetricQ, {minOrder_Integer, maxOrder_Integer}, options___?OptionQ] /; minOrder <= maxOrder := 
Module[
	{i},
	Sum[
		InvarLagrangian[metric, {i}, options],
		{i, minOrder, maxOrder}
	]
];

InvarLagrangian[metric_?MetricQ, {order_Integer}, options___?OptionQ] := Module[
	{coefPar, orderPar, NoCoeff, invariants, q, Defconstant},

	{orderPar, coefPar} = {OrderParameter, Coefficients} /. CheckOptions[options] /. Options[InvarLagrangian];

	If[coefPar === None,
		coefPar = NoCoeff
	];
	NoCoeff[__] := 1;
   
	Defconstant[par_Symbol] := If[!ConstantSymbolQ[#], DefConstantSymbol[#]] & /@ Variables[par];
	Defconstant[orderPar];
	
	invariants 	= ProductInvariants[metric, {order}, options];
	q			= Flatten[Array[coefPar, {1, Length[invariants]}, {order, 1}]];
	Defconstant /@ q;
	
	NoScalar[
		orderPar^order * q.Sort[invariants //. CurvatureRelations@CovDOfMetric@metric] 
	]   
];

OrderCoefficientString[o_, n_] := \!\(\*
	TagBox[
		StyleBox[
			RowBox[{
				"\"\<\\!\\(\\*SubsuperscriptBox[\\(c\\), \\(\>\"", 
				"<>", 
				RowBox[{"ToString", "[", "n", "]"}],
				"<>", 
				"\"\<\\), \\(\>\"", 
				"<>", 
				RowBox[{"ToString", "[", "o", "]"}], 
				"<>", 
				"\"\<\\)]\\)\>\""
			}],
			ShowSpecialCharacters -> False,
			ShowStringCharacters -> True,
			NumberMarks -> True
		],
		FullForm
	]
\)

OrderCoefficient[o_, n_] := Module[{symbol},
	symbol = GiveSymbol["Co", o/2, "n", n];
	If[!ConstantSymbolQ[symbol],
		DefConstantSymbol[symbol]
	];
	PrintAs[symbol] ^= OrderCoefficientString[o/2, n];
	symbol
];

End[]
EndPackage[]