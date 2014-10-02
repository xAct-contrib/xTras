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

FullSimplification::usage =
	"FullSimplification[metric][expr] tries to simplify expr as much as possible, \
taking Bianchi identities into account and sorting covariant derivatives. \
It also uses to power of the Invar package to simplify scalar invariants of  \
Riccis and Riemanns (but not of other curvature tensors like the Weyl tensor).";


(* Monomials *)

IncludeDuals::usage = 
  "IncludeDuals is an option for SingleInvariants, ProductInvariants, RangeInvariants, \
and InvarLagrangian whether to include dual invariants or not.";

Coefficients::usage = 
	"Coefficients is an option for InvarLagrangian that specifies the coefficients in the Lagrangian. \
It can either be a function that takes two arguments, or None.";

OrderParameter::usage = 
	"OrderParameter is an option for InvarLagrangian that specifies \
the parameter that labels the order of derivatives. The default is 1.";

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



Begin["`Private`"]

(********************)
(* Invar extensions *)
(********************)


Options[EulerDensity] ^= 
	{
		Verbose -> False
	};

EulerDensity[cd_?CovDQ, options : OptionsPattern[]] := 
	EulerDensity[cd, DimOfManifold @ ManifoldOfCovD @ cd, options];

EulerDensity[cd_?CovDQ, dimension_?EvenQ, OptionsPattern[]]:=
	With[
		{
			indices = Sort @ GetIndicesOfVBundle[VBundleOfMetric @ MetricOfCovD @ cd, dimension],
			sign = SignDetOfMetric[MetricOfCovD @ cd],
			rangeDim = Range @ dimension,
			map = If[
				OptionValue @ Verbose, 
				MapTimed[#1, #2, Description -> "Computing Euler density"]&, 
				Map
			]
		},
		With[
			{
				riemannproduct = Product[
					GiveSymbol[Riemann,cd][
						Slot[2i-1],
						Slot[2i],
						-indices[[2i-1]],
						-indices[[2i]]
					],
					{i,1,dimension/2}
				]
			},
			PutScalar @ SameDummies @ Total @ map[
				(* Reconstruction function. *)
				sign * Signature[#] * ToCanonical[riemannproduct& @@ #]& @ PermuteList[indices, InversePerm @ #] &,
				(* List of permutations: do a transversal over dim/2 pairs of symmetric indices.
				   This is valid because the Riemann is always antisymmetric in its first pair of indices,
				   and these permutations correspond to indices on the generalized (antisymmetric) delta,
				   so the antisymmetry cancels out and becomes symmetric. *) 
				TransversalInSymmetricGroup[
					StrongGenSet[rangeDim, GenSet @@ (Cycles /@ Partition[rangeDim, 2])], 
					Symmetric @ rangeDim
				]
			]
		]
	];


GetInvarOptions[cd_] := {
	CurvatureRelationsQ[cd],
	UseMetricOnVBundle /. Options[ToCanonical],
	AllowUpperDerivatives /. Options[ContractMetric],
	xAct`xTensor`$CommuteCovDsOnScalars
};

SetInvarOptions[cd_] := ResetInvarOptions[cd, {False, All, True, False}];

ResetInvarOptions[cd_, {curvatureRelationQ_, useMetricOnVBundle_, allowUpperDerivatives_, commuteScalars_}] := (
	If[curvatureRelationQ, 
		SetCurvatureRelations[cd, Verbose -> False],
		ClearCurvatureRelations[cd, Verbose -> False]
	];
	SetOptions[
		ToCanonical, 
		UseMetricOnVBundle -> useMetricOnVBundle
	];
	SetOptions[
		ContractMetric, 
		AllowUpperDerivatives -> allowUpperDerivatives
	];
	xAct`xTensor`$CommuteCovDsOnScalars = commuteScalars;
);


InvarWrapper[invarFunction_, g_?MetricQ][expr_, otherargs___] := Module[
	{
		cd 		= CovDOfMetric[g],
		options = GetInvarOptions[CovDOfMetric@g],
		result
	},
	(* Set options to Invar compatible settings. *)
	SetInvarOptions[cd];
	(* Run the Invar function.
	   We need to catch the possible Abort (thrown if the database is not installed) because 
	   the options must be reset. *)
	CheckAbort[
		result = Block[{Print},invarFunction[g, expr, otherargs]],
		ResetInvarOptions[cd, options];
		Abort[];
	];
	(* Reapply the options as they were before *)
	ResetInvarOptions[cd, options];	
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
	{result, curvatureTensors,cd},
	
	cd = CovDOfMetric@metric;
	curvatureTensors = {
  		metric,
		GiveSymbol[Ricci,cd],
 		GiveSymbol[RicciScalar,cd],
 		GiveSymbol[Riemann,cd]
	};
		
	(* RiemannSimplify returns expression with a Scalar head, so removing the scalars with the rule
	   is safe. *) 
	result = NoScalar[PutScalar[expr] /. 
		Scalar[subexpr_] /; SameQ[
			{},
			Complement[
				FindAllOfType[subexpr, Tensor] /. t_?xTensorQ[___] :> t, 
	     		curvatureTensors
	     	] 
		] :> InvarWrapper[
			RiemannSimplify[#2, Release@level, CurvatureRelationsQ@cd, #1] &, 
			metric
		][ContractMetric@subexpr]
	];

	(* If the result contains Cycles something went wrong, and we return the original expression. *)
	If[FreeQ[expr, Cycles] && !FreeQ[result, Cycles],
		Message[
			RiemannSimplification::error, 
			"Failed to simplify Riemann tensors. Most likely caused by automatic rules on the Riemann tensor. Returning unmodified input expression."
		];
		expr,
		result
	]
];

Options[FullSimplification] ^= {SortCovDs -> True};

FullSimplification[options___?OptionQ][expr_] := Fold[FullSimplification[#2, options][#1] &, expr, $Metrics];
FullSimplification[metric_?MetricQ, options___?OptionQ][expr_] := Module[
	{
		cd			= CovDOfMetric[metric],
		oldmonv		= Options[ToCanonical, UseMetricOnVBundle],
		olduppder	= Options[ContractMetric, AllowUpperDerivatives],
		sortcd		= SortCovDs /. CheckOptions[options] /. Options[FullSimplification],
		riemann		= GiveSymbol[Riemann, CovDOfMetric@metric],
		ricci		= GiveSymbol[Ricci, CovDOfMetric@metric],
		tmp 
	},

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

	(* Sort covariant derivatives *)
	If[sortcd,
		
		While[!DivFreeQ[tmp, riemann], 
			tmp = ContractMetric[SortCovDsToDiv[riemann,cd][tmp] /. CurvatureRelationsBianchi[cd, Riemann], metric]
		];
		While[!DivFreeQ[tmp, ricci], 
			tmp = ContractMetric[SortCovDsToDiv[ricci,cd][tmp] /. CurvatureRelationsBianchi[cd, Ricci], metric]
		];
		
		tmp = SortCovDs[tmp];
		
		(* Question: why do we need to do this twice? *)
		While[!DivFreeQ[tmp, riemann], 
			tmp = ContractMetric[SortCovDsToDiv[riemann,cd][tmp] /. CurvatureRelationsBianchi[cd, Riemann], metric]
		];
		While[!DivFreeQ[tmp, ricci], 
			tmp = ContractMetric[SortCovDsToDiv[ricci,cd][tmp] /. CurvatureRelationsBianchi[cd, Ricci], metric]
		];
	];
	
	tmp = ToCanonical@ContractMetric[tmp, metric];
	 
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
	Coefficients -> (DefNiceConstantSymbol["C", {"n", #2}, {"o", #1/2}]&), 
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

End[]