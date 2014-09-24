VarL::usage = 
	"VarL[ g[a,b] ][ L ] performs a variation of \!\(TraditionalForm\`\*SqrtBox[\(-g\)]L\) \
with respect to the metric g, and divides with \!\(TraditionalForm\`\*SqrtBox[\(-g\)]\) afterwards.";


(* Background perturbations *)

ExpandBackground::usage = 
  "ExpandBackground[expr,n] expands only the nth order perbutation of \
expr on an arbitrary background, without an expansion parameter or \
1/n! factor. The background is set with the option \
BackgroundSolution.";

PerturbBackground::usage = 
  "PerturbBackground[expr,n] does an (unexpanded) perturbation around \
an arbitrary background for only the order n. This is useful if you \
want to keep symbolic things like Perturbation[EinsteinCD[]] \
unexpanded. The background is set with the option BackgroundSolution.";

ToBackground::usage = 
  "ToBackground[expr] ensures expr is on some background. The \
background is set with the option BackgroundSolution.";

ExpandFlat::usage = 
  "ExpandFlat[expr,n] expands only the nth order perbutation of expr \
on a flat background, without an expansion parameter or 1/n! \
factor.";

PerturbFlat::usage = 
  "PerturbFlat[expr,n] does an (unexpanded) perturbation around a \
flat background for only the order n. This is useful if you want to \
keep symbolic things like Perturbation[EinsteinCD[]] unexpanded.";

ToFlat::usage = 
  "ToFlat[expr] ensures expr is on a flat background, i.e. it sets \
unperturbed curvature tensors to zero, etc.";

FlatRules::usage = 
  "FlatRules[expr] produces produces replacement rules for the \
curvature tensors of CD on a symmetric space of zero curvature. \
Additionally, partial derivatives of metric are also zero.";

BackgroundSolution::usage = 
  "BackgroundSolution is an option for ToBackground. It should be (a list of) replacement rules \
that send curvature tensors to their background values.";

ExtraRules::usage = "ExtraRules is an option for ApplyBackground.";

SymmetricSpaceRules::usage = 
  "SymmetricSpaceRules[CD,K] produces replacement rules for the \
curvature tensors of CD on a symmetric space of constant curvature K.";

EinsteinSpaceRules::usage = 
  "EinsteinSpaceRules[CD,K] produces replacement rules for the \
curvature tensors of CD (except the Riemann and Weyl) on an Einstein space of \
curvature K.";


Begin["`Private`"]



(**************)
(* Variations *)
(**************)

(* 
	The xTensor VarD works fine, but there's a better method using the xPert 
	package. This is due to Cyril Pitrou. See
	http://groups.google.com/group/xact/browse_thread/thread/46f3ae4cbae14ea8/22c9e238d0869271
*)

Unprotect[xAct`xTensor`DefMetric];
If[
	FreeQ[
		Options[xAct`xTensor`DefMetric], 
		DefMetricPerturbation
	], 
	Options[xAct`xTensor`DefMetric] ^= Append[
		Options[xAct`xTensor`DefMetric], 
		DefMetricPerturbation -> True
	];
];
Protect[xAct`xTensor`DefMetric];

xTension["xTras`xPert`", DefMetric, "End"] := xTrasxPertDefMetric;
xTension["xTras`xPert`", DefCovD, "End"] := xTrasxPertDefCovD;

xTrasxPertDefCovD[cd_[ind_], vbundles_, options___?OptionQ] := (
	(* Teach xPert how to expand the new curvature tensors. *)
	xAct`xPert`Private`ExpandPerturbation1[Perturbation[s:GiveSymbol[Schouten,cd][__],n_.],opts___] := 
		ExpandPerturbation[Perturbation[SchoutenToRicci@s, n], opts];
	xAct`xPert`Private`ExpandPerturbation1[Perturbation[s:GiveSymbol[SchoutenCC,cd][__],n_.],opts___] := 
		ExpandPerturbation[Perturbation[SchoutenCCToRicci@s, n], opts];
	xAct`xPert`Private`ExpandPerturbation1[Perturbation[s:GiveSymbol[EinsteinCC,cd][__],n_.],opts___] := 
		ExpandPerturbation[Perturbation[EinsteinCCToRicci@s, n], opts];
);

xTrasxPertDefMetric[signdet_, metric_[-a_, -b_], cd_, options___] :=
	If[
		TrueQ @ And [
			DefMetricPerturbation /. CheckOptions[options] /. Options[DefMetric],
			! ( FlatMetric /. CheckOptions[options] /. Options[DefMetric] ) 
		],	
		DefMetricPerturbation[
			metric,
			GiveSymbol[Perturbation,metric],
			GiveSymbol[PerturbationParameter,metric]
		];
		PrintAs[Evaluate@GiveSymbol[Perturbation,metric]] ^= StringJoin[
			PrintAs[Evaluate@Perturbation],
			PrintAs[Evaluate@metric]
		];
	];


xTension["xTras`xPert`", DefMetricPerturbation, "End"] := xTrasDefMetricPerturbation;

xTrasDefMetricPerturbation[metric_,pert_,param_] :=		
	DefMetricVariation[metric, pert, param];


DefMetricVariation[metric_?MetricQ, per_, Automatic] := 
	DefMetricVariation[metric, per, PerturbationParameter[metric]];
	
DefMetricVariation[metric_?MetricQ, per_, param_] := Module[
	{var, M, vb, a, b},

	vb 		= VBundleOfMetric @ metric;
	M 		= BaseOfVBundle @ vb;
	{a,b}	= GetIndicesOfVBundle[vb,2];

	If[
		Perturbation[ metric[-a,-b] ] =!= per[LI[1],-a,-b],
		Throw@Message[
			DefMetricVariation::error, 
			"Metric perturbation does not match or is not defined."
		]
	];
	Block[{$DefInfoQ = False},
		DefTensor[var[-a, -b], M, Symmetric[{-a, -b}]];
	];
	
	With[
		{
			cd 	 = CovDOfMetric[metric],
			sqrt = Sqrt[SignDetOfMetric[metric] Determinant[metric][]]
		},
		
		(* We can now define a total variation (w.r.t.to metric) as \
		   follows.Note that we're only varying the metric and hence set \
		   variations of any other tensors to zero. *)
		VarDt[metric, expr_] := Module[{mod},
			mod = MapIfPlus[ContractMetric,Expand@Perturbation[expr]];
			ExpandPerturbation@SameDummies@mod /. {
				per[LI[1], inds__] :> var[inds],
				p:(tensor_[LI[1], ___]) /; (xTensorQ[tensor] && tensor =!= per && PerturbationOrder[p]===1) -> 0
			}
		];

		(* The functional derivation is then defined as ... *)
		VarD[metric[-c_Symbol, -d_Symbol], cd][expr_] := Module[{mod,withvar,novar},
			mod 	= Expand@VarDt[metric, expr];
			novar 	= mod /. var[__]->0;
			withvar	= mod - novar;
			VarD[var[-c, -d], cd][withvar]
		];
		VarD[metric[+c_Symbol, +d_Symbol], cd][expr_] := Module[{mod,withvar,novar},
			mod 	= Expand@VarDt[metric, expr];
			novar 	= mod /. var[__]->0;
			withvar = mod - novar;
			-VarD[var[c, d], cd][withvar]
		];

		(* And finally one handy function that varies Lagrangians, 
		   and thus takes care of the square root of the determinant. *)
		VarL[metric[inds__]][L_] := VarL[metric[inds], cd][L];
		VarL[metric[inds__], cd][L_] := VarD[metric[inds], cd][L] + ReplaceDummies[L] VarD[metric[inds], cd][sqrt]/sqrt;  
	];
];



(*************************)
(* Background expansions *)
(*************************)


Options[ToBackground] ^= {
	BackgroundSolution -> {}, 
	ExtraRules -> {}
};

ExpandBackground[expr_, order_Integer: 1, options___?OptionQ] := 
	ToBackground[ExpandPerturbation@PerturbBackground[expr, order, options], options];

PerturbBackground[expr_, order_Integer: 1, options___?OptionQ] := 
	ToBackground[Perturbation[expr, order], options];

ToBackground[expr_, options___?OptionQ] := Module[
	{
		bgRules, extraRules, temp, temprules, CreateSymbol, modexpr
	},
	{bgRules, extraRules} = {BackgroundSolution, ExtraRules} 
		/. CheckOptions[options] 
		/. Options[ToBackground];
	(* Replace the unexpanded perturbations in the expression, 
	   such that we won't set unexpanded perturbation of curvature \
	   tensors to background values later on. *)
	temprules = {};
	CreateSymbol[p_] := Module[{symbol},
		symbol = Unique[temp];
		AppendTo[temprules, symbol -> p];
		symbol
	];
	modexpr = expr /. p : Perturbation[___] :> CreateSymbol[p];
	(* Set curvature tensors to zero and apply optional extra rules. *)
	modexpr = modexpr /. bgRules /. extraRules;
	(* Re-insert the unexpanded pertubations. *)
	modexpr = modexpr /. temprules 
];

ExpandFlat[expr_, order_Integer: 1, options___?OptionQ] := 
	ExpandBackground[expr, order, BackgroundSolution -> FlatRules[expr], options];

PerturbFlat[expr_, order_Integer: 1, options___?OptionQ] := 
	PerturbBackground[expr, order, BackgroundSolution -> FlatRules[expr], options];

ToFlat[expr_, order_Integer: 1, options___?OptionQ] := 
	ToBackground[expr, BackgroundSolution -> FlatRules[expr], options];

FindAllMetrics[expr_] := Module[{tensors, manifolds, vbundles,metrics},
	tensors 	= Union[FindAllOfType[expr, Tensor] /. t_[___] /; xTensorQ[t] :> t];
	manifolds 	= Flatten[Select[HostsOf[#], ManifoldQ] & /@ tensors] // Union;
	vbundles 	= Union[
		Flatten[Select[HostsOf[#], VBundleQ] & /@ tensors], 
		Flatten[TangentBundleOfManifold /@ manifolds]
	];
	metrics = Union[
		Flatten[MetricsOfVBundle /@ vbundles], 
		Flatten[Select[MasterOf /@ tensors, MetricQ]]
	]
];


FlatRules[expr_] := Module[{tensors, manifolds, vbundles, metrics, cds},
	(* First determine what all the covariant derivatives of the expression are. *)
	tensors 	= Union[FindAllOfType[expr, Tensor] /. t_[___] /; xTensorQ[t] :> t];
	manifolds 	= Flatten[Select[HostsOf[#], ManifoldQ] & /@ tensors] // Union;
	vbundles 	= Union[
		Flatten[Select[HostsOf[#], VBundleQ] & /@ tensors], 
		Flatten[TangentBundleOfManifold /@ manifolds]
	];
	metrics = Union[
		Flatten[MetricsOfVBundle /@ vbundles], 
		Flatten[Select[MasterOf /@ tensors, MetricQ]]
	];
	cds = CovDOfMetric /@ metrics;
	(* Create rules that set the curvature tensors of the CDs to zero. *)
	Flatten[FlatRules /@ cds]
];

FlatRules[expr_] := Flatten[ FlatRules[CovDOfMetric[#]]& /@ FindAllMetrics[expr] ];

FlatRules[CD_?CovDQ] := With[
	{
		metric = MetricOfCovD[CD],	
		a = #1,
		b = #2,
		c = #3
	},
	Join[
		SymmetricSpaceRules[CD, 0],
		Block[{Print}, MakeRule[{PD[a]@metric[b, c], 0}, MetricOn -> All]]
	]
]&[Sequence@@GetIndicesOfVBundle[First@VBundlesOfCovD@CD, 3]];



SymmetricSpaceRules[CD_?CovDQ, K_?ConstantExprQ] := Module[
	{
		a,b,c,d, pa,pb,pc,pd, L, MR, 
		D		= DimOfManifold[ManifoldOfCovD[CD]],
		metric	= MetricOfCovD[CD],
		vb 		= First@VBundlesOfCovD[CD]
	},
	{a,b,c,d}		= GetIndicesOfVBundle[vb,4];
	{pa,pb,pc,pd}	= With[{vbpmq = SymbolJoin[vb,"`pmQ"]},
		PatternTest[Pattern[#,Blank[]],vbpmq]& /@ {a,b,c,d}
	];
	
	(* 
	   The reason for this funny construction is that we don't want to evaluate
	   curvature tensors with brackets, because some might be zero 
	   (e.g. the Weyl tensor in three dimension) and some might have other
	   downvalues associated to them by the user.
	   We want to return rules of the form
	   
	       HoldPattern[ RicciCD[a_?TangentM`pmQ,b_?TangentM`pmQ] ] :> RHS
	  
	   The problem is to get the tensor heads, for which we need to do some evaluating. 
	   That's why we wrap stuff in the MR function, because its arguments
	   will get evaluated but the complete thing in HoldPattern[] won't.
	  
	   Note that using a With + MakeRule construction won't help,
	   because even though MakeRule has the attribute HoldFirst, it does evaluate
	   its LHS along the way. 
	*) 
	MR[head_,inds___][rhs_] := RuleDelayed[HoldPattern[head[inds]], rhs];

	List[
		MR
			[GiveSymbol[Riemann, CD], pa, pb, pc, pd]
			[K (metric[a, c] metric[b, d] - metric[a, d] metric[b, c])]
		,
		MR
			[GiveSymbol[Weyl, CD], pa, pb, pc, pd]
			[0]
		,
		MR
			[GiveSymbol[Kretschmann, CD]]
			[ 2 (D-1) D K^2 ]
		,
		MR
			[GiveSymbol[Ricci, CD], pa, pb]
			[$RicciSign K (D-1) metric[a, b]]
		,
		MR
			[GiveSymbol[TFRicci, CD],pa,pb] 
			[0]
		,
		MR
			[GiveSymbol[RicciScalar, CD]] 
			[$RicciSign D*(D-1) K ] 
		,
		MR
			[GiveSymbol[Schouten, CD],pa,pb] 
			[$RicciSign 1/2 K metric[a,b] ]
		,
		MR
			[GiveSymbol[SchoutenCC,CD],LI[L_],pa,pb]
			[1/2 ($RicciSign*K - L)metric[a,b] ]
		,
		MR
			[GiveSymbol[Einstein, CD], pa, pb] 
			[$RicciSign K (D-1) (1 - D/2) metric[a, b] ]
		,
		MR
			[GiveSymbol[EinsteinCC,CD], LI[L_], pa, pb] 
			[1/2 (D-2)(D-1)(L-$RicciSign*K)metric[a,b]]
	]
];

EinsteinSpaceRules[CD_?CovDQ, K_?ConstantExprQ] :=
	(* We just remove the Riemmann, Weyl, and Kretschmann rules, which are the first three. *)
	Part[SymmetricSpaceRules[CD,K], Span[4, -1]];

End[]