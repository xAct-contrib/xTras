(****************)
(*              *)
(*    Setup     *)
(*              *)
(****************)

$CommuteCovDsOnScalars = True;
SetOptions[ToCanonical, UseMetricOnVBundle -> All];
SetOptions[ContractMetric, AllowUpperDerivatives -> True];


Test[
	DefManifold[
		M,
		dim,
		IndexRange[a,l]
	]
	,
	Null
	,
	TestID->"xTensor-20130102-C1X1X5"
]

Test[
	DefMetric[
		-1,
		metric[-a,-b],
		CD,
		PrintAs -> "g",
		CurvatureRelations -> True,
		SymCovDQ -> True
	]
	,
	Null
	,
	TestID->"xTensor-20130723-N4O7F9"
]



DefConstantSymbol[K];


(*******************)
(*                 *)
(*  ConstantExprQ  *)
(*                 *)
(*******************)

Test[
	ConstantExprQ[ 1 / (K + 2) ]
	,
	True
	,
	TestID->"xTensor-20130723-T9C9W5"
]

Test[
	ConstantExprQ[ K metric[-a,-b] ] 
	,
	False
	,
	TestID->"xTensor-20130102-J0M4U9"
]


(*******************************)
(*                             *)
(*     Index free notation     *)
(*                             *)
(*******************************)

Test[
	FromIndexFree[ IndexFree@RiemannCD ]
	,
	RiemannCD[-a,-b,-c,-d]
	,
	TestID->"xTensor-20130402-W2F7U9"
]


Test[
	FromIndexFree[ IndexFree@CD@CD@RicciCD ]
	,
	CD[-a,-b]@RicciCD[-c,-d]
	,
	TestID->"xTensor-20130402-H4V1O5"
]

Test[
	FromIndexFree[ CD[-a]@CD[-b]@RicciCD[-c,-d] ]
	,
	CD[-a]@CD[-b]@RicciCD[-c,-d]
	,
	TestID->"xTensor-20130402-A3J6N8"
]

Test[
	ToIndexFree[ CD[-a]@CD[-b]@RicciCD[-c,-d] ]
	,
	IndexFree[ CD@CD@RicciCD ]
	,
	TestID->"xTensor-20130402-D1Z2D6"
]


Test[
	ToIndexFree[ IndexFree@CD@CD@RicciCD ]
	,
	IndexFree@CD@CD@RicciCD
	,
	TestID->"xTensor-20130402-C8K9Z4"
]


Test[
	TermsOf[ RiemannCD[-a,-b,-c,-d] RicciCD[c,d] + metric[-a,-b] + RicciCD[-a,-b]  ]
	,
	{ IndexFree[ metric ], IndexFree[ RicciCD ], IndexFree[ RicciCD RiemannCD ] }
	,
	TestID->"xTensor-20130608-I5K8V6"
]

Test[
	TermsOf[ K + RicciScalarCD[] + (K + 2) ( RicciCD[a,b]RicciCD[-a,-b] + RicciScalarCD[] ) ]
	,
	{ IndexFree @ 1, IndexFree[ RicciCD^2 ], IndexFree @ RicciScalarCD }
	,
	TestID->"Algebra-20130821-W6J9D3"
]


(*************************)
(*                       *)
(*  Curvature relations  *)
(*                       *)
(*************************)

Test[
	SymRiemannCD[a,-a,b,c]
	,
	RicciCD[b,c]
	,
	TestID->"xTensor-20130102-O4A0D6"
]

Test[
	SymRiemannCD[a,b,-a,c]
	,
	-1/2 RicciCD[b,c]
	,
	TestID->"xTensor-20140207-Z4I4V3"
]


Test[
	CurvatureRelationsQ[CD]
	,
	True
	,
	TestID->"xTensor-20140207-Q7O3S0"
]

ClearCurvatureRelations[CD]

Test[
	CurvatureRelationsQ[CD]
	,
	False
	,
	TestID->"xTensor-20130102-K0R2E7"
]

Test[
	RiemannCD[-a,-b,-c,b]
	,
	RiemannCD[-a,-b,-c,b]
	,
	TestID->"xTensor-20130102-B1P1B8"
]

Test[
	RicciCD[-a,a]
	,
	RicciCD[-a,a]
	,
	TestID->"xTensor-20130102-N8B2Z0"
]

Test[
	SymRiemannCD[a,-a,b,c]
	,
	SymRiemannCD[a,-a,b,c]
	,
	TestID->"xTensor-20140207-T8X9W2"
]

Test[
	SymRiemannCD[a,b,-a,c]
	,
	SymRiemannCD[a,b,-a,c]
	,
	TestID->"xTensor-20140207-F2J3O8"
]


SetCurvatureRelations[CD]

Test[
	CurvatureRelationsQ[CD]
	,
	True
	,
	TestID->"xTensor-20130102-T2U4M0"
]

Test[
	RiemannCD[-a,-b,-c,b]
	,
	RicciCD[-a,-c]
	,
	TestID->"xTensor-20130102-C9B8D1"
]

Test[
	RicciCD[-a,a]
	,
	RicciScalarCD[]
	,
	TestID->"xTensor-20130102-Y5Y4E2"
]

Test[
	SymRiemannCD[a,-a,b,c]
	,
	RicciCD[b,c]
	,
	TestID->"xTensor-20140207-D1K6G8"
]

Test[
	SymRiemannCD[a,b,-a,c]
	,
	-1/2 RicciCD[b,c]
	,
	TestID->"xTensor-20140207-R4K7S2"
]



(**********************************)
(*                                *)
(*  Contracted Bianchi identities *)
(*                                *)
(**********************************)

Test[
	ContractMetric[CD[-a][ EinsteinToRicci@EinsteinCD[a,b] ]] /. CurvatureRelationsBianchi[CD]
	,
	0
	,
	TestID->"xTensor-20130126-B1M8S4"
]

Test[
	ToCanonical@ContractMetric[ 
		metric[-a,-c] Antisymmetrize[CD[e]@RiemannCD[a,b,c,d],{a,b,e}] 
	] /. CurvatureRelationsBianchi[CD] // ToCanonical
	,
	0
	,
	TestID->"xTensor-20130126-K9Y5J9"
]

Test[
	CD[a]@RicciCD[-a, -b] /. CurvatureRelationsBianchi[CD]
	,
	CD[-b][RicciScalarCD[]]/2
	,
	TestID->"Invar-20130102-Z9B2U8"
]

Test[
	CD[f]@RicciCD[-g, -f] /. CurvatureRelationsBianchi[CD]
	,
	CD[-g][RicciScalarCD[]]/2
	,
	TestID->"xPert-20130124-Q1C6L4"
]


Test[
	CD[b]@RicciCD[a, -b] /. CurvatureRelationsBianchi[CD]
	,
	CD[a][RicciScalarCD[]]/2
	,
	TestID->"Invar-20130102-N0C0V5"
]

Test[
	CD[a]@RiemannCD[-a, -b, -c, -d] /. CurvatureRelationsBianchi[CD] // ToCanonical
	,
	CD[-c][RicciCD[-b, -d]] - CD[-d][RicciCD[-b, -c]]
	,
	TestID->"Invar-20130102-V9L4H6"
]

Test[
	CD[b]@RiemannCD[-a, -b, -c, -d] /. CurvatureRelationsBianchi[CD] // ToCanonical
	,
	-CD[-c][RicciCD[-a, -d]] + CD[-d][RicciCD[-a, -c]]
	,
	TestID->"Invar-20130102-Z5B4W8"
]

Test[
	CD[c]@RiemannCD[-a, -b, -c, -d] /. CurvatureRelationsBianchi[CD] // ToCanonical
	,
	CD[-a][RicciCD[-b, -d]] - CD[-b][RicciCD[-a, -d]]
	,
	TestID->"Invar-20130102-Z7Y9G1"
]

Test[
	CD[d]@RiemannCD[-a, -b, -c, -d] /. CurvatureRelationsBianchi[CD] // ToCanonical
	,
	-CD[-a][RicciCD[-b, -c]] + CD[-b][RicciCD[-a, -c]]
	,
	TestID->"Invar-20130102-N2G6L7"
]

(*****************************)
(*                           *)
(*  Extra curvature tensors  *)
(*                           *)
(*****************************)

Test[
	xTensorQ /@ {SchoutenCD, EinsteinCCCD, SchoutenCCCD, SymRiemannCD}
	,
	{True, True, True, True}
	,
	TestID->"xTensor-20130101-K2C1H5"
]

Test[
	Simplification@RiemannToWeyl@SchoutenToRicci[
		RiemannCD[-a, -b, -c, -d] - 
		metric[-b, -d]*SchoutenCD[-a, -c] +	metric[-b, -c]*SchoutenCD[-a, -d] + 
	 	metric[-a, -d]*SchoutenCD[-b, -c] - metric[-a, -c]*SchoutenCD[-b, -d]
	]
	,
	WeylCD[-a,-b,-c,-d]
	,
	TestID->"xTensor-20130102-C0Y2O2"
]

Test[
	Simplification@RicciToSchouten@WeylToRiemann[ WeylCD[-a,-b,-c,-d] ]
	,
	RiemannCD[-a, -b, -c, -d] - 
		metric[-b, -d]*SchoutenCD[-a, -c] +	metric[-b, -c]*SchoutenCD[-a, -d] + 
	 	metric[-a, -d]*SchoutenCD[-b, -c] - metric[-a, -c]*SchoutenCD[-b, -d]
	,
	TestID->"xTensor-20130102-A2I0C5"
]

Test[
	Simplification@RicciToSchouten[ RicciCD[-a, -b] - metric[-a, -b] / (2 dim - 2) RicciScalarCD[] ]
	,
	(dim-2) SchoutenCD[-a,-b]
	,
	TestID->"xTensor-20130102-V9X1H6"
]

Test[
	Apart@Simplification@SchoutenToRicci[ (dim-2) SchoutenCD[-a, -b] ]
	,
	RicciCD[-a, -b] - metric[-a, -b] /(2 ( dim - 1) ) RicciScalarCD[]
	,
	TestID->"xTensor-20130101-O9Y8H7"
]

Test[
	Simplification@SchoutenCCToRicci[
		SchoutenCCCD[LI[K],-a, -b] - 
		 ( 1/(dim-2) ( RicciCD[-a, -b] - metric[-a, -b] / (2 dim - 2) RicciScalarCD[])
		   -1/2 metric[-a, -b] K )
	]
	,
	0
	,
	TestID->"xTensor-20130102-L3I6J4"
]

Test[
	Simplification@RicciToSchoutenCC[K][
		SchoutenCCCD[LI[K],-a, -b] - 
		 ( 1/(dim-2) ( RicciCD[-a, -b] - metric[-a, -b] / (2 dim - 2) RicciScalarCD[])
		   -1/2 metric[-a, -b] K )
	]
	,
	0
	,
	TestID->"xTensor-20130102-P7K2P4"
]

Test[
	Simplification@EinsteinCCToRicci[
		EinsteinCCCD[LI[K],-a, -b] - 
		 ( RicciCD[-a, -b] - 1/2 metric[-a, -b] RicciScalarCD[] + 1/2 (dim-2)(dim-1) metric[-a, -b] K )
	]
	,
	0
	,
	TestID->"xTensor-20130102-O6X6P9"
]

Test[
	Simplification@RicciToEinsteinCC[K][
		EinsteinCCCD[LI[K],-a, -b] - 
		 ( RicciCD[-a, -b] - 1/2 metric[-a, -b] RicciScalarCD[] + 1/2 (dim-2)(dim-1) metric[-a, -b] K )
	]
	,
	0
	,
	TestID->"xTensor-20130102-R0I1J2"
]


Test[
	SchoutenCD[-a, -b] - SchoutenCCCD[LI[K], -a, -b] // ToRicci
	,
	1/2 K metric[-a,-b]
	,
	TestID->"xTensor-20130105-X0J1K1"
]

Test[
	EinsteinCD[-a, -b] - EinsteinCCCD[LI[K], -a, -b] // EinsteinToRicci // EinsteinCCToRicci
	,
	EinsteinCD[-a, -b] - EinsteinCCCD[LI[K], -a, -b] // ToRicci
	,
	TestID->"xTensor-20130105-W8L0H1"
]

Test[
	TFRicciCD[-a, -b] // ToRicci
	,
	TFRicciCD[-a, -b] // TFRicciToRicci
	,
	TestID->"xTensor-20130105-G6O1I6"
]

Test[
	ContractMetric[metric[a,b] (  ( # - EinsteinCCToRicci[#] )&[EinsteinCCCD[LI[K], -a, -b]] )]
	,
	0
	,
	TestID->"xTensor-20130126-A4I4Y7"
]

Test[
	SymRiemannCD[a,b,c,d] // SymRiemannToRiemann
	,
	RiemannCD[a, c, b, d]/2 + RiemannCD[a, d, b, c]/2
	,
	TestID->"xTensor-20140207-Y5V1W6"
]

Test[
	RiemannCD[a,b,c,d] // RiemannToSymRiemann
	,
	(2*SymRiemannCD[a, c, b, d])/3 - (2*SymRiemannCD[a, d, c, b])/3
	,
	TestID->"xTensor-20140207-K2T4K2"
]

Test[
	RiemannCD[e,f,g,h] // SymmetryOfExpression
	,
	RiemannCD[e,f,g,h] // RiemannToSymRiemann // SymmetryOfExpression
	,
	TestID->"xTensor-20140207-F1O8A9"
]

Test[
	SymRiemannCD[e,f,g,h] // SymmetryOfExpression
	,
	SymRiemannCD[e,f,g,h] // SymRiemannToRiemann // SymmetryOfExpression
	,
	TestID->"xTensor-20140207-O2I5K1"
]

Test[
	RiemannCD[-a,-b,-c,-d]//RiemannToSymRiemann//SymRiemannToRiemann//ToCanonical
	,
	RiemannCD[-a, -b, -c, -d] // RiemannYoungProject
	,
	TestID->"xTensor-20140207-Z6C6E0"
]

Test[
	SymRiemannCD[-a,-b,-c,-d]//SymRiemannToRiemann //RiemannToSymRiemann//ToCanonical
	,
	SymRiemannCD[-a,-b,-c,-d]// YoungProject[#, {{-a, -b}, {-c, -d}},ManifestSymmetry -> Symmetric] &
	,
	TestID->"xTensor-20140207-Q2F1X7"
]



(***************************)
(*                         *)
(*  Covariant derivatives  *)
(*                         *)
(***************************)

Test[
	SortedCovDsQ[ CD[-a]@CD[-b]@RicciCD[-c,-d] ]
	,
	False
	,
	TestID->"xTensor-20130102-R9R1L7"
]

Test[
	SortedCovDsQ[ CD[-b]@CD[-a]@RicciCD[-c,-d] ]
	,
	True
	,
	TestID->"xTensor-20130102-O9Z7B0"
]

Test[
	DivFreeQ[ CD[-b]@CD[-a]@RicciCD[-c,b], RicciCD, CD ]
	,
	False
	,
	TestID->"xTensor-20130102-K3W0M2"
]

Test[
	DivFreeQ[ CD[-b]@CD[-a]@RicciCD[-c,-d], RicciCD, CD ]
	,
	True
	,
	TestID->"xTensor-20130102-N0Q6G8"
]

Test[
	Simplification@SortCovDsToDiv[RicciCD][
		CD[a]@CD[b]@RicciCD[-a, -c] -
		(RicciCD[b, a]*RicciCD[-c, -a] - RicciCD[a, d]*
		RiemannCD[b, -a, -c, -d] + CD[b][CD[-a][RicciCD[-c, a]]] )
	]
	,
	0
	,
	TestID->"xTensor-20130102-J2I5V1"
]

Test[
	DivFreeQ[ CD[a]@CD[b]@CD[c]@CD[d]@CD[e]@RicciCD[-a, -f], RicciCD, CD ]
	,
	False
	,
	TestID->"xTensor-20130102-V1I0S4"
]

Test[
	DivFreeQ[
		Simplification[ 
			SortCovDsToDiv[RicciCD][ CD[a]@CD[b]@CD[c]@CD[d]@CD[e]@RicciCD[-a, -f] ] -
			CD[b]@CD[c]@CD[d]@CD[e]@CD[a]@RicciCD[-a, -f]
		],
		RicciCD, CD
	]
	,
	True
	,
	TestID->"xTensor-20130102-B8D7D2"
]

Test[
	Simplification[
		SortCovDsToBox[RicciCD][ CD[-a]@CD[a]@CD[-b]@RicciCD[-c, -d] ] -
		(CD[-b][CD[-e][CD[e][RicciCD[-c, -d]]]] + 
		 RicciCD[-b, e]*CD[-e][RicciCD[-c, -d]] - 
		 2*RiemannCD[-b, e, -d, f]*CD[-e][RicciCD[-c, -f]] - 
		 2*RiemannCD[-b, e, -c, f]*CD[-e][RicciCD[-d, -f]] - 
		 RicciCD[-d, e]*CD[-f][RiemannCD[-b, f, -c, -e]] - 
		 RicciCD[-c, e]*CD[-f][RiemannCD[-b, f, -d, -e]])
	]
	,
	0
	,
	TestID->"xTensor-20130102-O5B5L4"
]


(***************************)
(*                         *)
(*     Killing vectors     *)
(*                         *)
(***************************)

DefTensor[V[a], M, KillingVectorOf -> metric]

Test[
	KillingVectorQ[V,metric]
	,
	True
	,
	TestID->"xTensor-20130102-M1Q2G3"
]

Test[
	MetricOfKillingVector[V]
	,
	metric
	,
	TestID->"xTensor-20130102-P1G3Z1"
]

Test[
	LieD[V[-a]][metric[-a, -b]]
	,
	0
	,
	TestID->"xTensor-20130102-W9B4F0"
]

Test[
	ToCanonical[ CD[a]@V[b] ]
	,
	-CD[b]@V[a]
	,
	TestID->"xTensor-20130102-I5S5V1"
]

Test[
	ToCanonical[ CD[b]@V[a] ]
	,
	CD[b]@V[a]
	,
	TestID->"xTensor-20130102-J4E1T6"
]

Test[
	ToCanonical[ CD[-c]@CD[-b]@V[-a] - RiemannCD[-a,-b,-c,d] V[-d] ]
	,
	0
	,
	TestID->"xTensor-20130102-O0O5N1"
]

(****************************)
(*                          *)
(* GradChristoffelToRiemann *)
(*                          *)
(****************************)

Test[
	ChangeCurvature[RiemannCD[-a,-b,-c,d], CD, PD] // GradChristoffelToRiemann // ToCanonical
	,
	RiemannCD[-a,-b,-c,d]
	,
	TestID->"xTensor-20131121-Q5H0O6"
]

Test[
	ChangeCurvature[RiemannCD[-b,a,-c,-d], CD, PD] // GradChristoffelToRiemann // ContractMetric // ToCanonical
	,
	-RiemannCD[a,-b,-c,-d]
	,
	TestID->"xTensor-20131121-L3S5O2"
]

Test[
	ChangeCurvature[RiemannCD[-b,a,-c,-d] RiemannCD[c,d,-e,-f], CD, PD] // GradChristoffelToRiemann // ContractMetric // ToCanonical // ScreenDollarIndices
	,
	-RiemannCD[a,-b,c,d] RiemannCD[-e,-f,-c,-d]
	,
	TestID->"xTensor-20131121-O8R5S1"
]




(*************************************)
(*                                   *)
(* Symmetrized covariant derivatives *)
(*                                   *)
(*************************************)


Test[
	Last@SymmetryOf[CD[a,b]@RicciScalarCD[]]
	,
	Symmetric[{1, 2}, Cycles]
	,
	TestID->"xTensor-20140206-G5W8M3"
]

Test[
	Last@SymmetryOf[CD[a,b,c,d,e]@RicciScalarCD[]]
	,
	Symmetric[{1, 2, 3, 4, 5}, Cycles]
	,
	TestID->"xTensor-20140206-P1X2R3"
]

Test[
	CD[b, a]@RicciCD[d, e] - CD[a, b]@RicciCD[d, e] // ToCanonical
	,
	0
	,
	TestID->"xTensor-20140206-I2V7M4"
]

Test[
	CD[a, b, c, d]@RicciScalarCD[] // ExpandSymCovDs
	,
	CD[a]@CD[b]@CD[c]@CD[d]@RicciScalarCD[] // Symmetrize
	,
	TestID->"xTensor-20140206-B1B6D2"
]

DefTensor[T1[a],M];
DefTensor[S2[a,b],M,Symmetric[{a,b}]];
DefTensor[S3[a,b,c],M,Symmetric[{a,b,c}]];
DefTensor[S4[a,b,c,d],M,Symmetric[{a,b,c,d}]];

Test[
	xAct`xTras`Private`PartitionedSymmetrize[S2@@#1 S3@@#2 &, {a, b, c, d, e}, {2, 3}] // Expand
	,
	S2[a, b] S3[c, d, e] // Symmetrize // ToCanonical
	,
	TestID->"xTensor-20140206-U7X6W0"
]

Test[
	xAct`xTras`Private`PartitionedSymmetrize[T1@@#1 S4@@#2 &, {a,b,c,d,e},{1,4}] // Expand
	,
	T1[a]S4[b,c,d,e]//Symmetrize//ToCanonical
	,
	TestID->"xTensor-20140206-P9K7L2"
]


Test[
	xAct`xTras`Private`PartitionedSymmetrize[S4@@#1 &, {a, b, c, d}, {4}]
	,
	S4[a,b,c,d]
	,
	TestID->"xTensor-20140206-I4P1F5"
]



Test[
	xAct`xTras`Private`PartitionedSymmetrize[S2@@#1 T1@@#2 S3@@#3 &,{a,b,c,d,e,f},{2,1,3}]//Expand
	,
	S2[a,b]T1[c]S3[d,e,f]//Symmetrize//ToCanonical
	,
	TestID->"xTensor-20140206-T8Q5S1"
]


Test[
	CD[a,b]@metric[-d,-e]
	,
	0
	,
	TestID->"xTensor-20140206-D5P2I7"
]

Test[
	CD[a,b,c]@metric[d,e]
	,
	0
	,
	TestID->"xTensor-20140206-H5G1R7"
]

Test[
	CD[a,b,c]@metric[-d,e]
	,
	0
	,
	TestID->"xTensor-20140206-S7F5X4"
]

Test[
	CD[a,b,c,d]@metric[f,-e]
	,
	0
	,
	TestID->"xTensor-20140206-I3A3P0"
]

Test[
	CD[a,b,c]@{metric[d,e],RicciCD[d,e]}
	,
	{0, CD[a, b, c][RicciCD[d, e]]}
	,
	TestID->"xTensor-20140206-H2Z3M3"
]

Test[
	CD[a][Scalar[RicciCD[a,b]RicciCD[-a,-b]]^2]//ScreenDollarIndices
	,
	2*Scalar[RicciCD[-a, -b]*RicciCD[a, b]]*(RicciCD[b, c]*CD[a][RicciCD[-b, -c]] + RicciCD[-b, -c]*CD[a][RicciCD[b, c]])
	,
	TestID->"xTensor-20140206-B6O1I2"
]

Test[
	(CD[a, b][Scalar[RicciCD[a, b] RicciCD[-a, -b]]^2] - (CD[a]@CD[b][Scalar[RicciCD[a, b] RicciCD[-a, -b]]^2] // Symmetrize[#, {a, b}] &)) // ExpandSymCovDs // ToCanonical
	,
	0
	,
	TestID->"xTensor-20140206-G4Y7H4"
]

Test[
	CD[a,b][-45 RicciScalarCD[]]
	,
	-45*CD[a, b][RicciScalarCD[]]
	,
	TestID->"xTensor-20140206-P8N8L4"
]

Test[
	CD[a,b][  RicciScalarCD[] +RicciScalarCD[]^2]-Symmetrize@CD[a]@CD[b][RicciScalarCD[]+RicciScalarCD[]^2]//ExpandSymCovDs//ToCanonical
	,
	0
	,
	TestID->"xTensor-20140206-Y7Q9E2"
]

Test[
	CD[a,b][RicciScalarCD[]RicciCD[c,d]]-Symmetrize[CD[a]@CD[b][RicciScalarCD[]RicciCD[c,d]],{a,b}]//ExpandSymCovDs//ToCanonical
	,
	0
	,
	TestID->"xTensor-20140206-C8T8P4"
]

Test[
	CD[a,b,e][  RicciScalarCD[]RicciCD[c,d]]-Symmetrize[CD[a]@CD[b]@CD[e][RicciScalarCD[]RicciCD[c,d]],{a,b,e}]//ExpandSymCovDs//ToCanonical
	,
	0
	,
	TestID->"xTensor-20140206-N0T9L4"
]

Test[
	CD[a]@T1[b]//SymmetrizeCovDs
	,
	CD[a]@T1[b]
	,
	TestID->"xTensor-20140206-F1D6R5"
]

Test[
	CD[b]@CD[a]@T1[c]//SymmetrizeCovDs//ExpandSymCovDs//SortCovDs//ToCanonical
	,
	CD[b]@CD[a]@T1[c]
	,
	TestID->"xTensor-20140206-X8M9G3"
]

Test[
	CD[a,b]@T1[c]//ExpandSymCovDs//SortCovDs//ToCanonical//SymmetrizeCovDs//ToCanonical
	,
	CD[a,b]@T1[c]
	,
	TestID->"xTensor-20140206-L6M0H2"
]

Test[
	CD[c]@CD[b]@CD[a]@T1[d]//SymmetrizeCovDs//ToCanonical//ExpandSymCovDs//SortCovDs//ToCanonical
	,
	CD[c]@CD[b]@CD[a]@T1[d]
	,
	TestID->"xTensor-20140206-C2M1E6"
]

Test[
	CD[d]@CD[c]@CD[b]@CD[a]@T1[e]//SymmetrizeCovDs//ToCanonical//ExpandSymCovDs//SortCovDs//ToCanonical
	,
	CD[d]@CD[c]@CD[b]@CD[a]@T1[e]
	,
	TestID->"xTensor-20140206-J2F2G6"
]


Test[
	CD[f]@CD[d]@CD[c]@CD[b]@CD[a]@T1[e]//SymmetrizeCovDs//ToCanonical//ExpandSymCovDs//SortCovDs//ToCanonical//RiemannYoungProject//ToCanonical
	,
	CD[f]@CD[d]@CD[c]@CD[b]@CD[a]@T1[e]
	,
	TestID->"xTensor-20140221-T7S1X9"
]


Test[
	CD[a,b,c]@RicciCD[f,g]//ExpandSymCovDs//Expand//SymmetrizeCovDs//ToCanonical
	,
	CD[a,b,c]@RicciCD[f,g]
	,
	TestID->"xTensor-20140206-P7E5Q2"
]

Test[
	CD[a,b,c,d]@RicciCD[f,g]//ExpandSymCovDs//Expand//SymmetrizeCovDs//ToCanonical
	,
	CD[a,b,c,d]@RicciCD[f,g]
	,
	TestID->"xTensor-20140206-K4J4X0"
]

Test[
	ToCanonical@SortCovDs@ExpandSymCovDs[# - SymmetrizeCovDs[#]]& @ CD[a, b]@CD[-a, -b]@RicciScalarCD[]
	,
	0
	,
	TestID->"xTensor-20140221-X0G2L5"
]



Test[
	VarD[T1[a],CD][T1[a]CD[-a,c]@T1[-c]]//CollectTensors//ScreenDollarIndices
	,
	2*CD[-a, -b][T1[b]]
	,
	TestID->"xTensor-20140206-H8D9B5"
]

Test[
	VarD[T1[a],CD][T1[a]CD[-a,c,d]@S2[-c,-d]]//CollectTensors//ScreenDollarIndices
	,
	CD[-a, -b, -c][S2[b, c]]
	,
	TestID->"xTensor-20140206-X1N3R8"
]

Test[
	VarD[S2[a,b],CD][T1[a]CD[-a,c,d]@S2[-c,-d]]//CollectTensors//ScreenDollarIndices
	,
	-CD[-a, -b, -c][T1[c]]
	,
	TestID->"xTensor-20140206-M0D7Z6"
]

Test[
	ChangeCovD[CD[a]@T1[b]]//ScreenDollarIndices
	,
	ChristoffelCD[b, a, -c]*T1[c] + PD[a][T1[b]]
	,
	TestID->"xTensor-20140206-P1I2L4"
]

Test[
	ChangeCovD[CD[a,c]@T1[b]]-ChangeCovD[1/2(CD[a]@CD[c]@T1[b] + CD[c]@CD[a]@T1[b])]//ToCanonical
	,
	0
	,
	TestID->"xTensor-20140206-T8W3T8"
]

Test[
	metric[-b,-c]CD[a,b]@RiemannCD[c,d,e,f]//ContractMetric//ToCanonical
	,
	-CD[a, -c][RiemannCD[d, c, e, f]]
	,
	TestID->"xTensor-20140206-U0W8U3"
]

Test[
	metric[-a,-c]CD[a,b]@RiemannCD[c,d,e,f]//ContractMetric//ToCanonical
	,
	-CD[b, -c][RiemannCD[d, c, e, f]]
	,
	TestID->"xTensor-20140206-Z3O1M9"
]

Test[
	metric[-a,-b]CD[a,b]@RiemannCD[c,d,e,f]//ContractMetric//ToCanonical
	,
	CD[a, -a][RiemannCD[c, d, e, f]]
	,
	TestID->"xTensor-20140206-S8O8Q6"
]

Test[
	metric[-d,-f]CD[a,b]@RiemannCD[c,d,e,f]//ContractMetric//ToCanonical
	,
	CD[a, b][RicciCD[c, e]]
	,
	TestID->"xTensor-20140206-Q5Y2Q2"
]

Test[
	ToCanonical @ ExpandPerturbation @ Perturbation[CD[d]@CD[a,b]@T1[c] - 1/2 CD[d][ CD[a]@CD[b]@T1[c] + CD[b]@CD[a]@T1[c] ] ]
	,
	0
	,
	TestID->"xTensor-20140206-J5E7S1"
]

Test[
	ToCanonical @ ExpandPerturbation @ Perturbation[CD[a,b]@RicciScalarCD[] - 1/2 (CD[a]@CD[b]@RicciScalarCD[] + CD[b]@CD[a]@RicciScalarCD[]) ]
	,
	0
	,
	TestID->"xTensor-20140206-M2P0X3"
]

Test[
	CD[-a,-b]@T4[c,d,e,f]//ToIndexFree//FromIndexFree
	,
	CD[-a,-b]@T4[c,d,e,f]
	,
	TestID->"xTensor-20140206-Z0V8Y4"
]