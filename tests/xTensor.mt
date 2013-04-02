(****************)
(*              *)
(*    Setup     *)
(*              *)
(****************)

$CommuteCovDsOnScalars = True;
SetOptions[ToCanonical, UseMetricOnVBundle -> All];
SetOptions[ContractMetric, AllowUpperDerivatives -> True];

DefManifold[
	M,
	dim,
	IndexRange[a,l]
];

DefMetric[
	-1,
	metric[-a,-b],
	CD,
	PrintAs -> "g",
	CurvatureRelations -> True
];

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
	TestID->"xTensor-20130102-C1X1X5"
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
(*  Implode / explode indices  *)
(*                             *)
(*******************************)

Test[
	ExplodeIndices[ RiemannCD ]
	,
	RiemannCD[-a,-b,-c,-d]
	,
	TestID->"xTensor-20130402-W2F7U9"
]


Test[
	ExplodeIndices[ CD@CD@RicciCD ]
	,
	CD[-a]@CD[-b]@RicciCD[-c,-d]
	,
	TestID->"xTensor-20130402-H4V1O5"
]

Test[
	ExplodeIndices[ CD[-a]@CD[-b]@RicciCD[-c,-d] ]
	,
	CD[-a]@CD[-b]@RicciCD[-c,-d]
	,
	TestID->"xTensor-20130402-A3J6N8"
]

Test[
	ImplodeIndices[ CD[-a]@CD[-b]@RicciCD[-c,-d] ]
	,
	CD@CD@RicciCD
	,
	TestID->"xTensor-20130402-D1Z2D6"
]


Test[
	ImplodeIndices[ CD@CD@RicciCD ]
	,
	CD@CD@RicciCD
	,
	TestID->"xTensor-20130402-C8K9Z4"
]


(*************************)
(*                       *)
(*  Curvature relations  *)
(*                       *)
(*************************)

Test[
	CurvatureRelationsQ[CD]
	,
	True
	,
	TestID->"xTensor-20130102-O4A0D6"
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
	CD[a]@RiemannCD[-a, -b, -c, -d] /. CurvatureRelationsBianchi[CD]
	,
	CD[-c][RicciCD[-b, -d]] - CD[-d][RicciCD[-b, -c]]
	,
	TestID->"Invar-20130102-V9L4H6"
]

Test[
	CD[b]@RiemannCD[-a, -b, -c, -d] /. CurvatureRelationsBianchi[CD]
	,
	-CD[-c][RicciCD[-a, -d]] + CD[-d][RicciCD[-a, -c]]
	,
	TestID->"Invar-20130102-Z5B4W8"
]

Test[
	CD[c]@RiemannCD[-a, -b, -c, -d] /. CurvatureRelationsBianchi[CD]
	,
	CD[-a][RicciCD[-d, -b]] - CD[-b][RicciCD[-d, -a]]
	,
	TestID->"Invar-20130102-Z7Y9G1"
]

Test[
	CD[d]@RiemannCD[-a, -b, -c, -d] /. CurvatureRelationsBianchi[CD]
	,
	-CD[-a][RicciCD[-c, -b]] + CD[-b][RicciCD[-c, -a]]
	,
	TestID->"Invar-20130102-N2G6L7"
]

(*****************************)
(*                           *)
(*  Extra curvature tensors  *)
(*                           *)
(*****************************)

Test[
	xTensorQ /@ {SchoutenCD, EinsteinCCCD, SchoutenCCCD}
	,
	{True, True, True}
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