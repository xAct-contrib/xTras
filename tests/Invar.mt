(*******************)
(*                 *)
(*  Euler density  *)
(*                 *)
(*******************)

Test[
	ToCanonical[
		NoScalar@EulerDensity[CD,4]
		- (4*RicciCD[-a, -b]*RicciCD[a, b] - RicciScalarCD[]^2 - 
 		  RiemannCD[-a, -b, -c, -d]*RiemannCD[a, b, c, d] )
	]
	,
	0
	,
	TestID->"Invar-20130102-C9X8J2"
]

(*******************************************)
(*                                         *)
(*   InvarWrapper & RiemannSimplification  *)
(*                                         *)
(*******************************************)

(* Using RiemannSimplify without the InvarWrapper returns Cycles[] for this case. *)
Test[
	InvarWrapper[RiemannSimplify[#2, 4, True, #1] &, metric][ CD[a]@CD[-a]@RicciScalarCD[] ]
	,
	Scalar[ CD[-a]@CD[a]@RicciScalarCD[] ]
	,
	TestID->"Invar-20130102-Y7B5N2"
]

Test[
	InvarWrapper[RiemannSimplify[#2, 4, True, #1] &, metric][ CD[a]@CD[b]@RicciCD[-a, -b] ]
	,
	1/2 Scalar[ CD[-a]@CD[a]@RicciScalarCD[] ]
	,
	TestID->"Invar-20130102-S3V7F7"
]

Test[
	ScreenDollarIndices@RiemannSimplification[][ CD[a]@CD[-a]@RicciScalarCD[] ]
	,
	CD[-a]@CD[a]@RicciScalarCD[]
	,
	TestID->"Invar-20130102-M1G8F3"
]

Test[
	ToCanonical@RiemannSimplification[][
		2 
		+ RiemannCD[a,b,c,d] RiemannCD[-a,-c,-b,-d] 
		- RiemannCD[a,b,c,d] RiemannCD[-a,-b,-c,-d]/2 
	]
	,
	2
	,
	TestID->"Invar-20130102-P6X6I8"
]

Test[
	ScreenDollarIndices@RiemannSimplification[][metric[e,f]RiemannCD[a,b,c,d] RiemannCD[-a,-c,-b,-d]+2RicciCD[e,f]]
	,
	2 RicciCD[e, f] + 1/2 metric[e, f]*RiemannCD[-a, -b, -c, -d]*RiemannCD[a, b, c, d]
	,
	TestID->"Invar-20130102-G6C0O4"
]

(*******************************************)
(*                                         *)
(*          FullSimplification             *)
(*                                         *)
(*******************************************)

Test[
	ScreenDollarIndices@FullSimplification[][CD[-a]@CD[-b]@RicciCD[-c,-d]]
	,
	RicciCD[-d, e]*RiemannCD[-a, -b, -c, -e] + 
 		RicciCD[-c, e]*RiemannCD[-a, -b, -d, -e] + 
 		CD[-b][CD[-a][RicciCD[-c, -d]]]
	,
	TestID->"Invar-20130102-T0T6F9"
]

Test[
	ScreenDollarIndices@FullSimplification[metric][CD[a]@CD[-c]@RicciCD[-a,-b]]
	,
	RicciCD[-b, a]*RicciCD[-c, -a] - RicciCD[a, d]*RiemannCD[-b, -a, -c, -d] + CD[-c][CD[-b][RicciScalarCD[]]]/2
	,
	TestID->"Invar-20130126-F0L3J9"
]

(*******************************************)
(*                                         *)
(*        Invar database access            *)
(*                                         *)
(*******************************************)


Test[
	SingleInvariants[metric, {0}]
	,
	{1}
	,
	TestID->"Invar-20130102-G1C2W7"
]

Test[
	SingleInvariants[metric, {2}]
	,
	{RicciScalarCD[]}
	,
	TestID->"Invar-20130102-S9H9Y5"
]

Test[
	Sort[ScreenDollarIndices@ContractMetric@NoScalar[#] & /@ SingleInvariants[metric, {4}]]
	,
	{
		RicciCD[-a, -b]*RicciCD[a, b], 
		RiemannCD[-a, -b, -c, -d]*RiemannCD[a, b, c, d], 
		CD[-a][CD[a][RicciScalarCD[]]]
	}
	,
	TestID->"Invar-20130102-D0S4J9"
]

Test[
	Sort[ScreenDollarIndices@ContractMetric@NoScalar[#] & /@ ProductInvariants[metric, {4}]]
	,
	{
		RicciCD[-a, -b]*RicciCD[a, b], 
		RicciScalarCD[]^2, 
 		RiemannCD[-a, -b, -c, -d]*RiemannCD[a, b, c, d], 
 		CD[-a][CD[a][RicciScalarCD[]]]
 	}
	,
	TestID->"Invar-20130102-U2K6U3"
]

Test[
	Length@ProductInvariants[metric, {6}]
	,
	17
	,
	TestID->"Invar-20130102-J6E2P1"
]

Test[
	Length@ProductInvariants[metric, {8}]
	,
	92
	,
	TestID->"Invar-20130102-O5H4M8"
]