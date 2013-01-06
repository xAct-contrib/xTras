(***************************)
(*                         *)
(*     Young projectors    *)
(*                         *)
(***************************)

Test[
	Antisymmetrize[
		RiemannCD[-a,-b,-c,-d],
		{-a,-b,-c}
	] // ToCanonical // RiemannYoungProject// ToCanonical
	,
	0
	,
	TestID->"Combinatorics-20130106-I3K0K8"
]

Test[
	Antisymmetrize[
		CD[-a]@RiemannCD[-b,-c,-d,-e],
		{-a,-b,-c}
	] //ToCanonical // RiemannYoungProject[#,1]& // ToCanonical
	,
	0
	,
	TestID->"Combinatorics-20130106-O0D1Q2"
]

(***************************)
(*                         *)
(*     AllContractions     *)
(*                         *)
(***************************)

Test[
	xAct`xTras`Combinatorics`Private`NextDummyPermutations[{1, 2, 3, 4}, {3, 4}, {}]
	,
	{{3,4,1,2},{3,1,4,2},{3,1,2,4},{1,3,4,2},{1,3,2,4},{1,2,3,4}}
	,
	TestID->"Combinatorics-20130104-V8M7C2"
]

Test[
	xAct`xTras`Combinatorics`Private`NextDummyPermutations[{1, 3, 2, 4}, {1, 2}, {3, 4}]
	,
	{{1, 3, 2, 4}}
	,
	TestID->"Combinatorics-20130104-O0V6Z4"
]

Test[
	xAct`xTras`Combinatorics`Private`NextDummyPermutations[{1, 2, 5, 3, 6, 4}, {3, 4}, {5, 6}]
	,
	{{3, 4, 5, 1, 6, 2}, {3, 1, 5, 4, 6, 2}, {3, 1, 5, 2, 6, 4}, {1, 3, 5, 4, 6, 2}, {1, 3, 5, 2, 6, 4}, {1, 2, 5, 3, 6, 4}}
	,
	TestID->"Combinatorics-20130104-F4H4M5"
]

Test[
	xAct`xTras`Combinatorics`Private`NextDummyPermutations[{1, 3, 5, 2, 6, 4}, {1, 2}, {3, 4, 5, 6}]
	,
	{{1, 3, 5, 2, 6, 4}}
	,
	TestID->"Combinatorics-20130104-B4Y0S1"
]

Test[
	AllContractions[ RiemannCD[a,b,c,d] ]
	,
	{RicciScalarCD[]}
	,
	TestID->"Combinatorics-20130104-Z7U3B3"
]

Test[
	AllContractions[ RiemannCD[a,b,c,d] RiemannCD[e,f,g,h] ]
	,
	{
		RicciCD[-a, -b]*RicciCD[a, b], 
		RicciScalarCD[]^2, 
		RiemannCD[-a, -b, -c, -d]*RiemannCD[a, b, c, d], 
		RiemannCD[-a, -c, -b, -d]*RiemannCD[a, b, c, d]
	}
	,
	TestID->"Combinatorics-20130104-H3H0E5"
]

Test[
	AllContractions[ RiemannCD[a,b,c,d], IndexList[a,b], Symmetric[{a,b}] ]
	,
	{ RicciCD[a,b], metric[a,b] RicciScalarCD[] }
	,
	TestID->"Combinatorics-20130104-L9M2D7"
]

Test[
	AllContractions[ RiemannCD[a,b,c,d], UncontractedPairs -> 1 ]
	,
	{ RicciCD[a,b] }
	,
	TestID->"Combinatorics-20130104-X6T4E6"
]

Test[
	AllContractions[ RiemannCD[a,b,c,d], ContractedPairs -> 1 ]
	,
	{ RicciCD[a,b] }
	,
	TestID->"Combinatorics-20130104-A0G2Q0"
]


(***************************)
(*                         *)
(*   IndexConfigurations   *)
(*                         *)
(***************************)


Test[
	IndexConfigurations[RiemannCD[-a, -b, -c, -d]]
	,
	{RiemannCD[-a, -b, -c, -d], RiemannCD[-a, -c, -b, -d], RiemannCD[-a, -d, -b, -c]}
	,
	TestID->"Combinatorics-20130104-I5G3I3"
]

Test[
	IndexConfigurations[metric[a, b]]
	,
	{metric[a,b]}
	,
	TestID->"Combinatorics-20130104-F4R8W8"
]

Test[
	IndexConfigurations[metric[a, b] metric[c, d]]
	,
	{
		metric[a, d] metric[b, c], 
		metric[a, c] metric[b, d], 
		metric[a, b] metric[c, d]
	}
	,
	TestID->"Combinatorics-20130104-Y5K0B7"
]