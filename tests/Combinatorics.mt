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
		RiemannCD[-e,-f,-g,-h],
		{-e,-g,-h}
	] // ToCanonical // RiemannYoungProject// ToCanonical
	,
	0
	,
	TestID->"xPert-20130124-Y2N0D7"
]

Test[
	ToCanonical[# - RiemannYoungProject[#]]& @ RiemannYoungProject[ RiemannCD[-e,-f,-g,-h] ]
	,
	0
	,
	TestID->"Combinatorics-20140207-M1Y6Y7"
]

Test[
	Antisymmetrize[
		CD[-a]@RiemannCD[-b,-c,-d,-e],
		{-a,-b,-c}
	] //ToCanonical // RiemannYoungProject // ToCanonical
	,
	0
	,
	TestID->"Combinatorics-20130106-O0D1Q2"
]

Test[
	ToCanonical[# - RiemannYoungProject[#]]& @ RiemannYoungProject[ CD[-a]@RiemannCD[-b,-c,-d,-e] ]
	,
	0
	,
	TestID->"Combinatorics-20140210-T1B5Z6"
]

Test[
	Antisymmetrize[
		WeylCD[-a,-b,-c,-d],
		{-a,-b,-c}
	] // ToCanonical // RiemannYoungProject// ToCanonical
	,
	0
	,
	TestID->"Combinatorics-20140207-M5K1K5"
]

Test[
	ToCanonical[# - RiemannYoungProject[#]]& @ RiemannYoungProject[ WeylCD[-a,-b,-c,-d] ]
	,
	0
	,
	TestID->"Combinatorics-20140210-S4U4S1"
]

Test[
	Antisymmetrize[
		CD[-a]@WeylCD[-b,-c,-d,-e],
		{-a,-b,-c}
	] //ToCanonical // RiemannYoungProject // ToCanonical
	,
	0
	,
	TestID->"Combinatorics-20140207-T3N3Q5"
]

Test[
	ToCanonical[# - RiemannYoungProject[#]]& @ RiemannYoungProject[ CD[-a]@WeylCD[-b,-c,-d,-e] ]
	,
	0
	,
	TestID->"Combinatorics-20140210-L5Y2F1"
]


Test[
	Symmetrize[
		SymRiemannCD[-a,-b,-c,-d],
		{-a,-b,-c}
	] // ToCanonical // RiemannYoungProject// ToCanonical
	,
	0
	,
	TestID->"Combinatorics-20140207-L7M2H2"
]

Test[
	ToCanonical[# - RiemannYoungProject[#]]& @ RiemannYoungProject[ SymRiemannCD[-a,-b,-c,-d] ]
	,
	0
	,
	TestID->"Combinatorics-20140210-U9G6C3"
]


Test[
	Symmetrize[
		CD[-a]@SymRiemannCD[-b,-c,-d,-e],
		{-a,-b,-c,-d}
	] //ToCanonical // RiemannYoungProject // ToCanonical
	,
	0
	,
	TestID->"Combinatorics-20140207-E3Z6Q7"
]

Test[
	Symmetrize[
		CD[-a]@SymRiemannCD[-b,-c,-d,-e],
		{-a,-c,-d,-e}
	] //ToCanonical // RiemannYoungProject // ToCanonical
	,
	0
	,
	TestID->"Combinatorics-20140210-G1H0G4"
]

Test[
	ToCanonical[# - RiemannYoungProject[#]]& @ RiemannYoungProject[ CD[-e]@SymRiemannCD[-a,-b,-c,-d] ]
	,
	0
	,
	TestID->"Combinatorics-20140210-K3W9M1"
]

Test[
	CD[a]@RiemannCD[b,c,d,e]//RiemannToSymRiemann//ToCanonical//Antisymmetrize[#,{a,b,c}]&//ToCanonical//RiemannYoungProject//ToCanonical
	,
	0
	,
	TestID->"Combinatorics-20140210-G8C6D2"
]

Test[
	CD[a]@RiemannCD[b,c,d,e]//RiemannToSymRiemann//ToCanonical//Antisymmetrize[#,{a,b,d}]&//ToCanonical//RiemannYoungProject//ToCanonical
	,
	0
	,
	TestID->"Combinatorics-20140210-U5J4J2"
]

Test[
	RiemannCD[b,c,d,e]//RiemannToSymRiemann//ToCanonical//Antisymmetrize[#,{b,c,d}]&//ToCanonical//RiemannYoungProject//ToCanonical
	,
	0
	,
	TestID->"Combinatorics-20140210-I8Y0O8"
]

(***************************)
(*                         *)
(*      MakeTraceless      *)
(*                         *)
(***************************)


Test[
	WeylCD[-b,-e,-d,-a] - ExpandSym@MakeTraceless[
		RiemannCD[-b,-e,-d,-a]
	] // RiemannToWeyl // Simplification
	,
	0
	,
	TestID->"Combinatorics-20130402-R1X2F7"
]


Test[
	TFRicciCD[-d,-e] - ExpandSym@MakeTraceless[
		RicciCD[-d,-e]
	] // RicciToTFRicci // Simplification
	,
	0
	,
	TestID->"Combinatorics-20130402-Y1I3X5"
]

(***************************)
(*                         *)
(*     AllContractions     *)
(*                         *)
(***************************)

Test[
	xAct`xTras`Private`NextDummyPermutations[{1, 2, 3, 4}, {3, 4}, {}]
	,
	{{3,4,1,2},{3,1,4,2},{3,1,2,4},{1,3,4,2},{1,3,2,4},{1,2,3,4}}
	,
	TestID->"Combinatorics-20130104-V8M7C2"
]

Test[
	xAct`xTras`Private`NextDummyPermutations[{1, 3, 2, 4}, {1, 2}, {3, 4}]
	,
	{{1, 3, 2, 4}}
	,
	TestID->"Combinatorics-20130104-O0V6Z4"
]

Test[
	xAct`xTras`Private`NextDummyPermutations[{1, 2, 5, 3, 6, 4}, {3, 4}, {5, 6}]
	,
	{{3, 4, 5, 1, 6, 2}, {3, 1, 5, 4, 6, 2}, {3, 1, 5, 2, 6, 4}, {1, 3, 5, 4, 6, 2}, {1, 3, 5, 2, 6, 4}, {1, 2, 5, 3, 6, 4}}
	,
	TestID->"Combinatorics-20130104-F4H4M5"
]

Test[
	xAct`xTras`Private`NextDummyPermutations[{1, 3, 5, 2, 6, 4}, {1, 2}, {3, 4, 5, 6}]
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
	AllContractions[ RiemannCD[a,b,c,d], UncontractedIndices -> 2 ]
	,
	{ RicciCD[a,b] }
	,
	TestID->"Combinatorics-20130104-X6T4E6"
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


(***************************)
(*                         *)
(*     TableauSymmetric    *)
(*                         *)
(***************************)

Test[
	TableauSymmetric[{{1}, {2}}, ManifestSymmetry -> Antisymmetric]
	,
	StrongGenSet[{1, 2}, GenSet[-Cycles[{1, 2}]]]
	,
	TestID->"Combinatorics-20130516-Y8P7U0"
]

Test[
	TableauSymmetric[{{1}, {2}}, ManifestSymmetry -> Symmetric]
	,
	StrongGenSet[{1, 2}, GenSet[-Cycles[{1,2}]]]
	,
	TestID->"Combinatorics-20130516-E5B0W4"
]

Test[
	TableauSymmetric[{{1,2}}, ManifestSymmetry -> Antisymmetric]
	,
	StrongGenSet[{1, 2}, GenSet[Cycles[{1, 2}]]]
	,
	TestID->"Combinatorics-20130517-X0F6A1"
]

Test[
	TableauSymmetric[{{1,2}}, ManifestSymmetry -> Symmetric]
	,
	StrongGenSet[{1, 2}, GenSet[Cycles[{1,2}]]]
	,
	TestID->"Combinatorics-20130517-K7C4F6"
]

Test[
	TableauSymmetric[{{1, 2}, {3, 4}}, ManifestSymmetry -> Antisymmetric]
	,
	StrongGenSet[{1, 2, 3, 4}, GenSet[-Cycles[{1, 3}], -Cycles[{2, 4}], Cycles[{1, 2}, {3, 4}]]]
	,
	TestID->"Combinatorics-20130516-F2U6Y7"
]

Test[
	TableauSymmetric[{{1, 2}, {3, 4}}, ManifestSymmetry -> Symmetric]
	,
	StrongGenSet[{1, 2, 3, 4}, GenSet[Cycles[{1, 2}], Cycles[{3, 4}], Cycles[{1, 3}, {2, 4}]]]
	,
	TestID->"Combinatorics-20130516-D6L5U5"
]

Test[
	TableauSymmetric[{{1, 2, 3}, {4}}, ManifestSymmetry -> Antisymmetric]
	,
	StrongGenSet[{1, 2, 3, 4}, GenSet[-Cycles[{1, 4}], Cycles[{2, 3}]]]
	,
	TestID->"Combinatorics-20130516-O5K0I3"
]


Test[
	TableauSymmetric[{{1, 2}, {3, 4}, {5}}, ManifestSymmetry -> Symmetric]
	,
	StrongGenSet[{1, 2, 3, 4, 5}, GenSet[Cycles[{1, 2}], Cycles[{3, 4}], Cycles[{1, 3}, {2, 4}]]]
	,
	TestID->"Combinatorics-20130516-H4X5I5"
]

Test[
	TableauSymmetric[{{1, 2}, {3, 4}, {5}}, ManifestSymmetry -> Antisymmetric]
	,
	StrongGenSet[{1, 2, 3, 4, 5}, GenSet[-Cycles[{1, 3}], -Cycles[{3, 5}], -Cycles[{2, 4}]]]
	,
	TestID->"Combinatorics-20130516-U0L0M6"
]

Test[
	TableauSymmetric[{{1, 2, 5}, {3, 4}}, ManifestSymmetry -> Antisymmetric]
	,
	StrongGenSet[{1, 2, 3, 4, 5}, GenSet[-Cycles[{1, 3}], -Cycles[{2, 4}], Cycles[{1, 2}, {3, 4}]]]
	,
	TestID->"Combinatorics-20130516-P6E3D3"
]