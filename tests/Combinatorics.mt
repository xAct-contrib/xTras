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

Test[
	AllContractions[ IndexFree[RiemannCD^3] ]
	,
	{
	 RicciCD[-a, c]*RicciCD[a, b]*RicciCD[-b, -c], RicciCD[-a, -b]*RicciCD[a, b]*RicciScalarCD[], 
	 RicciScalarCD[]^3, RicciCD[a, b]*RicciCD[c, d]*RiemannCD[-a, -c, -b, -d], 
	 RicciScalarCD[]*RiemannCD[-a, -b, -c, -d]*RiemannCD[a, b, c, d], 
	 RicciScalarCD[]*RiemannCD[-a, -c, -b, -d]*RiemannCD[a, b, c, d], 
	 RicciCD[a, b]*RiemannCD[-a, c, d, e]*RiemannCD[-b, -c, -d, -e], 
	 RicciCD[a, b]*RiemannCD[-a, c, d, e]*RiemannCD[-b, -d, -c, -e], 
	 RiemannCD[-a, -c, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, -e, -d, -f], 
	 RiemannCD[-a, e, -c, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, -e, -d, -f], 
	 RiemannCD[-a, e, -c, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, -f, -d, -e], 
	 RiemannCD[-a, -b, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-c, -d, -e, -f], 
	 RiemannCD[-a, -b, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-c, -e, -d, -f]
	}
	,
	TestID->"Combinatorics-20140602-I3E3Q6"
]

(* This test also takes pretty long. *)
(*
Test[
	AllContractions[ IndexFree[RiemannCD^4] ]
	,
	{
	 RicciCD[-a, c]*RicciCD[a, b]*RicciCD[-b, d]*RicciCD[-c, -d], 
	 RicciCD[-a, -b]*RicciCD[a, b]*RicciCD[-c, -d]*RicciCD[c, d], 
	 RicciCD[-a, c]*RicciCD[a, b]*RicciCD[-b, -c]*RicciScalarCD[], 
	 RicciCD[-a, -b]*RicciCD[a, b]*RicciScalarCD[]^2, RicciScalarCD[]^4, 
	 RicciCD[a, b]*RicciCD[c, d]*RicciScalarCD[]*RiemannCD[-a, -c, -b, -d], 
	 RicciScalarCD[]^2*RiemannCD[-a, -b, -c, -d]*RiemannCD[a, b, c, d], 
	 RicciScalarCD[]^2*RiemannCD[-a, -c, -b, -d]*RiemannCD[a, b, c, d], 
	 RicciCD[a, b]*RicciScalarCD[]*RiemannCD[-a, c, d, e]*RiemannCD[-b, -c, -d, -e], 
	 RicciCD[-a, c]*RicciCD[a, b]*RicciCD[d, e]*RiemannCD[-b, -d, -c, -e], 
	 RicciCD[a, b]*RicciScalarCD[]*RiemannCD[-a, c, d, e]*RiemannCD[-b, -d, -c, -e], 
	 RicciCD[a, b]*RicciCD[c, d]*RiemannCD[-a, -c, e, f]*RiemannCD[-b, -d, -e, -f], 
	 RicciCD[a, b]*RicciCD[c, d]*RiemannCD[-a, -c, e, f]*RiemannCD[-b, -e, -d, -f], 
	 RicciCD[a, b]*RicciCD[c, d]*RiemannCD[-a, e, -c, f]*RiemannCD[-b, -e, -d, -f], 
	 RicciScalarCD[]*RiemannCD[-a, -c, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, -e, -d, -f], 
	 RicciScalarCD[]*RiemannCD[-a, e, -c, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, -e, -d, -f], 
	 RicciCD[a, b]*RicciCD[c, d]*RiemannCD[-a, e, -c, f]*RiemannCD[-b, -f, -d, -e], 
	 RicciScalarCD[]*RiemannCD[-a, e, -c, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, -f, -d, -e], 
	 RicciScalarCD[]*RiemannCD[-a, -b, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-c, -d, -e, -f], 
	 RicciCD[-a, c]*RicciCD[a, b]*RiemannCD[-b, d, e, f]*RiemannCD[-c, -d, -e, -f], 
	 RicciCD[a, b]*RicciCD[c, d]*RiemannCD[-a, e, -b, f]*RiemannCD[-c, -e, -d, -f], 
	 RicciScalarCD[]*RiemannCD[-a, -b, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-c, -e, -d, -f], 
	 RicciCD[-a, c]*RicciCD[a, b]*RiemannCD[-b, d, e, f]*RiemannCD[-c, -e, -d, -f], 
	 RicciCD[a, b]*RiemannCD[-a, c, d, e]*RiemannCD[-b, -d, f, g]*RiemannCD[-c, -e, -f, -g], 
	 RicciCD[a, b]*RiemannCD[-a, c, d, e]*RiemannCD[-b, f, -d, g]*RiemannCD[-c, -e, -f, -g], 
	 RicciCD[a, b]*RiemannCD[-a, c, d, e]*RiemannCD[-b, -d, f, g]*RiemannCD[-c, -f, -e, -g], 
	 RicciCD[a, b]*RiemannCD[-a, c, d, e]*RiemannCD[-b, f, -d, g]*RiemannCD[-c, -f, -e, -g], 
	 RicciCD[a, b]*RiemannCD[-a, c, d, e]*RiemannCD[-b, f, -d, g]*RiemannCD[-c, -g, -e, -f], 
	 RicciCD[-a, -b]*RicciCD[a, b]*RiemannCD[-c, -d, -e, -f]*RiemannCD[c, d, e, f], 
	 RicciCD[-a, -b]*RicciCD[a, b]*RiemannCD[-c, -e, -d, -f]*RiemannCD[c, d, e, f], 
	 RicciCD[a, b]*RiemannCD[-a, c, d, e]*RiemannCD[-b, -c, f, g]*RiemannCD[-d, -e, -f, -g], 
	 RicciCD[a, b]*RiemannCD[-a, c, -b, d]*RiemannCD[-c, e, f, g]*RiemannCD[-d, -e, -f, -g], 
	 RicciCD[a, b]*RiemannCD[-a, c, d, e]*RiemannCD[-b, -c, f, g]*RiemannCD[-d, -f, -e, -g], 
	 RicciCD[a, b]*RiemannCD[-a, c, -b, d]*RiemannCD[-c, e, f, g]*RiemannCD[-d, -f, -e, -g], 
	 RiemannCD[-a, -b, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-c, -e, g, h]*RiemannCD[-d, -f, -g, -h], 
	 RiemannCD[-a, -c, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, -e, g, h]*RiemannCD[-d, -g, -f, -h], 
	 RiemannCD[-a, -c, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, g, -e, h]*RiemannCD[-d, -g, -f, -h], 
	 RiemannCD[-a, e, -c, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, g, -e, h]*RiemannCD[-d, -g, -f, -h], 
	 RiemannCD[-a, -b, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-c, -e, g, h]*RiemannCD[-d, -g, -f, -h], 
	 RiemannCD[-a, -b, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-c, g, -e, h]*RiemannCD[-d, -g, -f, -h], 
	 RiemannCD[-a, e, -c, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, g, -f, h]*RiemannCD[-d, -h, -e, -g], 
	 RiemannCD[-a, -c, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, g, -e, h]*RiemannCD[-d, -h, -f, -g], 
	 RiemannCD[-a, e, -c, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, g, -e, h]*RiemannCD[-d, -h, -f, -g], 
	 RiemannCD[-a, -b, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-c, g, -e, h]*RiemannCD[-d, -h, -f, -g], 
	 RiemannCD[-a, -b, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-c, -d, g, h]*RiemannCD[-e, -f, -g, -h], 
	 RiemannCD[-a, -b, -c, e]*RiemannCD[a, b, c, d]*RiemannCD[-d, f, g, h]*RiemannCD[-e, -f, -g, -h], 
	 RiemannCD[-a, -c, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, -d, g, h]*RiemannCD[-e, -g, -f, -h], 
	 RiemannCD[-a, -c, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, g, -d, h]*RiemannCD[-e, -g, -f, -h], 
	 RiemannCD[-a, e, -c, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, g, -d, h]*RiemannCD[-e, -g, -f, -h], 
	 RiemannCD[-a, -b, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-c, -d, g, h]*RiemannCD[-e, -g, -f, -h], 
	 RiemannCD[-a, -b, e, f]*RiemannCD[a, b, c, d]*RiemannCD[-c, g, -d, h]*RiemannCD[-e, -g, -f, -h], 
	 RiemannCD[-a, -b, -c, e]*RiemannCD[a, b, c, d]*RiemannCD[-d, f, g, h]*RiemannCD[-e, -g, -f, -h], 
	 RiemannCD[-a, -c, -b, e]*RiemannCD[a, b, c, d]*RiemannCD[-d, f, g, h]*RiemannCD[-e, -g, -f, -h], 
	 RiemannCD[-a, e, -c, f]*RiemannCD[a, b, c, d]*RiemannCD[-b, g, -d, h]*RiemannCD[-e, -h, -f, -g], 
	 RiemannCD[-a, -b, -c, -d]*RiemannCD[a, b, c, d]*RiemannCD[-e, -f, -g, -h]*RiemannCD[e, f, g, h], 
	 RiemannCD[-a, -b, -c, -d]*RiemannCD[a, b, c, d]*RiemannCD[-e, -g, -f, -h]*RiemannCD[e, f, g, h], 
	 RiemannCD[-a, -c, -b, -d]*RiemannCD[a, b, c, d]*RiemannCD[-e, -g, -f, -h]*RiemannCD[e, f, g, h]
	}
	,
	TestID->"Combinatorics-20140602-U6Y7G0"
]
*)


(***************************)
(*                         *)
(*      ConstructDDIs      *)
(*                         *)
(***************************)


dim = 3;

Test[
	ConstructDDIs[
		RiemannCD[a, b, c, d], 
		{a, b, c, d}, 
		RiemannSymmetric[{a, b, c, d}]
	] //  RiemannToWeyl // CollectTensors // RiemannYoungProject // CollectTensors // xAct`xTras`Private`DeleteDuplicateFactors
	,
	{WeylCD[a, b, c, d] + WeylCD[a, c, b, d]/2 - WeylCD[a, d, b, c]/2}
	,
	TestID->"Combinatorics-20140224-X1V4E8"
]

dim = 2;

Test[
	ConstructDDIs[
		RiemannCD[a, b, c, d], 
		{a, b}, 
		Symmetric[{a, b}]
	] // RicciToEinstein
	,
	{EinsteinCD[a, b]}
	,
	TestID->"Combinatorics-20140224-R6L4K1"
]

dim =. ;

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