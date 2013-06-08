ComputeBasisValues::usage =
	"ComputeBasisValues[chart1,chart2] computes and stores the values of the \
basis elements relating chart1 to chart2 and vice versa. Thus it computes \
Basis[-chart1,chart2] and Basis[-chart2,chart1]. \
\n\nNote that ComputeBasisValues internally uses InChart, so it is preferable \
to define the transformations of the coordinates from chart1 to chart2 and vice versa \
with InChart before using ComputeBasisValues."; 

ImplodedTensorValues::usage =
	"ImplodedTensorValues[CD, T, basis] computes the values \
of the covariant derivative CD of the tensor T in the given basis, and stores the values \
in the imploded tensor CDT. Both CD and T do not take indices."


Begin["`Private`"]

(********************)
(* xCoba extensions *)
(********************)

ComputeBasisValues[B1_?ChartQ,B2_?ChartQ] := ComputeBasisValues1@@#& /@ {{B1,B2},{B2,B1}};

ComputeBasisValues1[B1_?ChartQ,B2_?ChartQ] := Module[
	{
		C1,C2,basisArray,values
	},
	values = Outer[
		Simplify@D[#1,#2]&,
		InChart[B2]@ScalarsOfChart@B1,
		ScalarsOfChart@B2
	];
	{C1,C2} 	= CNumbersOf[#,VBundleOfBasis@#]&/@{B1,B2};
	basisArray	= Outer[Basis[{#1,B1},{#2,-B2}]&,C1,C2];
	ComponentValue[basisArray,values]
];


ImplodedTensorValues[cd_?CovDQ, T_?xTensorQ, B_?BasisQ, f_:Identity] := Module[
	{
		cdT,valueArray,implodedArray
	},
	(* Construct the expression with derivative and indices. *)
	cdT = cd[DownIndex@DummyIn@First@VBundlesOfCovD@cd]@Apply[T,DummyIn/@SlotsOfTensor[T]];
	(* Implode cdT, go to the basis, and give a component list. *)
	implodedArray = cdT//Implode//ToBasis[B]//ComponentArray;
	valueArray = f@ToValues[
		(* Construct the component array of the unimploded cdT. *)
		(* Note that we once use FreeToBasis for the free indices and once  
		   DummyToBasis for the contraction with the Christoffels. *)
		(* Doing the ChangeCovD is not strictly necessary, but it is 'more correct' to change to the PD of the basis. *) 
		cdT // FreeToBasis[B] // DummyToBasis[B] // ChangeCovD[#, cd, PDOfBasis@B]& // TraceBasisDummy // ComponentArray,
		(* Get a list of all the tensors in the problem, which are just the tensor T and the relevant Christoffel *)
		{T, GiveSymbol[Christoffel,cd,PDOfBasis@B]}
	];
	(* Assign values and return. *)
	(* Note that this is the main bottleneck for large tensors, because ComponentValue
	   first checks for dependencies and modifies the TensorValues list for each component.
	   It would be much faster to replace the TensorValues list in one go. *)
	ComponentValue[implodedArray, valueArray]
];

(*
ToBasis[basis_][expr_] /; Apply[And, AIndexQ /@ IndicesOf[][expr]] :=
	ChangeCovD[
		expr,
		$CovDs,
		PDOfBasis[basis]
	] /. i_?AbstractIndexQ :> {i, basis}
*)

End[]