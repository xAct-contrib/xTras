BeginPackage["xAct`xTras`Combinatorics`", {
	"xAct`xCore`",
	"xAct`xPerm`",
	"xAct`xTensor`",
	"xAct`SymManipulator`",
	"xAct`xTras`xCore`",
	"xAct`xTras`Algebra`"
}]

(* MetricPermutations *)

SymmetrizeMethod::usage =
	"SymmetrizeMethod is an option for AllContractions. Its values can be \
'ImposeSymmetry' (default) or 'ImposeSym'. The former uses xTensor's explicit \
symmetrization to symmetrize the free indices, whereas the latter uses \
SymManipulator's implicit ImposeSym to symmetrize the free indices.";

UncontractedPairs::usage = 
	"UncontractedPairs is an option for AllContractions which specifies how \
many index pairs should not be contracted. The default is None, which amounts to \
contracting all indices. It can also be an integer in the range from 0 to half the number \
total indices. \n\
When UncontractedPairs and ContractedPairs have conflicting values, UncontractedPairs \
takes precendence. \n\
Note that when not all index pairs are contracted, AllContractions returns a \
list with one element for each unique contraction, not taking the ordering of \
the free indices into account.";

ContractedPairs::usage =
	"ContractedPairs is an option for AllContractions which specifies how \
many index pairs should be contracted. The default is All, which amounts to \
contracting all indices. It can also be an integer in the range from 0 to half the number \
total indices. \n\
When UncontractedPairs and ContractedPairs have conflicting values, UncontractedPairs \
takes precendence. \n\
Note that when not all index pairs are contracted, AllContractions returns a \
list with one element for each unique contraction, not taking the ordering of \
the free indices into account.";

AllContractions::usage =
	"AllContractions[expr] returns a sorted list of all possible full contractions of \
expr over its free indices. expr cannot have dummy indices. The free indices have to belong to the same \
tangent bundle, which also has to have a symmetric metric. \n\
\n\
AllContractions[expr, indexList] gives all possible contractions of expr that \
have free indices specified by indexList. This is equivalent to adding an \
auxiliary tensor with indices 'indexList' to expr, computing all contractions, \
and varying w.r.t. the auxiliary tensor afterwards. \n\
\n\
AllContractions[expr, indexList, symm] gives all possible contractions of expr \
with free indices indexList and the symmetry symm imposed on the free indices. \n\
\n\
See also the options SymmetrizeMethod, ContractedPairs, and UncontracedPairs.";


IndexConfigurations::usage =
	"IndexConfigurations[expr] gives a list of all independent index configurations of expr.";

MetricPermutations::usage =
	"MetricPermutations[metric,indices] gives a list of all possible \
permutations of indices distributed over n/2 metrics (n being the number of indices). \
Thus MetricPermutations[g][{a,b,c,d}] gives {g[a,b]g[c,d], g[a,c]g[b,d], g[a,d]g[b,c]}. \
\n\nNaively this gives n! combinations, but because the metric is symmetric and the \
ordering of the product is irrelevant, the number of permutations reduces to \
(n-1)!!, which is roughly the square root of n!. MetricPermutations takes \
this simplification into account.";

UnorderedPairsPermutations::usage =
	"UnorderPairsPermutations[list] gives all permutations of the elements of \
list that are unorder pairs. list has to have an even number of elements.";


(* Young tableaux *)

YoungTableauQ::usage = 
  "YoungTableauQ[tableau] returns True if tableau is a proper Young \
tableau, and False otherwhise. A proper Young tableau is a list of \
lists of integers or symbols, whose intersection is empty. \
Furthermore the length of the lists has to decrease monotonically.";

YoungSymmetrize::usage = 
  "YoungSymmetrize[expr,tableau] symmetrizes a tensorial expression \
according to tableau, where the entries of the tableau have to be the \
set of free indices of expr.\n
  YoungSymmetrize[tensor,tableau] symmetrizes the tensor (withouth \
the [] on tensor) according to tableau, where the entries of tableau \
are integers.";

YoungProject::usage = 
  "YoungProject[expr,tableau] projects a tensorial expression onto \
tableau, where the entries of the tableau have to be the set of free \
indices of expr. \nYoungProject[tensor,tableau] projects the tensor \
(withouth the [] on tensor) onto tableau, where the entries of \
tableau are integers. \n\nThe difference between projecting and \
symmetrizing is that the projection has a different overall factor, \
such that repeatedly projecting does not change the result.";

RiemannYoungRule::usage = 
  "RiemannYoungRule[CD,n] gives the projection rule of n'th covariant \
derivative of the Riemann tensor of the covariant derivative CD onto \
its Young tableau. n has the default levelspec form. The default for n is {0}.";




Begin["`Private`"]



(**************************************)
(* Metric contractions & permutations *)
(**************************************)

Options[AllContractions] ^= {
	Verbose -> False,
	SymmetrizeMethod -> ImposeSymmetry,
	ContractedPairs -> All,
	UncontractedPairs -> None
};

AllContractions[expr_,options___?OptionQ] := 
	AllContractions[expr, IndexList[], options];

AllContractions[expr_,freeIndices:IndexList[___?AIndexQ],options___?OptionQ] := 
	AllContractions[expr, freeIndices, StrongGenSet[{},GenSet[]], options];

AllContractions[expr_,freeIndices:IndexList[___?AIndexQ], symmetry_StrongGenSet,options___?OptionQ] := Module[
	{
		verbose,symmethod,symm,map,exprIndices,numIndices,VB,metric,
		auxT,auxTexpr,indexlist,dummylist,dummies,M,removesign,process,step,
		contractions,
		sym,sgs,frees,dummysets,newdummies,newdummypairs,previousdummies,canon,
		numContractions,unconpairs,conpairs,
		removesign2
	},

	(* Set the options. Note that Function (&) has the HoldAll attribute
	   so we don't need to use SetDelayed. *)
	{verbose,symmethod,conpairs,unconpairs} = 
		{Verbose, SymmetrizeMethod, ContractedPairs, UncontractedPairs} 
		/. CheckOptions[options] /. Options[AllContractions];
	If[TrueQ[verbose],
		map = MapTimed,
		map = Map[#1,#2]&
	];
	
	canon = ReplaceDummies[ToCanonical@ContractMetric[#],dummylist]&;
	If[symmethod === ImposeSymmetry,
		(* ImposeSymmetry: impose symmetry first, and then canonalize later. *)
		symm = canon@ImposeSymmetry[#,freeIndices,SymmetryGroupOfTensor@auxT]&,
		(* ImposeSym: canocalize first (to contract metrics generated at the previous step), then symmetrize. *)
		symm = ImposeSym[canon[#],freeIndices,SymmetryGroupOfTensor@auxT]&
	];
	
	(* Get the indices of the complete expression (expr + freeindices), and count them.  *)
	exprIndices	= Join[freeIndices,IndicesOf[Free][expr]];
	numIndices 	= Length[exprIndices];
	
	If[conpairs === All || unconpairs === None,
		numContractions = numIndices / 2;
	];
	If[conpairs =!= All,
		numContractions = conpairs;
	];	
	If[unconpairs =!= None,
		numContractions = numIndices / 2 - unconpairs;
	];
		
	(* Do some checks *)
	If[IndicesOf[Dummy][expr] =!= IndexList[],
		Throw@Message[AllContractions::error, "Input expression cannot have dummy indices."];
	];
	If[!IntegerQ@numContractions,
		Throw@Message[AllContractions::error, "Can only contract an even number of indices."];
	];
	If[numContractions > numIndices / 2 || numContractions < 0,
		Throw@Message[AllContractions::error, "Number of contractions out of range."];
	];
	If[Length@Union[VBundleOfIndex/@exprIndices] =!= 1,
		Throw@Message[AllContractions::error, "More than one tangent bundle detected."];
	];
	If[Length@MetricsOfVBundle@VBundleOfIndex@First@exprIndices === 0,
		Throw@Message[AllContractions::error, "No metric found in tangent bundle."];
	];

	(* Init geometric variables. *)
	VB 		= VBundleOfIndex@First@exprIndices;
	M 		= BaseOfVBundle@VB;
	metric 	= First@MetricsOfVBundle@VB;
	
	(* Check if the metric is symmetric. *)
	If[xAct`xTensor`Private`SymmetryOfMetric[metric] =!= 1,
		Throw@Message[AllContractions::error, "Can't do contractions for non-symmetric metrics."];
	];
		
	(* Define an auxilary tensor. We vary w.r.t. to this tensor afterwards to free the indices. *)
	Block[{$DefInfoQ=False},DefTensor[auxT@@freeIndices,M,symmetry]];

	(* Replace indices on the auxT (because they might overlap with expr). *)
	auxTexpr	= expr * auxT@@Table[DummyIn@VB,{Length@freeIndices}];
	(* Get the symmetry and its Strong Generating Set. *)
	sym			= SymmetryOf[auxTexpr];
	sgs			= sym[[4]];
	
	(* One more check. *)
	If[numIndices =!= First@sym,
		Throw@Message[
			AllContractions::error, 
			"Number of indices and range of symmetry doesn't match."
		];
	];
	
	
	(* Canonicalization might give a minus sign or zero. 
	   Remove both and the Images head. *)
	removesign[0] 	= Sequence[];
	removesign[-Images[perm_]] := perm;
	removesign[ Images[perm_]] := perm;

	(* The actual processing function that does all possible single contractions of a permutation. *)
	process[entry_] := Map[
		removesign@CanonicalPerm[Images[#],numIndices,sgs,frees,dummysets]&,
		NextDummyPermutations[entry,newdummies,previousdummies]
	] // Union;

	(* Initiliaze some variables for below. *)
	step 			= 1;
	contractions 	= {Range@numIndices};		
	newdummypairs 	= Reverse@Partition[Range@numIndices,{2}];

	(* Apply the processing function numContraction times, starting from the seed. *)
	map[(
		frees 			= Range[numIndices-2step];
		dummysets 		= {DummySet[VB,Reverse[newdummypairs[[1;;step]]],1]};
		newdummies 		= newdummypairs[[step]];
		previousdummies = Flatten@Reverse[newdummypairs[[1;;step-1]]];
		contractions 	= Union@@(map[
				process,
				contractions,
				Description -> "Contracting pair " <> ToString[step++]
			]);
		)&,
		Range[numContractions],
		Description -> StringJoin[
			"Contracting ", ToString@numContractions, 
			" pairs of indices with metric ", ToString@PrintAs[metric]
		]
	];
	
	(* Construct a list of indices. Don't include freeIndices, because they
	   will clash later when we remove the auxiliary tensor. *)
	indexlist 	= IndexList@@GetIndicesOfVBundle[VB,numIndices - numContractions,UpIndex/@freeIndices];
	dummylist	= indexlist[[-numContractions;;-1]];
	dummies		= IndexList@@Riffle[List@@dummylist,-List@@dummylist];
	indexlist	= IndexSort@Join[indexlist[[1;;numIndices-2numContractions]],dummies];
	
	(* Reconstruct tensorial expressions from the permutations. *)
	contractions = xAct`xTensor`Private`Reconstruct[
		sym,
		{1,PermuteList[indexlist,InversePerm@Images[#]]}
	]& /@ contractions;
	
	(* Vary w.r.t. the auxiliary tensor to free the indices. This should be fast,
	   so we don't need to keep the user informed. *)
	contractions = ReplaceAll[
		contractions,
		auxT[inds___] :> Inner[
			delta, 
			IndexList[inds], 
			freeIndices, 
			Times
		]
	];
	
	(* Impose symmetry. *)
	contractions = map[
		symm,
		contractions,
		Description -> "Imposing symmetry."
	];
	
	(* Lastly, undefine the auxiliary tensor. We couldn't do this before
	   because we needed its symmetry in the previous step. *)
	Block[{$UndefInfoQ=False},UndefTensor[auxT]];
	
	(* Return result. Because tensors might have up/down values, we still need to 
	   remove signs, zeros, and duplicates. *)
	removesign2[-x_] := x;
	removesign2[ x_] := x;	
	DeleteCases[Union[removesign2 /@ contractions], 0] 
];

(* 
 * NextDummyPermutations take a list representating an Images permutation,
 * a pair of new dummies, and a list of old dummies, and gives all possible
 * permutations of positions of the new dummies in the initial permutation
 * while leaving the old dummies alone.
 * All inputs are assumed to be lists of numbers, and previousDummies 
 * is assumed to ordered, and newDummy1 < newDummy2. 
 *)
NextDummyPermutations[perm_List, {newDummy1_,newDummy2_}, previousDummies_] := With[
	{
		positions	= Sort[{Position[perm,#][[1,1]],#}&/@previousDummies],
		subsets 	= Subsets[Range[Length@perm - Length@previousDummies],{2}],
		range 		= Range[Length@perm - Length@previousDummies-2]
	},
	Fold[
		Insert[#1, #2[[2]], #2[[1]] ]&,
		Insert[Insert[range, newDummy1, #[[1]]], newDummy2, #[[2]] ],
		positions
	]& /@ subsets
];

(* 
 *	MetricPermutations is no longer used by AllContractions, 
 *  and is superseded by IndexConfigurations, but let's keep it anyhow.
 *)

MetricPermutations[metric_?MetricQ, list_] /; EvenQ@Length@list := 
	Map[Times @@ Map[metric @@ # &, #] &, UnorderedPairsPermutations[list]];

UnorderedPairsPermutations[list_] /; EvenQ@Length@list := 
	Partition[#, 2] & /@ UnorderedPairsPermutations1[list];

(* The actual workhorse. *)
UnorderedPairsPermutations1[list_] /; Length[list] == 2 := {list};
UnorderedPairsPermutations1[list_] /; Length[list] > 2 && EvenQ[Length[list]] := Module[
	{
		previous, last
	},
	last 		= list[[-2 ;; -1]];
	previous 	= UnorderedPairsPermutations1[list[[1 ;; -3]]];
	Flatten[Map[PairPermuteJoin[#, last] &, previous], 1]
];

(* Internal function. *)
PairPermuteJoin[list_, newPair_] := Module[{joined1, joined2, cycles, positions},
	joined1 	= Join[list, newPair];
	joined2 	= Join[list, Reverse@newPair];
	positions 	= 2 Range[Length[list]/2]; 
	cycles 		= xAct`xPerm`Cycles[{#, Length[list] + 1}] & /@ positions;
	Join[
		{joined1},
		xAct`xPerm`PermuteList[joined1, #] & /@ cycles,
		xAct`xPerm`PermuteList[joined2, #] & /@ cycles
	]
];



IndexConfigurations[expr_] := Module[
	{
		sym = SymmetryOf[expr],
		sgs, indices, len, perms
	},
	sgs 	= sym[[4]];
	indices = IndexSort[ IndexList @@ Last /@ sym[[3]] ];
	len 	= sym[[1]];
	perms	= xAct`SymManipulator`Private`TransversalComputation[
		sgs, 
  		Symmetric@Range@len 
  	];
  	Union@Map[
  		xAct`xTensor`Private`Reconstruct[
   			sym, 
   			{1, PermuteList[indices, InversePerm@#]}
		]&, 
		perms
	]
];


(***************************)
(* Young projectors et al. *)
(***************************)

(* 
	I got the idea of using Young projectors from Guillaume Faye and Kaspar Peeters.
 	See http://groups.google.com/group/xact/browse_thread/thread/d19247526163ed62/986a5b04d4fa19f4 .
*)

YoungTableauQ[tableau:{{___Integer}...}|{{(-_Symbol|_Symbol)...}...}] := And[
	Reverse[Length /@ tableau] === Sort[Length /@ tableau],
	If[Length@tableau > 1, 
		Intersection @@ tableau === {}, 
		True
	]
];
YoungTableauQ[_] := False;

ExprYoungQ[expr_,tableau_] := And[
	YoungTableauQ[tableau],
	Sort@IndicesOf[Free][expr] === IndexList @@ Sort@Flatten[tableau]
];
TnsrYoungQ[tnsr_,tableau_] := And[
	YoungTableauQ[tableau],
	Total[Length /@ tableau] === Length[SlotsOfTensor@tnsr]
];

YoungSymmetrize[expr_, tableau_] := Module[{transpose, sym},
	transpose 	= Transpose@PadRight@tableau /. 0 -> Sequence[];
	sym 		= Fold[ToCanonical[Symmetrize[#1, #2]] &, expr, tableau];
	Fold[ToCanonical[Antisymmetrize[#1, #2]] &, sym, transpose]
] /; ExprYoungQ[expr,tableau];

YoungSymmetrize[tensor_?xTensorQ, tableau : {{___Integer} ...}] := Module[{indices},
	indices = DummyIn /@ SlotsOfTensor@tensor;
	YoungSymmetrize[tensor @@ indices, indices[[#]] & /@ tableau]
] /; TnsrYoungQ[tensor,tableau];

YoungSymmetrize[tensor_?xTensorQ] := Total[YoungSymmetrize[tensor, #] & /@ SymmetryTableauxOfTensor[tensor]];

YoungProject[expr_, tableau_] := Module[{sym1, sym2, n, result},
	sym1 = YoungSymmetrize[expr, tableau];
	sym2 = YoungSymmetrize[sym1, tableau];
	Block[{$DefInfoQ = False, $UndefInfoQ = False},
		(* TODO: compute the overall factor n from group-theoretical arguments
		   instead of symmetrizing twice *)
		DefConstantSymbol[n];
		result = n sym1 /. First@SolveConstants[sym1 == n sym2, n] // ToCanonical;
		UndefConstantSymbol[n];
	];
	result
] /; ExprYoungQ[expr,tableau];

YoungProject[tensor_?xTensorQ, tableau : {{___Integer} ...}] := Module[{indices},
   indices = DummyIn /@ SlotsOfTensor@tensor;
   YoungProject[tensor @@ indices, indices[[#]] & /@ tableau]
] /; TnsrYoungQ[tensor,tableau];


RiemannYoungRule[cd_?CovDQ] := RiemannYoungRule[cd,{0}];

RiemannYoungRule[cd_?CovDQ, numcds_Integer] /; numcds >= 0 :=
	RiemannYoungRule[cd,{0,numcds}];

RiemannYoungRule[cd_?CovDQ, {min_Integer,max_Integer}] /; max >= min && min >=0 :=
	Flatten[RiemannYoungRule[cd,{#}]&/@Range[min,max]];

RiemannYoungRule[cd_?CovDQ, {numcds_Integer}] /; numcds >= 0 := Module[
	{
		riemann, indrie, indcds, tableau, expr
	},
	riemann	= GiveSymbol[Riemann, cd];
	indrie	= DummyIn /@ SlotsOfTensor@riemann;
	indcds 	= -Table[DummyIn@First@VBundlesOfCovD[cd], {numcds}];
	tableau = {Join[indrie[[{1, 3}]], indcds], indrie[[{2, 4}]]};
	expr 	= Fold[cd[#2][#1] &, riemann @@ indrie, indcds];
	MakeRule[Evaluate[{expr, YoungProject[expr, tableau]}]]
];

End[]
EndPackage[]