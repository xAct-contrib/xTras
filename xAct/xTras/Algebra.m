BeginPackage["xAct`xTras`Algebra`", {
	"xAct`xCore`",
	"xAct`xTensor`",
	"xAct`xTras`xCore`"
}]

(* TensorCollect et al *)


TensorCollector::usage = 
  "TensorCollector[expr] wraps all tensors in expr in a head \
'TensorCollector'.";

$TensorCollectorColor::usage =
	"$TensorCollectorColor is a global variable specifying the color of \
the parentheses surrounding the formatting of a TensorCollector expression. \
The default value is blue (RGBColor[0,0,1]).";

RemoveTensorCollector::usage =
	"RemoveTensorCollector[expr] removes TensorCollectors from expr. \n\
RemoveTensorCollector is also an option for CollectTensors. \
If True, CollectTensors removes the TensorCollector heads from the expression \
before returning. The default is True.";

RemoveConstants::usage = 
  "RemoveConstants[expr] removes all constants from the tensorial \
expression expr.";

RemoveTensors::usage = 
  "RemoveTensors[expr] removes all tensors from expr, and leaves just \
the constants.";

CollectMethod::usage =
	"CollectMethod is an option for CollectTensors that specifies which \
(pure) function is used before tensors are collected. \
The Default is ToCanonical@ContractMetric@NoScalar@#& .";

SimplifyMethod::usage =
	"SimplifyMethod is an option for CollectTensors that specifies how \
collected prefactors are simplified. The Default is Simplify";

CollectTensors::usage = 
  "CollectTensors[expr] acts as Collect[expr,tensorsof[expr]]";

CollectTensors::denominator = 
	"There are denominators with a sum inside TensorCollectors. \
Things might not have been fully collected.";

TensorCollect::usage =
	"TensorCollect is an alias for CollectTensors. Deprecated.";

DoTensorCollect::usage = 
  "DoTensorCollect[func][expr] maps func on every collected tensor in \
expr. This is useful if you have an expression with one tensor object \
with lots of different constants.";

CollectConstants::usage =
	"CollectConstants[expr] act as Collect[expr, constantsof[expr] ].";

SolveTensors::usage =
	"SolveTensors[equation, {t1,t2,...}] solves equation for tensors t1, t2, ... \n\
\n\
SolveTensors[equation] attempts to solve equation for any tensors in it.";

SortMethod::usage =
	"SortMethod is an option for SolveTensors.";

MakeEquationRule::usage = 
  "Deprecated.\n\n\
MakeEquationRule[{equation,pattern,cond}] returns rules for \
tensors matching pattern in the given equation.\
\nNote that is extremely similar to IndexSolve.\
\nMakeEquationRule is deprecated; it has been superseded by SolveTensors.";

ToConstantSymbolEquations::usage =
	"ToConstantSymbolEquations[eq] takes the tensorial equation eq \
and turns it into equations for the constant symbols appearing eq.";

SolveConstants::usage =
	"SolveConstants[expr, vars] attempts to solve the tensorial expr for constant \
symbols vars. \n\
SolveConstants[expr] attempts to solve expr for all the constant \
symbols appearing in expr. \n\
SolveConstants[expr, !vars] attempts to solve expr for all the constant symbols \
appearing in expr, except vars.";


Begin["`Private`"]



(***************************)
(*                         *)
(*   CollectTensors et. al. *)
(*                         *)
(***************************)



(*******************)
(* TensorCollector *)
(*******************)

(* Define an inert head "TensorCollector". *)
Block[{$DefInfoQ=False},
	DefInertHead[
		TensorCollector,
		(* Setting LinearQ to True will slow evaluation down dramatically 
		   because of a clashing definition below. *)
		LinearQ -> False, 
		ContractThrough -> {}, 
		Master -> Null, 
		PrintAs -> Identity, 
		ProtectNewSymbol -> False,
		DefInfo -> {"", ""}
	];
];
(* Thread TensorCollector over List, Plus, and Equal. *)
TensorCollector[x_List]  := TensorCollector /@ x;
TensorCollector[x_Plus]  := TensorCollector /@ x;
TensorCollector[x_Equal] := TensorCollector /@ x;
(* TensorCollector on itself returns a single TensorCollector. *)
HoldPattern[TensorCollector[TensorCollector[expr_]]] := TensorCollector[expr];

(* The next line will clash if TensorCollector is LinearQ (which it is not). *)
TensorCollector[x_ * y_] /; FreeQ[x, _?xTensorQ | _?ParameterQ] := x TensorCollector[y];
TensorCollector[x_] /; FreeQ[x, _?xTensorQ | _?ParameterQ] := x;

(* TensorCollector formatting. The same as how Scalar formats, only now in blue. *)
$TensorCollectorColor = RGBColor[0, 0, 1];

TensorCollector /: MakeBoxes[TensorCollector[expr_], StandardForm] := 
xAct`xTensor`Private`interpretbox[
	TensorCollector[expr], 
	RowBox[{
		StyleBox["(", FontColor -> $TensorCollectorColor], 
		MakeBoxes[expr, StandardForm], 
		StyleBox[")", FontColor -> $TensorCollectorColor]
	}]
];


RemoveTensorCollector[expr_] := expr /. HoldPattern@TensorCollector[x_] :> x;


(***********************************)
(* RemoveConstants / RemoveTensors *)
(***********************************)

RemoveConstants[expr_] := expr /. x_?ConstantExprQ *y_ /; ! FreeQ[y, _?xTensorQ | _?ParameterQ] :> y
SetAttributes[RemoveConstants, Listable]

RemoveTensors[expr_] := TensorCollector[expr] /. HoldPattern[TensorCollector[___]] -> 1
SetAttributes[RemoveTensors, Listable]


(******************)
(* CollectTensors *)
(******************)

TensorCollect = CollectTensors;

Options[CollectTensors] ^= {
	CollectMethod -> Default, 
	SimplifyMethod -> Simplify, 
	RemoveTensorCollector -> True, 
	Verbose -> False
}

CollectTensors[expr_, options___?OptionQ] := expr;
CollectTensors[expr_List, options___?OptionQ] := CollectTensors[#,options]& /@ expr;
CollectTensors[expr_, options___?OptionQ] /; !FreeQ[expr, Plus | _?xTensorQ] && Head[expr] =!= List :=
Module[{verbose,print,time,method,simplify,rtc,mod,dummies,tcs,tcscanon,tcscanondd},
	
	(* Get the options. *)	
	{method,simplify,rtc,verbose} = 
		{CollectMethod,SimplifyMethod,RemoveTensorCollector,Verbose}
		/. CheckOptions[options] /. Options[CollectTensors];
	
	verbose = TrueQ[verbose];
	rtc 	= TrueQ[rtc];
	
	If[verbose,
		time=AbsoluteTime[];
		Print[
			"** Collecting tensors in expression of length ", ToString@Length@expr, 
			" and head ", ToString@Head@expr, "."
		];
		print[msg_]:=(
			Print[
				"** ", msg, " in ",
				ToString@Round[AbsoluteTime[]-time,0.01], 
				"s."
			];
			time=AbsoluteTime[];
		)
	,
		print[msg_]:=Null
	];
	
	If[rtc,
		rtc = RemoveTensorCollector,
		rtc = Identity
	];
	
	(* If we have perturbations in the expression don't contract metrics etc. *)
	If[!FreeQ[expr,Perturbation],
		method = Identity;
	];
	If[method === Default,
		method = ToCanonical@ContractMetric@NoScalar@#&;
	];
	
	(* Expand. *)
	If[verbose,
		mod = MapTimedIfPlus[Expand,expr,Description->"Expanding terms"];
	,
		mod = MapIfPlus[Expand,expr];
	];
	print["Expanded to " <> ToString@Length@mod <> " terms"];
	
	(* Find dummies. *)
	dummies = Sort[xAct`xTensor`Private`IndexName /@ FindDummyIndices[Evaluate@mod]];
	print["Found " <> ToString@Length@dummies <> " dummies, " <> ToString@Shallow[List@@dummies]];
	
	(* Replace dummies. *)
	If[Length@dummies > 0,
		mod = xAct`xTensor`Private`ReplaceDummies2[mod,dummies];
		print["Replaced dummies"];
	];
	
	(* Apply the tensorcollector. *)
	mod = Block[{$RecursionLimit=4096},
		TensorCollector@mod
	];
	print["Applied TensorCollector"];
	
	(* Find tensorcollectors. *)
	tcs = Union[Cases[mod,HoldPattern[TensorCollector[_]],{0,Infinity},Heads->True]];
	print["Found " <> ToString@Length@tcs <> " TensorCollectors"];
	
	(* Canonicalize tensorcollectors. *)
	tcscanon = 
		tcs /. HoldPattern@TensorCollector[arg_] :> TensorCollector[
			xAct`xTensor`Private`ReplaceDummies2[method@arg,dummies]
		];
	tcscanondd = Union@tcscanon;
	print["Canonicalized " <> ToString@Length@tcscanondd <> " TensorCollectors"];
	
	(* Reinsert canonicalized tensorcollectors. *)
	mod = mod/.Inner[Rule,tcs,tcscanon,List];
	print["Replaced TensorCollectors"];

	
	(* If there are denominators with a plus inside TensorCollector heads, 
	   things might not by fully simplified. Warn the user. *)
	If[!FreeQ[tcscanondd, HoldPattern[Power[y_, x_]] /; x < 0 && !FreeQ[y, Plus]],
		Message[CollectTensors::denominator]
	];
	
	(* Collect in terms of the tensors, simplify the overall factors, and remove TensorCollectors. *)
	Collect[mod, tcscanondd] 
		/. x_*y_TensorCollector :> simplify[x] y 
		// rtc
];


Options[CollectConstants] ^= {
	SimplifyMethod -> CollectTensors
}

Default[CollectConstants] ^= !{};
CollectConstants[expr_,Optional[notvars:!(_List|_Symbol)], options___?OptionQ] := 
	CollectConstants[
		expr,
		Complement[
			Union@Cases[expr, _Symbol?ConstantSymbolQ, {0,Infinity}, Heads->True],
			Flatten[{!notvars}]
		],
		options
	];

CollectConstants[expr_, constant_Symbol, options___?OptionQ] := 
	CollectConstants[expr,{constant}, options];

CollectConstants[expr_, constants_List, options___?OptionQ] := Module[
	{
		simplify = SimplifyMethod /. CheckOptions[options] /. Options[CollectConstants]
	},
	Collect[expr, constants] /. (y_ * x:Alternatives@@constants) :> (simplify[y] x)
];




DoTensorCollect[func_][expr_] := Module[{collected, map},
	collected = TensorCollector[expr];
	map[subexpr_] := If[FreeQ[subexpr, TensorCollector],
		func[subexpr],
		subexpr /. HoldPattern[TensorCollector[p___]] :> func[p]
 	];
	MapIfPlus[map,collected]
];


(**************************)
(* SolveConstants et. al. *)
(**************************)

(* Equal with multiple arguments: take the first two and chain the rest recursively. *)
ToConstantSymbolEquations[Equal[a_,b_,c_,d___]] := 
	ToConstantSymbolEquations[Equal[a,b]] && ToConstantSymbolEquations[Equal[b,c,d]]; 

(* Equal over lists: Thread over the lists and feed back into ToConstantSymbolEquations. *)
ToConstantSymbolEquations[eq:(Equal[_List,_]|Equal[_,_List]|Equal[_List,_List])] := 
	Thread[eq] /. eqs_Equal :> ToConstantSymbolEquations[eqs];

(* Main function *)
ToConstantSymbolEquations[eq:Equal[lhs_,rhs_]] := Module[{collected,list,freeT,withT},
	collected = CollectTensors[lhs - rhs, 
		CollectMethod->Default, 
		SimplifyMethod->Identity,
		RemoveTensorCollector->False
	];
	list = If[Head[#] === Plus, 
			List@@#, 
			List@#
		]& @ collected;
	freeT = Select[list,FreeQ[#,TensorCollector]&];
	withT = Select[list,!FreeQ[#,TensorCollector]&]  /. HoldPattern[TensorCollector[___]] -> 1;
	
	Apply[
		And, 
		Equal[#,0]& /@ DeleteDuplicates@Append[withT, Plus@@freeT] 
	] 
];

(* This is a handy shortcut. *)
Default[SolveConstants] ^= !{};
SolveConstants[expr_,Optional[notvars:!(_List|_Symbol)]] := 
	Solve[
		expr /. HoldPattern[equation_Equal] :> ToConstantSymbolEquations[equation],
		Complement[
			Union@Cases[expr, _Symbol?ConstantSymbolQ, {0,Infinity}, Heads->True],
			Flatten[{!notvars}]
		]
	];

SolveConstants[expr_,varsdoms__] := 
	Solve[expr /. HoldPattern[equation_Equal] :> ToConstantSymbolEquations[equation], varsdoms];	


(****************)
(* SolveTensors *)
(****************)

Options[SolveTensors] ^= {
	MakeRule -> True,
	SortMethod -> Sort
}

(* Public function with specified tensors. *)
SolveTensors[expr_, tensors_List, options___?OptionQ] := SolveTensors1[
	CollectTensors[
		expr, 
		RemoveTensorCollector -> False, 
		CollectMethod -> Identity, 
		SimplifyMethod -> Identity
	],
	TensorCollector[tensors],
	options
];

(* Public function withouth specified tensors: try to solve for all tensors. *)
SolveTensors[expr_, options___?OptionQ] := 
SolveTensors1[
	#,
	Union@Cases[#, HoldPattern@TensorCollector[_], {0, Infinity}, Heads -> True],
	options
]& @ CollectTensors[
	expr, 
	RemoveTensorCollector -> False, 
	CollectMethod -> Identity, 
	SimplifyMethod -> Identity
];

(* Main driver. *)
SolveTensors1[eqs_,tensors_List,options___?OptionQ] := Module[{mr,sm,mrrule},
	(* Get options. *)	
	{mr,sm} = {MakeRule,SortMethod} 
		/. CheckOptions[options] 
		/. Options[SolveTensors];
	(* Rule for making proper xTensor rules. *)
	If[TrueQ[mr],
		mrrule = Rule[lhs_,rhs_] :> Sequence@@MakeRule[Evaluate[
				{
					lhs,
					If[Length@IndicesOf[][lhs] === 0 && Length@IndicesOf[][rhs] =!= 0,
						PutScalar[rhs],
						rhs
					]
				}
			],
			options
		],
		mrrule = {}
	];
	(* Do the actual work. *)
	RemoveTensorCollector[Solve[eqs, sm@tensors]] /. mrrule
];

(* Deprecated, superseded by SolveTensors. *)
MakeEquationRule[{Equal[LHS_,RHS_], pattern_, cond___}, options___?OptionQ]:=
  Module[{expanded, list, terms, coefficient, lhs, rhs},
	expanded	= CollectTensors[LHS - RHS, CollectMethod->Default, SimplifyMethod->Identity];
	list		= If[Head[expanded] === Plus, List@@expanded, List@expanded];
	terms = Cases[list, (Optional[c_] * t_) /; ConstantExprQ[c] && MatchQ[t, HoldPattern@pattern] :> {t, c}];
	If[Length[terms] =!=1, Return[{}]];
	coefficient	= terms[[1,2]];
	lhs			= terms[[1,1]];
	rhs			= Simplification[lhs - (expanded/coefficient)];
	If[Length[IndicesOf[][lhs]] === 0, rhs = PutScalar[rhs]];
	MakeRule[Evaluate[{lhs, rhs, cond}], options]
];

SetAttributes[MakeEquationRule,HoldFirst];





End[]
EndPackage[]