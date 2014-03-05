(* TensorCollect et al *)

UnitConstant::usage =
	"UnitConstant is a constant whose value is one. It prints as 1, and is used with TensorWrapper.";

TensorWrapper::usage = 
  "TensorWrapper[expr] wraps all tensors in expr in a head \
'TensorWrapper'.";

$TensorWrapperColor::usage =
	"$TensorWrapperColor is a global variable specifying the color of \
the parentheses surrounding the formatting of a TensorWrapper expression. \
The default value is blue (RGBColor[0,0,1]).";

RemoveTensorWrapper::usage =
	"RemoveTensorWrapper[expr] removes TensorWrappers from expr. \n\
RemoveTensorWrapper is also an option for CollectTensors. \
If True, CollectTensors removes the TensorWrapper heads from the expression \
before returning. The default is True.";

RemoveConstants::usage = 
  "RemoveConstants[expr] removes all constants from the tensorial \
expression expr.";

RemoveTensors::usage = 
  "RemoveTensors[expr] removes all tensors from expr, and leaves just \
the constants.";

MapTimedTensors::usage =
	"MapTimedTensors[f,expr] maps f over all tensorial expressions in expr with MapTimed.";

MapTensors::usage =
	"MapTensors[f,expr] maps f over all tensorial expressions in expr.";


CollectMethod::usage =
	"CollectMethod is an option for CollectTensors that specifies which \
(pure) function is used before tensors are collected. \
The Default is ToCanonical@ContractMetric@NoScalar@#& .";

SimplifyMethod::usage =
	"SimplifyMethod is an option for CollectTensors that specifies how \
collected prefactors are simplified. The Default is Simplify";

CollectTensors::usage = 
  "CollectTensors[expr] collects all tensors of expr. It behaves a bit as Collect[expr, tensorsof[expr]] would.";

CollectTensors::denominator = 
	"There are denominators with a sum inside TensorWrappers. \
Things might not have been fully collected.";

CollectConstants::usage =
	"CollectConstants[expr] collects all constant symbols of expr. It behaves as Collect[expr, constantsof[expr]].";

SolveTensors::usage =
	"SolveTensors[equation, {t1,t2,...}] solves equation for tensors t1, t2, ... \n\
SolveTensors[equation] attempts to solve equation for any tensors in it.";

SortMethod::usage =
	"SortMethod is an option for SolveTensors.";

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



(*****************)
(* TensorWrapper *)
(*****************)

Block[{$DefInfoQ=False},
	DefConstantSymbol[UnitConstant, PrintAs -> "1"];
];

UnitConstant /: UnitConstant * UnitConstant = UnitConstant;
UnitConstant /: UnitConstant * TensorWrapper[ UnitConstant ] := TensorWrapper[ UnitConstant ];
UnitConstant /: Power[UnitConstant, x_Integer] /; x > 1 := UnitConstant;

(* Define an inert head "TensorWrapper". *)
Block[{$DefInfoQ=False},
	DefInertHead[
		TensorWrapper,
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
(* Thread TensorWrapper over List, Plus, and Equal. *)
TensorWrapper[x_List]  := TensorWrapper /@ x;
TensorWrapper[x_Plus]  := TensorWrapper /@ x;
TensorWrapper[x_Equal] := TensorWrapper /@ x;
(* TensorWrapper on itself returns a single TensorWrapper. *)
HoldPattern[TensorWrapper[TensorWrapper[expr_]]] := TensorWrapper[expr];

(* TensorWrapper on Times. Doing a recursive definition like
   TensorWrapper[x_ * y_] /; FreeQ[x, _?xTensorQ | _?ParameterQ] := x TensorWrapper[y];
   is exponentially slow. So we do it differently. *)
TensorWrapper[HoldPattern[Times[x__]]] :=
	With[
		{
			tensorqlist = ! FreeQ[#, _?xTensorQ | _?ParameterQ] & /@ {x}
		},
		Times[
			(* The terms without tensors go outside the tensorwrapper. *)
			Times @@ Pick[{x}, Not /@ tensorqlist],
			(* The terms with tensors go inside the tensorwrapper. *)
			(* Note that when no terms go inside the tensorwrapper,
			   it still produces rest*TensorWrapper[UnitConstant] because
			   Times[] gives 1. *)
			TensorWrapper[
				Times @@ Pick[{x}, tensorqlist] 
			]
		(* The conditional below is to prevent infinite recursion
		   in the case when all factors have tensors. *)
		] /; ! And @@ (tensorqlist)
	];
(* TensorWrapper on a single term. *)
TensorWrapper[x_] /; Head[x] =!= Times && x =!= UnitConstant && FreeQ[x, _?xTensorQ | _?ParameterQ] := 
	x TensorWrapper[UnitConstant];

(* Move a tensor wrapper on the unit constant inside another tensor wrapper *)
TensorWrapper /: TensorWrapper[UnitConstant] * HoldPattern@TensorWrapper[x_] := TensorWrapper[x];

(* TensorWrapper formatting. The same as how Scalar formats, only now in blue. *)
$TensorWrapperColor = RGBColor[0, 0, 1];

TensorWrapper /: MakeBoxes[TensorWrapper[expr_], StandardForm] := 
xAct`xTensor`Private`interpretbox[
	TensorWrapper[expr], 
	RowBox[{
		StyleBox["(", FontColor -> $TensorWrapperColor], 
		MakeBoxes[expr, StandardForm], 
		StyleBox[")", FontColor -> $TensorWrapperColor]
	}]
];


RemoveTensorWrapper[expr_] := expr /. FoldedRule[
	HoldPattern@TensorWrapper[x_] :> x,
	UnitConstant -> 1
];



(***********************************)
(* RemoveConstants / RemoveTensors *)
(***********************************)

RemoveConstants[expr_] := 
	CollectTensors[
		expr, 
		CollectMethod -> Identity,
		SimplifyMethod -> Identity, 
		RemoveTensorWrapper -> False
	] /. _ * y_TensorWrapper :> y // RemoveTensorWrapper

RemoveTensors[expr_] :=  
	CollectTensors[
		expr, 
		CollectMethod -> Identity,
		SimplifyMethod -> Identity, 
		RemoveTensorWrapper -> False
	] /. HoldPattern[TensorWrapper[___]] -> 1


(********************************)
(* MapTensors / MapTimedTensors *)
(********************************)

MapTimedTensors[func_, expr_, options___?OptionQ] := MapTensors1[func, expr, MapTimed, options];
MapTensors[func_, expr_] := MapTensors1[func, expr, Map];

MapTensors1[func_, expr_, mapmethod_,options___] := Module[
	{
		wrapped, wrappers, fwrappers
	},
	wrapped 	= TensorWrapper[expr];
	wrappers	= Union[Cases[wrapped,HoldPattern[TensorWrapper[_]],{0,Infinity},Heads->True]];
	fwrappers	= mapmethod[func, RemoveTensorWrapper[wrappers], options ];
	wrapped /. Inner[Rule,wrappers,fwrappers, List]
];



(******************)
(* CollectTensors *)
(******************)

(* We need NoScalar to map over List and Equal.
   TODO: when this is in xAct`xTensor` (1.0.5+), remove it. *)

Unprotect[NoScalar];
NoScalar[expr_List] := NoScalar /@ expr;
NoScalar[expr_Equal] := NoScalar /@ expr;
Protect[NoScalar];

Options[CollectTensors] ^= {
	CollectMethod -> Default, 
	SimplifyMethod -> Simplify, 
	RemoveTensorWrapper -> True, 
	Verbose -> False
}

CollectTensors[expr_, options___?OptionQ] :=
Module[{verbose,print,time,method,simplify,rtc,mod,dummies,tcs,tcscanon,tcscanondd},
	
	(* Get the options. *)	
	{method,simplify,rtc,verbose} = 
		{CollectMethod,SimplifyMethod,RemoveTensorWrapper,Verbose}
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
		rtc = RemoveTensorWrapper,
		rtc = Identity
	];
	
	(* If we have perturbations in the expression don't contract metrics etc. *)
	If[!FreeQ[expr,Perturbation],
		method = Identity;
	];
	If[method === Default,
		method = ToCanonical@ContractMetric@#&;
	];
	
	(* Expand. *)
	If[!FreeQ[expr,Scalar],
		If[verbose,
			mod = MapTimedIfPlus[ExpandTensors@NoScalar[#]&,expr,Description->"Removing Scalar heads and expanding tensorial terms"];
		,
			mod = MapIfPlus[ExpandTensors@NoScalar[#]&,expr];
		]
	,
		If[verbose,
			mod = MapTimedIfPlus[ExpandTensors[#]&,expr,Description->"Expanding tensorial terms"];
		,
			mod = MapIfPlus[ExpandTensors[#]&,expr];
		]
	];
	print["Expanded to " <> ToString@Length@mod <> " tensorial terms"];
	
	(* Find dummies. *)
	(* TODO: this doesn't work when expr is a list whose free indices are not uniform. *)
	dummies = Sort[xAct`xTensor`Private`IndexName /@ FindDummyIndices[Evaluate@mod]];
	print["Found " <> ToString@Length@dummies <> " dummies, " <> ToString@Shallow[List@@dummies]];
	
	(* Replace dummies. *)
	If[Length@dummies > 0,
		mod = xAct`xTensor`Private`ReplaceDummies2[mod,dummies];
		print["Replaced dummies"];
	];
	
	(* Apply the TensorWrapper. *)
	mod = Block[{$RecursionLimit=4096},
		TensorWrapper@mod
	];
	print["Applied TensorWrapper"];
	
	(* Find TensorWrappers. *)
	tcs = Union[Cases[mod,HoldPattern[TensorWrapper[_]],{0,Infinity},Heads->True]];
	print["Found " <> ToString@Length@tcs <> " TensorWrappers"];
	
	(* Canonicalize TensorWrappers. *)
	tcscanon = If[!verbose,
		tcs /. HoldPattern@TensorWrapper[arg_] :> TensorWrapper[
			xAct`xTensor`Private`ReplaceDummies2[method@arg,dummies]
		],
		MapTimed[ 
			( # /. HoldPattern@TensorWrapper[arg_] :> TensorWrapper[
				xAct`xTensor`Private`ReplaceDummies2[method@arg,dummies] 
			] )&,
			tcs,
			Description -> "Canonicalizing TensorWrappers"
		]
	];
	(* Find unique tensor wrappers. *)
	tcscanondd = Union[Cases[tcscanon,HoldPattern[TensorWrapper[_]],{0,Infinity},Heads->True]];
	print["Canonicalized " <> ToString@Length@tcscanondd <> " TensorWrappers"];
	
	(* Reinsert canonicalized TensorWrappers. *)
	mod = mod/.Inner[Rule,tcs,tcscanon,List];
	print["Replaced TensorWrappers"];

	
	(* If there are denominators with a plus inside TensorWrapper heads, 
	   things might not by fully simplified. Warn the user. *)
	If[!FreeQ[tcscanondd, HoldPattern[Power[y_, x_]] /; x < 0 && !FreeQ[y, Plus]],
		Message[CollectTensors::denominator]
	];
	
	(* Collect in terms of the tensors. *)
	mod = Collect[mod, tcscanondd];
	print["Collected TensorWrappers"];
	
	(* Simplify the overall factors, and remove TensorWrappers. *)
	mod = If[!verbose,
		(mod /. x_*y_TensorWrapper :> simplify[x] y) // rtc,
		MapTimedIfPlus[
			rtc[# /. x_*y_TensorWrapper :> simplify[x] y]&,
			mod,
			Description -> "Simplifying coefficient of each term"
		]
	]; 
	print["Simplified coefficient of each term"];
	
	(* Return. *)
	mod
];

ExpandTensors::usage =
	"ExpandTensors[expr] only expands sub-expressions in expr that have tensors."; 
(* Thread over Plus, List, and Equal. *)
ExpandTensors[expr_Plus]  := ExpandTensors /@ expr;
ExpandTensors[expr_List]  := ExpandTensors /@ expr;
ExpandTensors[expr_Equal] := ExpandTensors /@ expr;
(* Expand only the factors of Times that have tensors. *)
ExpandTensors[HoldPattern[Times[x__]]] :=
	Times[
		(* The terms without tensors are not expanded. *)
		Times @@ Pick[{x}, Not /@ #],
		(* The terms with tensors are expanded. *)
		Expand[
			Times @@ Pick[{x}, #] 
		]
	]& [ ! FreeQ[#, _?xTensorQ | _?ParameterQ] & /@ {x} ];
(* On everything else: expand. *)
ExpandTensors[expr_] := Expand[expr];


Options[CollectConstants] ^= {
	SimplifyMethod -> CollectTensors
}

Default[CollectConstants] ^= !{};
CollectConstants[expr_,Optional[notvars:!(_List|_Symbol)], options___?OptionQ] := 
	CollectConstants[
		expr,
		Complement[
			ConstantSymbolsOf[expr],
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
ToConstantSymbolEquations[eq:Equal[lhs_,rhs_]] := Module[{collected,list},
	collected = CollectTensors[lhs - rhs, 
		CollectMethod->Default, 
		SimplifyMethod->Identity,
		RemoveTensorWrapper->False
	];
	list = If[Head[#] === Plus, 
			List@@#, 
			List@#
		]& @ collected;
	Apply[
		And, 
		Equal[#,0]& /@ Union[list  /. HoldPattern[TensorWrapper[___]] -> 1]
	] 
];

(* This is a handy shortcut. *)
Default[SolveConstants] ^= !{};
SolveConstants[expr_,Optional[notvars:!(_List|_Symbol)]] := 
	Solve[
		expr /. HoldPattern[equation_Equal] :> ToConstantSymbolEquations[equation],
		Complement[
			ConstantSymbolsOf[expr],
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
	SortMethod -> Sort,
	BreakInMonomials -> True
}

(* User driver with a list of tensors. *)
SolveTensors[expr_, tensors_List, options___?OptionQ] :=
	SolveTensors1[expr, tensors, SortMethod -> Identity, options];

(* User driver with a single tensor. *)
SolveTensors[expr_, tensor_, options___?OptionQ] /; Head[tensor] =!= Rule && Head[tensor] =!= List :=
	SolveTensors1[expr, {tensor}, SortMethod -> Identity, options];

(* User driver with no tensors. *)
SolveTensors[expr_, options___?OptionQ] :=
	SolveTensors1[expr, {_}, options];

(* Internal driver. *)
SolveTensors1[expr_, patterns_List, options___?OptionQ] := Module[
	{
		mr,sm,collected,tensors,ntw,sorted,mrrule,dummies,breakin,breakinrule
	},
	
	(* Get options. *)	
	{mr,sm,breakin} = {MakeRule,SortMethod,BreakInMonomials} 
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
			FilterRules[{options},Options[MakeRule]]
		],
		mrrule = {}
	];

	(* Apply tensor wrappers. *)
	collected = CollectTensors[
		expr, 
		RemoveTensorWrapper -> False, 
		CollectMethod -> Identity,
		SimplifyMethod -> Identity
	];
	
	dummies = Sort[xAct`xTensor`Private`IndexName /@ FindDummyIndices[Evaluate@expr]];
	
	(* Break the tensor wrappers in monomials *)
	If[TrueQ[breakin],
		collected = collected /. RuleDelayed[
			HoldPattern@TensorWrapper[x_],
			Apply[
				Times,
				xAct`xTensor`Private`MonomialsOfTerm[x]
					/. Monomial[mono_,inds_] :> TensorWrapper[
						xAct`xTensor`Private`ReplaceDummies2[mono,dummies]
					]
					/. xAct`xTensor`Private`ScalarMonomial[args__] :> Times@@(TensorWrapper /@ List[args])
			]
		] /. HoldPattern[TensorWrapper[x_^y_]]:>TensorWrapper[x]^y;
		breakinrule = RuleDelayed[
			HoldPattern@TensorWrapper[x_]^Optional[n_] /; ScalarQ[x],
			TensorWrapper@NoScalar[Scalar[x]^n]
		];
	,
		breakinrule = {};
	];
	
	(* Find tensors. We don't want to solve for the unit constant, so remove that one. *)
	tensors = DeleteCases[
		Union@Cases[collected, HoldPattern@TensorWrapper[_], {0, Infinity}, Heads -> True],
		TensorWrapper@UnitConstant
	];
	
	(* Select tensors with patterns. The map is over patterns and not tensors to preserve the
	   ordering of patterns. *)
	tensors = Flatten[
		Map[
			Function[patt,Select[tensors,MatchQ[#,HoldPattern@TensorWrapper@patt]&]],
			patterns
		]
		,
		1
	];
	
	(* Sort the tensors. Because the sort method might not work on the TensorWrapper head
	   (because the user doesn't know SolveTensors uses TensorWrappers internally, and didn't 
	   take that into account), we need to make sure we sort without TensorWrappers. *)
	ntw 	= RemoveTensorWrapper[tensors];
	sorted 	= Part[
			tensors, 
			Flatten[Position[ntw,#,{1}]& /@ sm[ntw] ]
	];
	
	(* Solve the equation(s). *)
	RemoveTensorWrapper[
		Simplify@Solve[collected, sorted] /. breakinrule
	] /. mrrule	
]; 




End[]