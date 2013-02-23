BeginPackage["xAct`xTras`Algebra`", {
	"xAct`xCore`",
	"xAct`xTensor`",
	"xAct`xTras`xCore`"
}]

(* TensorCollect et al *)

TensorCollector::usage =
	"TensorCollector is an alias for TensorWrapper. Kept for backwards compatibility. Deprecated.";

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

DoTensorCollect::usage = 
  "Deprecated. Superseded by MapTensors / MapTimedTensors. \n \n \
DoTensorCollect[func][expr] maps func on every collected tensor in \
expr. This is useful if you have an expression with one tensor object \
with lots of different constants.";


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

TensorCollect::usage =
	"TensorCollect is an alias for CollectTensors. Kept for backwards compatibility. Deprecated.";

CollectConstants::usage =
	"CollectConstants[expr] collects all constant symbols of expr. It behaves as Collect[expr, constantsof[expr]].";

SolveTensors::usage =
	"SolveTensors[equation, {t1,t2,...}] solves equation for tensors t1, t2, ... \n\
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

(* The next line will clash if TensorWrapper is LinearQ (which it is not). *)
TensorWrapper[x_ * y_] /; FreeQ[x, _?xTensorQ | _?ParameterQ] := x TensorWrapper[y];
TensorWrapper[x_] /; FreeQ[x, _?xTensorQ | _?ParameterQ] && x =!= UnitConstant := x TensorWrapper[ UnitConstant ];

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


TensorCollector = TensorWrapper;


(***********************************)
(* RemoveConstants / RemoveTensors *)
(***********************************)

RemoveConstants[expr_] := TensorWrapper[expr] /. _ * y_TensorWrapper :> y // RemoveTensorWrapper

RemoveTensors[expr_] := TensorWrapper[expr] /. HoldPattern[TensorWrapper[___]] -> 1


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


DoTensorCollect[func_][expr_] := MapTensors[func, expr];


(******************)
(* CollectTensors *)
(******************)

TensorCollect = CollectTensors;

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
	
	(* Apply the TensorWrapper. *)
	mod = Block[{$RecursionLimit=4096},
		TensorWrapper@mod
	];
	print["Applied TensorWrapper"];
	
	(* Find TensorWrappers. *)
	tcs = Union[Cases[mod,HoldPattern[TensorWrapper[_]],{0,Infinity},Heads->True]];
	print["Found " <> ToString@Length@tcs <> " TensorWrappers"];
	
	(* Canonicalize TensorWrappers. *)
	tcscanon = 
		tcs /. HoldPattern@TensorWrapper[arg_] :> TensorWrapper[
			xAct`xTensor`Private`ReplaceDummies2[method@arg,dummies]
		];
	tcscanondd = Union@tcscanon;
	print["Canonicalized " <> ToString@Length@tcscanondd <> " TensorWrappers"];
	
	(* Reinsert canonicalized TensorWrappers. *)
	mod = mod/.Inner[Rule,tcs,tcscanon,List];
	print["Replaced TensorWrappers"];

	
	(* If there are denominators with a plus inside TensorWrapper heads, 
	   things might not by fully simplified. Warn the user. *)
	If[!FreeQ[tcscanondd, HoldPattern[Power[y_, x_]] /; x < 0 && !FreeQ[y, Plus]],
		Message[CollectTensors::denominator]
	];
	
	(* Collect in terms of the tensors, simplify the overall factors, and remove TensorWrappers. *)
	Collect[mod, tcscanondd] 
		/. x_*y_TensorWrapper :> simplify[x] y 
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
			(* Get all constant symbols, and select the non-numeric ones (we don't want things like Pi) *)
			Select[
				Union@Cases[expr, _Symbol?ConstantSymbolQ, {0,Infinity}, Heads->True],
				!NumericQ[#]&
			],
			Flatten[{!notvars}]
		]
	];

SolveConstants[expr_,varsdoms__] := 
	Solve[expr /. HoldPattern[equation_Equal] :> ToConstantSymbolEquations[equation], varsdoms];	


(****************)
(* SolveTensors *)
(****************)

(* 
	TODO:
	
	Instead of using TensorWrappers, we could use BreakInMonomials, and solve for Monomials.
	This has the advantage that equations like T1 T2 - T1 T3 == 0 can get solved as
 	{{T1 -> 0}, {T2 -> T3}} instead of the less general solution {{T1 T2 -> T1 T3}}.
 *)


Options[SolveTensors] ^= {
	MakeRule -> True,
	SortMethod -> Sort
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
SolveTensors1[expr_, patterns_List, options___?OptionQ] := Module[{mr,sm,collected,tensors,ntw,sorted,mrrule},
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
	(* Apply tensor wrappers. *)
	collected = CollectTensors[
		expr, 
		RemoveTensorWrapper -> False, 
		CollectMethod -> Identity, 
		SimplifyMethod -> Identity
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
	RemoveTensorWrapper[Solve[collected, sorted]] /. mrrule	
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