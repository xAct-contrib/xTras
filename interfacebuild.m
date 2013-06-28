(*
 *  interfacebuild.m
 *)

BeginPackage["InterfaceBuild`"]

ProcessDirectory::usage = 
	"ProcessDirectory[inputDir, outputFile] parses the usage messages for all documentation \
notebooks in inputDir and writes the output to outputFile.";

ProcessSourceNB::usage =
	"ProcessSourceNB[nbFile_, outputFile] parses the usage messages for the given documentation \
notebook and appends the output to outputFile.";



Begin["`Private`"]

ProcessDirectory[directory_String, outputFile_String, log_:Print] :=
	Module[
		{
			files = FileNames["*.nb", directory]
		},
		log["Found " <> ToString@Length@files <> " source notebooks."];
		Put[outputFile];
		log["Cleared output file " <> outputFile];
		ProcessSourceNB[#, outputFile, log]& /@ files;
	];


ProcessSourceNB[sourceNB_, outputFile_, log_:Print] := 
	WriteUsageMessage[
		ParseUsageMessage@GetUsageMessage@sourceNB,
		outputFile,
		log
	];


WriteUsageMessage[{symbol_,usagem_}, outputFile_, log_:Print] :=
	With[
		{
			mess = StringTrim@ConvertToString@usagem,
			symb = ToExpression@symbol
		},
		PutAppend[
			Unevaluated[symb::usage = mess;],
			outputFile
		];
		log["Wrote usage message for " <> symbol];
	];

WriteUsageMessage[{nbName_,Null}, _, log_:Print] :=
	log["Skipped notebook " <> nbName];


(* Returns {symbolName, usageMessage}, or {Null, Null} if nothing was found. *)
GetUsageMessage[sourceNB_] := 
	Module[
		{
			nbfile = Get[sourceNB],
			objectName,	usages
		},
		(* Get the name of the symbol, and the usage cell. *)
		objectName	= Cases[nbfile, Cell[name_,  "ObjectName", ___] :> name, {0,Infinity}, Heads->True];
		usages 		= Cases[nbfile, Cell[usage_, "Usage", ___] :> usage,     {0,Infinity}, Heads->True];
		
		(* Return *)
		If[ Length /@ {objectName, usages} =!= {1,1},
			{Last@FileNameSplit[sourceNB], Null},
			First /@ {objectName, usages}
		]
	];


ParseUsageMessage[{symbol_,usage_}] := 
	Module[
		{
			parsed = RowBox@@usage
		},
		(* Remove line separators after inputs. *)
		parsed = parsed //. 
				{before___, Cell[BoxData[contents_],"InlineFormula"], desc_String, after___} /; !FreeQ[contents,symbol]
			:>	{before, contents, TrimLineSeparator@desc, after};
		(* Remove remaining BoxData cells. *)
		parsed = parsed //. Cell[BoxData[content_],_]:>content;
		(* Clean up other stuff. *)
		parsed = parsed /. {
			(* Removes the identation. *)
			Cell[_,"ModInfo"] -> Sequence[],
			(* Removes buttons. *)
			ButtonBox[content_,__] :> content,
			(* Remove Deletable Styleboxes *)
			StyleBox[content_, Deletable -> _ ] :> content
		};
		(* Remove TraditionalForms because they don't print nice. *)
		(* We should handle the italics here a bit better, but doing it properly will 
		   be a lot of work, so we don't do it at all at the moment. *)
		parsed = parsed /. 
				FormBox[content_, TraditionalForm] 
			:> 	( content /. StyleBox[subcontent__, FontSlant -> "Italic"] :> subcontent );
		(* Flatten out RowBoxes. *)
		parsed = parsed //. RowBox[{before___,RowBox[{content___}], after___}] :> RowBox[{before,content,after}];
		(* Join consecutive StyleBoxes in RowBoxes. *)
		parsed = parsed //. 
				RowBox[{before___,StyleBox[string1_String, style__], StyleBox[string2_String, style__],after___}] 
			:> 	RowBox[{before,StyleBox[string1 <> string2, style], after}];
		(* Join consecutive strings in RowBoxes. *)
		parsed = parsed //. 
				RowBox[{before___,string1_String, string2_String,after___}]
			:>	RowBox[{before,string1<>string2,after}];
		(* Replace remaining newlines with line separators. *)
		parsed = parsed /. string_String :> NewLineReplace[string];
						
		(* Return *)
		{symbol, parsed}
	];


(* 
 *  Helper functions below. 
 *)

(* Trims \[LineSeparator] from the beginning of a string an replaces it with a space. *)
TrimLineSeparator[string_String] /; StringMatchQ[string, "\[LineSeparator]" ~~ ___] := 
	" " <> StringDrop[string,1];
TrimLineSeparator[string_String] := string;

(* Replaces all newline characters ("\n") with \[LineSeparator]. Returns a sequence. *)
NewLineReplace[string_String] /; StringMatchQ[string, ___ ~~ "\n"] := 
  Sequence[NewLineReplace@StringDrop[string, -1], "\[LineSeparator]"];
NewLineReplace[string_String] /; StringMatchQ[string, "\n" ~~ ___] := 
  Sequence["\[LineSeparator]", NewLineReplace@StringDrop[string, 1]];
NewLineReplace[string_String]  /; !StringFreeQ[string, "\n"] := 
  Sequence @@ Riffle[StringSplit[string, "\n"], "\[LineSeparator]"];
NewLineReplace[string_] := string;

(* Converts RowBox to a formatted string. *)
ConvertToString[RowBox[{strings___String}]]:=StringJoin[strings];
ConvertToString[boxes_]:=ToString[DisplayForm@boxes,StandardForm];


End[]
EndPackage[]