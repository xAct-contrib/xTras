AntLog[$Version];

(* Start Front End *)
Developer`InstallFrontEnd[];

(* Get all .nb files from build directory *)
outputDir = AntProperty["dist"];
files = FileNames["*.nb", outputDir, Infinity];

AntLog["Running cross-version replacements on "<>ToString@Length@files<>" files in "<>outputDir];

(* Function to discriminate between versions 6--8 and 9 *)
IfPreVer9[pre_, post_] := FEPrivate`If[             
   FEPrivate`Or[FEPrivate`SameQ[FEPrivate`$ProductVersion, "6.0"],
                FEPrivate`SameQ[FEPrivate`$ProductVersion, "7.0"],
                FEPrivate`SameQ[FEPrivate`$ProductVersion, "8.0"]], 
   pre, post];

(* Section spacer that displays as very thin cell in versions 6--8 *)
mySectionSpacer[sty_] = Cell["", sty,
        CellSize -> IfPreVer9[{Inherited, 1}, Inherited],
        CellElementSpacings -> {CellMinHeight -> 
      IfPreVer9[1, Inherited]},
        CellMargins -> IfPreVer9[0, Inherited],
        Editable -> False, Selectable -> False, Deletable -> False, 
   ShowCellBracket -> False, ShowSelection -> False];

(* List of styles for section headings that we will do replacements on *)
headingStyles = {
      "GuideMoreAboutSection", 
       "GuideTutorialsSection", "MoreAboutSection", 
       "MoreInformationSection",
       "PrimaryExamplesSection", "RelatedDemonstrationsSection", 
       "RelatedLinksSection", "SeeAlsoSection", 
       "TutorialsSection", 
       "RelatedTutorialsSection", "TutorialMoreAboutSection",
       "TutorialRelatedLinksSection", "NotesSection", 
         "GuideRelatedLinksSection"
  };

(* Utility functions*)
myspacer = Cell[BoxData[ToBoxes[Spacer[24]]]];

toTitleCase[str_String] := 
  StringReplace[ToLowerCase[str], 
   WordBoundary ~~ x_ :> ToUpperCase[x]];

framelabelopts = {CellSize -> {5000, Inherited}};

cellopts = {"WholeCellGroupOpener" -> True, 
   CellFrameLabelMargins -> 0, 
   CellElementSpacings -> {"CellMinHeight" -> 3}, 
   CellSize -> {Inherited, IfPreVer9[11, 14]}};

(* Loop over files *)
Do[
  AntLog["Processing " <> FileNameTake[file] <> "."];
  (* Importing nb files containing Manipulate objects can throw a bunch of benign newline interpretation warnings *)
  Quiet[expr = Get[file];, Syntax::newl];

  (* Convert section headings to Title Case in version 9 *)
    expr = expr /. {
    (*"MORE INFORMATION" section-- get rid of frame*)
    Cell[con_, sty : "NotesSection", o___] :> 
     Cell["", sty, Sequence @@ cellopts, o, CellFrameLabels -> {{
         IfPreVer9[
          Cell[con, sty, Sequence @@ framelabelopts], 
          con /. TextData[Cell[BoxData[box : ButtonBox[__]]]] :> TextData[box] /. 
           box_FrameBox -> 
            Cell[TextData[{myspacer, "Details and Options"}], 
             "NotesSection", Sequence @@ framelabelopts]
          ], None}, {None, None}}],
    (*Guide Tutorials section-- need to mimic standard version 9 section style*)
    Cell[con_, sty : "GuideTutorialsSection", o___] :> 
     Cell["", sty, Sequence @@ cellopts, o, 
      CellMargins -> 
       IfPreVer9[Inherited, {{Inherited, Inherited}, {Inherited, 20}}], 
      CellFrameLabels -> {{
         IfPreVer9[
          Cell[con, sty, Sequence @@ framelabelopts], 
          con /. str_String -> 
            Cell[TextData[{myspacer, toTitleCase[str]}], sty, 
             Sequence @@ framelabelopts]
          ], None}, {None, None}}],
    (*Examples section-- add placeholder for total example count*)
    Cell[con_, sty : "PrimaryExamplesSection", o___] :> 
     Cell["", sty, o, Sequence @@ cellopts, CellFrameLabels -> {{
         IfPreVer9[
          Cell[con, sty, Sequence @@ framelabelopts], 
          con /. (ButtonBox[str_String, bbo___] :> 
             ButtonBox[
              Cell[TextData[{myspacer, toTitleCase[str], "  ", 
                 "InsertExampleCount"}], sty, 
               Sequence @@ framelabelopts], bbo])
          ], None}, {None, None}}],
    (*All other section headings with text only in the title*)
    Cell[con_String, sty : Alternatives @@ headingStyles, o___] :> 
     Cell["", sty, Sequence @@ cellopts, o, CellFrameLabels -> {{
         IfPreVer9[
          Cell[con, sty, Sequence @@ framelabelopts],
          Cell[TextData[{myspacer, toTitleCase[con]}], sty, 
           Sequence @@ framelabelopts]
          ], None}, {None, None}}],
    (*All other section headings with buttons in the title*)
    Cell[con_, sty : Alternatives @@ headingStyles, o___] :> 
     Cell["", sty, Sequence @@ cellopts, o, CellFrameLabels -> {{
         IfPreVer9[
          Cell[con, sty, Sequence @@ framelabelopts], 
          con /. (ButtonBox[str_String, bbo___] :> 
             ButtonBox[
              Cell[TextData[{myspacer, toTitleCase[str]}], sty, 
               Sequence @@ framelabelopts], bbo])
          ], None}, {None, None}}]
  };

  (* Replace "Details and Options" with "Details" if there is no mention of options in the notes section *)
  expr = expr /. 
     notescell : CellGroupData[{Cell["", "NotesSection", ___], ___}, ___] :> 
     (notescell /. If[
       Count[notescell, str_String /; ! StringFreeQ[str, {"option", "Option"}], Infinity] > 1, 
           {}, 
       "Details and Options" -> "Details"
      ]);

(* Add total example count to Examples section heading in version 9 *)
expr = expr /. (examplegroup : Cell[CellGroupData[{Cell[_, "PrimaryExamplesSection", ___], ___}, ___], ___]) :>
  (examplegroup /. "InsertExampleCount" -> 
    Cell["(" <> 
      ToString[Total@Cases[examplegroup, Cell[countstring_, "ExampleCount"] :> ToExpression[countstring], Infinity]] <> 
      ")", "ExampleCount"]
   );

(* Add spacers before and after section content in version 9 *)
expr = expr /. {Cell[
  CellGroupData[{c1 : Cell[_, "SeeAlsoSection", ___], c2__}, o2___], o3___] :> 
    Cell[CellGroupData[{c1, c2}, o2], o3], 
  Cell[CellGroupData[{c1 : Cell[_, "PrimaryExamplesSection", ___], c2__}, o2___], o3___] :> 
    Cell[CellGroupData[{c1, c2, mySectionSpacer["SectionFooterSpacer"]}, o2], o3],                  
  Cell[CellGroupData[{c1 : Cell[_, Alternatives @@ headingStyles, ___], c2__}, o2___], o3___] :> 
    Cell[CellGroupData[{c1, mySectionSpacer["SectionHeaderSpacer"], c2, mySectionSpacer["SectionFooterSpacer"]}, o2], o3]};


  (*Fix button behavior in Example subsections*)
  expr = expr /. 
    Cell[con__, sty : "ExampleSection" | "ExampleSubsection", o___] :> 
      Cell[con, sty, "WholeCellGroupOpener" -> True, o];        

(*Fix rendering bug pre-version 9 *)
expr = expr /. 
  Cell[c_, "GuideAbstract", o___] :> 
    Cell[c, "GuideAbstract", CellFrame -> IfPreVer9[{{0, 0}, {1, 0}}, Inherited], o];

(* Improve font appearance for tutorial links in Guide pages, and mimic section heading style for Guide Tutorial heading *)
expr = expr /. {"GuideTutorial" -> "GuideMoreAbout", "GuideTutorialsSection" -> "GuideMoreAboutSection"};

(* Save notebook using Front End *)
Developer`UseFrontEnd[NotebookSave[NotebookPut[expr], file]],

{file,files}];