(* full messages *)
$MessagePrePrint = .;
SetDirectory[AntProperty["basedir"]];   
AntLog["_"];

(* Configure Front End *)
If[ AntProperty["frontEndLaunchFlags"] =!= Null,
    SetOptions[ 
        Developer`InstallFrontEnd, 
        Developer`LaunchFlags -> AntProperty["frontEndLaunchFlags"]
    ]
];

(* Add apps to $Path*)
    If[ !MemberQ[$Path, AntProperty["appPath"]],
    PrependTo[$Path, AntProperty["appPath"]]
]; 

outputDir   = AntProperty["outputDirNB"];
language    = AntProperty["language"];

Needs["DocumentationBuild`"];   

(* Create Index directory *)
indexDir = If[ Position[FileNameSplit[outputDir], language] === {},
    ToFileName[{outputDir, language, "Index"}]
,
    ToFileName[{
        FileNameTake[
            outputDir, 
            {1, Position[FileNameSplit[outputDir], language][[1, 1]]}
        ], 
        "Index"
    }]
];
If[ FileType@indexDir === Directory,
    AntLog["Deleting previous Index directory."];
    DeleteDirectory[indexDir, DeleteContents -> True];
];
AntLog["Creating Index directory."];
CreateDirectory[indexDir];

If[ FileType@indexDir === Directory,
    (* Open path to Indexer *)
    Needs["DocumentationSearch`"];
    indexer = DocumentationSearch`NewDocumentationNotebookIndexer[indexDir];
    If[Head@indexer =!= DocumentationSearch`DocumentationNotebookIndexer,
        AntLog["Creation of NotebookIndexer Failed!"];
    ];
    (* Create Spelling Index directory *)
    indexSpellDir = ToFileName[{DirectoryName[indexDir]}, "SpellIndex"];
    If[ FileType@indexSpellDir === Directory,
        AntLog["Deleting previous Spelling Index directory."];
        DeleteDirectory[indexSpellDir, DeleteContents -> True];
    ];
    AntLog["Creating Spelling Index directory."];
    CreateDirectory[indexSpellDir];
,
    AntLog["Creation of Index directory Failed! No index will be created."];
    Quit[];
];

(* Find all notebooks in the output dir. *)
notebooks = FileNames["*.nb", {outputDir}, Infinity];
AntLog["Found " <> ToString@Length@notebooks <> " notebooks."];

(* Index every notebook found. *)
IndexNotebook[notebook_] := Module[
    {
        plainText   = Import[notebook, {"NB", "Plaintext"}],
        metaData    = DocumentationBuild`Utils`GetSearchMetaDataList[notebook]
    },
    (* add text to index *)
    If[(Head@plainText === String) && (Head@metaData === List),
        DocumentationSearch`AddDocumentationNotebook[
            indexer,
            plainText, 
            metaData
        ];
        AntLog["Indexed notebook " <> FileNameTake[notebook] <> "."];
    ,
        AntLog["Skipped notebook " <> FileNameTake[notebook] <> "."];
    ]
];
IndexNotebook /@ notebooks;

(* Close the indices. *)
AntLog["Closing Index"];
res = DocumentationSearch`CloseDocumentationNotebookIndexer[indexer];
AntLog["Index closed. " <> ToString @ If[res =!= Null, res, " "] ];

AntLog["Creating Spelling Index"];
res = DocumentationSearch`CreateSpellIndex[indexDir, indexSpellDir];
AntLog["Spelling Index closed. " <> ToString @ If[res =!= Null, res, " "] ];(* Mathematica package *)