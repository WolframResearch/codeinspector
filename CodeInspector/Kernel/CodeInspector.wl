BeginPackage["CodeInspector`"]

(*
Functions
*)

CodeInspect

(*
CodeInspectBox exists because it is ambiguous whether or not the string "123" is supposed to
be interpreted as an integer or as a box
*)
CodeInspectBox

CodeInspectCST


CodeInspectAgg

CodeInspectAST



CodeInspectSummarize

CodeInspectBoxSummarize


AttachAnalysis

$IncludeMessageStacks


(*
Objects
*)

InspectionObject

InspectedStringObject
InspectedFileObject
InspectedBytesObject
InspectedLineObject

InspectedBoxObject
InspectedCellObject
InspectedNotebookObject


$ConcreteLintProgress
$ConcreteLintTime
$AggregateLintProgress
$AggregateLintTime
$AbstractLintProgress
$AbstractLintTime


(*
Messages
*)
CodeInspector


Begin["`Private`"]

Needs["CodeInspector`AbstractRules`"]
Needs["CodeInspector`AggregateRules`"]
Needs["CodeInspector`Boxes`"]
Needs["CodeInspector`ConcreteRules`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`LinterUI`"]
Needs["CodeInspector`MessageStack`"]
Needs["CodeInspector`Summarize`"]
Needs["CodeInspector`SuppressedRegions`"]
Needs["CodeInspector`TokenRules`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Abstract`"]
Needs["CodeParser`Scoping`"]
Needs["CodeParser`Utils`"]

Needs["PacletManager`"] (* for PacletInformation *)


(*
TODO: when targeting 12.1 as a minimum, then use paclet["AssetLocation", "BuiltInFunctions"]
*)
location = "Location" /. PacletInformation["CodeInspector"]

WolframLanguageSyntax`Generate`$analyzableMessagePositions = Get[FileNameJoin[{location, "Resources", "Data", "AnalyzableMessagePositions.wl"}]]
WolframLanguageSyntax`Generate`$analyzableMessages = Get[FileNameJoin[{location, "Resources", "Data", "AnalyzableMessages.wl"}]]
WolframLanguageSyntax`Generate`$badSymbols = Get[FileNameJoin[{location, "Resources", "Data", "BadSymbols.wl"}]]
WolframLanguageSyntax`Generate`$sessionSymbols = Get[FileNameJoin[{location, "Resources", "Data", "SessionSymbols.wl"}]]
WolframLanguageSyntax`Generate`$undocumentedSymbols = Get[FileNameJoin[{location, "Resources", "Data", "UndocumentedSymbols.wl"}]]



CodeInspector::old = "The old Lint paclet has been renamed to CodeInspector. Uninstall Lint paclet from your system."

If[PacletFind["Lint"] != {},
  Message[CodeInspector::old]
]


CodeInspector::versions1 = "CodeParser version `1` and CodeInspector version `2` are different. There may be unexpected problems. Evaluate PacletInstall /@ {\"CodeParser\", \"CodeInspector\"} to get the latest versions."

CodeInspector::versions2 = "CodeFormatter version `1` and CodeInspector version `2` are different. There may be unexpected problems. Evaluate PacletInstall /@ {\"CodeFormatter\", \"CodeInspector\"} to get the latest versions."

codeParserVersion = "Version" /. PacletInformation["CodeParser"]
codeFormatterVersion = "Version" /. PacletInformation["CodeFormatter"]
codeInspectorVersion = "Version" /. PacletInformation["CodeInspector"]

If[StringSplit[codeParserVersion, "."][[1;;2]] != StringSplit[codeInspectorVersion, "."][[1;;2]],
  Message[CodeInspector::versions1, codeParserVersion, codeInspectorVersion]
]

If[StringSplit[codeFormatterVersion, "."][[1;;2]] != StringSplit[codeInspectorVersion, "."][[1;;2]],
  Message[CodeInspector::versions2, codeFormatterVersion, codeInspectorVersion]
]



InspectionObject::usage = "InspectionObject[tag, description, severity, data] is a problem found in WL source code."

InspectionObject::warn = "\"Warn\" is being used as a severity, but it should be \"Warning\"."

InspectionObject[_, _, "Warn", _] /; (Message[InspectionObject::warn]; False) :=
  Null

(*
provide some selectors for Lint and LintedLine objects
*)

InspectionObject[tag_,     _,         _,     _]["Tag"] := tag
InspectionObject[   _, desc_,         _,     _]["Description"] := desc
InspectionObject[   _,     _, severity_,     _]["Severity"] := severity

InspectionObject[   _,     _,         _, data_]["AdditionalDescriptions"] := Lookup[data, "AdditionalDescriptions", {}]

(*
AdditionalDocumentationLinks is a list of:
{url, display}

where url is the link to be passed to SystemOpen or some such, and display is what will be displayed

*)
InspectionObject[   _,     _,         _, data_]["AdditionalDocumentationLinks"] := Lookup[data, "AdditionalDocumentationLinks", {}]



InspectedLineObject[_, lineNumber_, _,      _, ___]["LineNumber"] := lineNumber
InspectedLineObject[_,           _, _, lints_, ___]["Lints"] := lints






CodeInspect::usage = "CodeInspect[code] returns a list of problems found in code. \
code can be a string, a File, or a list of bytes."

Options[CodeInspect] = {
  "Editor" -> Automatic,
  PerformanceGoal -> "Speed",
  "TokenRules" :> $DefaultTokenRules,
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  "FileFormat" -> Automatic,
  (*
  filtering
  *)
  "TagExclusions" -> {},
  "SeverityExclusions" -> {},
  ConfidenceLevel -> 0.0,
  "LintLimit" -> Infinity,
  (*
  Pass through to CodeInspectCST
  *)
  "SuppressedRegions" -> {},
  "InheritedProperties" -> {},
  (*
  Pass through to CodeConcreteParse
  *)
  CharacterEncoding -> "UTF-8",
  SourceConvention -> "LineColumn",
  "TabWidth" -> 1
}


$fileByteCountMinLimit = 0*^6
$fileByteCountMaxLimit = 3*^6



CodeInspect[File[file_String], opts:OptionsPattern[]] :=
Catch[
Module[{performanceGoal, full, lints, cst, data,
  editor, suppressedRegions, fileFormat, ext, scanSessionTokens},

  performanceGoal = OptionValue[PerformanceGoal];

  editor = OptionValue["Editor"];
  fileFormat = OptionValue["FileFormat"];

  If[fileFormat === Automatic,
    ext = FileExtension[file];
    Switch[ext,
      "wl" | "m",
        fileFormat = "Package"
      ,
      "wls",
        fileFormat = "Script"
      ,
      _,
        fileFormat = "Unknown"
    ]
  ];

  $ConcreteLintProgress = 0;
  $AggregateLintProgress = 0;
  $AbstractLintProgress = 0;
  $ConcreteLintTime = Quantity[0, "Seconds"];
  $AggregateLintTime = Quantity[0, "Seconds"];
  $AbstractLintTime = Quantity[0, "Seconds"];

  full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <| "FileName" -> file |>]]
  ];

  Switch[fileFormat,
    "Script",
      scanSessionTokens = False
    ,
    _,
      scanSessionTokens = True
  ];

  If[performanceGoal == "Speed",
    If[FileByteCount[full] > $fileByteCountMaxLimit,
      Throw[Failure["FileTooLarge", <| "FileName" -> full, "FileSize" -> FileSize[full] |>]]
    ];
    If[FileByteCount[full] < $fileByteCountMinLimit,
      Throw[Failure["FileTooSmall", <| "FileName" -> full, "FileSize" -> FileSize[full] |>]]
    ];
  ];

  cst = CodeConcreteParse[File[full], FilterRules[{opts}, Options[CodeConcreteParse]]];

  If[FailureQ[cst],
    Throw[cst]
  ];

  suppressedRegions = SuppressedRegions[cst];

  Block[{$ScanSessionTokens = scanSessionTokens},
  lints = CodeInspectCST[cst, FilterRules[{opts}, Options[CodeInspectCST]], "SuppressedRegions" -> suppressedRegions];
  ];

  If[FailureQ[lints],
    Throw[lints]
  ];

  (*
  Add "File" to lints
  *)
  lints = Table[
    data = lint[[4]];
    data["File"] = full;
    data["Editor"] = editor;
    lint[[4]] = data;
    lint
    ,
    {lint, lints}
  ];

  lints
]]





CodeInspect[string_String, opts:OptionsPattern[]] :=
Catch[
Module[{suppressedRegions, cst, fileFormat, scanSessionTokens},

  fileFormat = OptionValue["FileFormat"];

  If[fileFormat === Automatic,
    fileFormat = "Unknown"
  ];

  $ConcreteLintProgress = 0;
  $AggregateLintProgress = 0;
  $AbstractLintProgress = 0;
  $ConcreteLintTime = Quantity[0, "Seconds"];
  $AggregateLintTime = Quantity[0, "Seconds"];
  $AbstractLintTime = Quantity[0, "Seconds"];

  Switch[fileFormat,
    "Script",
      scanSessionTokens = False
    ,
    _,
      scanSessionTokens = True
  ];

  cst = CodeConcreteParse[string, FilterRules[{opts}, Options[CodeConcreteParse]]];

  If[FailureQ[cst],
    Throw[cst]
  ];

  suppressedRegions = SuppressedRegions[cst];

  Block[{$ScanSessionTokens = scanSessionTokens},
  CodeInspectCST[cst, FilterRules[{opts}, Options[CodeInspectCST]], "SuppressedRegions" -> suppressedRegions]
  ]
]]



CodeInspect[bytes:{_Integer...}, opts:OptionsPattern[]] :=
Catch[
Module[{cst, suppressedRegions, fileFormat},

  fileFormat = OptionValue["FileFormat"];

  If[fileFormat === Automatic,
    fileFormat = "Unknown"
  ];
  
  $ConcreteLintProgress = 0;
  $AggregateLintProgress = 0;
  $AbstractLintProgress = 0;
  $ConcreteLintTime = Quantity[0, "Seconds"];
  $AggregateLintTime = Quantity[0, "Seconds"];
  $AbstractLintTime = Quantity[0, "Seconds"];

  Switch[fileFormat,
    "Script",
      scanSessionTokens = False
    ,
    _,
      scanSessionTokens = True
  ];
    
  cst = CodeConcreteParse[bytes, FilterRules[{opts}, Options[CodeConcreteParse]]];

  If[FailureQ[cst],
    Throw[cst]
  ];

  suppressedRegions = SuppressedRegions[cst];

  Block[{$ScanSessionTokens = scanSessionTokens},
  CodeInspectCST[cst, FilterRules[{opts}, Options[CodeInspectCST]], "SuppressedRegions" -> suppressedRegions]
  ]
]]



Options[CodeInspectCST] = {
  PerformanceGoal -> "Speed",
  "TokenRules" :> $DefaultTokenRules,
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  "SuppressedRegions" -> {},
  "BatchMode" -> True,
  "KeepLowlevelScopingLints" -> True,
  (*
  Properties to pass from cst to lints
  *)
  "InheritedProperties" -> {},
  (*
  filtering
  *)
  "TagExclusions" -> {},
  "SeverityExclusions" -> {},
  ConfidenceLevel -> 0.0,
  "LintLimit" -> Infinity
}

Attributes[CodeInspectCST] = {HoldFirst}

CodeInspectCST[cstIn_, opts:OptionsPattern[]] :=
Catch[
Module[{cst, data, agg, aggregateRules, abstractRules, ast, poss, lints,
  prog, concreteRules, performanceGoal, start,
  scopingData, scopingLints, suppressedRegions, tokenRules, isActive, inheritedProperties, batchMode, keepLowlevelScopingLints,
  tagExclusions, severityExclusions, confidence, lintLimit,
  stacks},

  If[$Debug,
    Print["CodeInspectCST"];
  ];

  cst = cstIn;
  data = cst[[3]];

  lints = {};

  (*
  not used, but may possibly be used in future
  *)
  performanceGoal = OptionValue[PerformanceGoal];

  tokenRules = OptionValue["TokenRules"];
  concreteRules = OptionValue["ConcreteRules"];
  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];
  suppressedRegions = OptionValue["SuppressedRegions"];
  inheritedProperties = OptionValue["InheritedProperties"];
  batchMode = OptionValue["BatchMode"];
  keepLowlevelScopingLints = OptionValue["KeepLowlevelScopingLints"];

  tagExclusions = OptionValue["TagExclusions"];
  severityExclusions = OptionValue["SeverityExclusions"];
  confidence = OptionValue[ConfidenceLevel];
  lintLimit = OptionValue["LintLimit"];

  If[$Debug,
    Print["suppressedRegions: ", suppressedRegions]
  ];

  isActive = makeIsActiveFunc[suppressedRegions];

  If[FailureQ[cst],
    Throw[cst]
  ];

  If[empty[tokenRules] && empty[concreteRules] && empty[aggregateRules] && empty[abstractRules],

    lints = Flatten[lints];

    lints = insertInheritedProperties[#, data, inheritedProperties]& /@ lints;

    lints = Select[lints, isActive];

    lints = filterLints[lints, tagExclusions, severityExclusions, confidence, lintLimit];

    Throw[lints]
  ];


  If[$Debug,
    Print["tokenRules"];
  ];

  
  KeyValueMap[Function[{pat, func},
    If[$Debug,
      Print[pat];
    ];
    poss = Position[cst, pat];
    AppendTo[lints, Map[Function[pos, func[pos, cst]], poss]];
    ], tokenRules];


  If[empty[concreteRules] && empty[aggregateRules] && empty[abstractRules],

    lints = Flatten[lints];

    lints = insertInheritedProperties[#, data, inheritedProperties]& /@ lints;

    lints = Select[lints, isActive];

    lints = filterLints[lints, tagExclusions, severityExclusions, confidence, lintLimit];

    Throw[lints]
  ];


  If[$Debug,
    Print["concreteRules"];
  ];

  prog = 0;
  start = Now;
  KeyValueMap[Function[{pat, func},
    If[$Debug,
      Print[pat];
    ];
    poss = Position[cst, pat];
    AppendTo[lints, Map[Function[pos, func[pos, cst]], poss]];
    prog++;
    $ConcreteLintProgress = Floor[100 * prog / Length[concreteRules]];
    ], concreteRules];
  $ConcreteLintTime = Now - start;


  If[empty[aggregateRules] && empty[abstractRules],

    lints = Flatten[lints];

    lints = insertInheritedProperties[#, data, inheritedProperties]& /@ lints;

    lints = Select[lints, isActive];

    lints = filterLints[lints, tagExclusions, severityExclusions, confidence, lintLimit];

    Throw[lints]
  ];

  agg = Aggregate[cst];

  cst =.;

  If[FailureQ[agg],
    Throw[agg]
  ];

  If[$Debug,
    Print["agg: ", agg];
  ];

  If[$Debug,
    Print["aggregateRules"];
  ];

  prog = 0;
  start = Now;
  KeyValueMap[Function[{pat, func},
    If[$Debug,
      Print[pat];
    ];
    poss = Position[agg, pat];
    AppendTo[lints, Map[Function[pos, func[pos, agg]], poss]];
    prog++;
    $AggregateLintProgress = Floor[100 * prog / Length[aggregateRules]];
    ], aggregateRules];
  $AggregateLintTime = Now - start;


  If[empty[abstractRules],

    lints = Flatten[lints];

    lints = insertInheritedProperties[#, data, inheritedProperties]& /@ lints;

    lints = Select[lints, isActive];

    lints = filterLints[lints, tagExclusions, severityExclusions, confidence, lintLimit];

    Throw[lints]
  ];

  ast = Abstract[agg, "BatchMode" -> batchMode];

  agg =.;

  If[FailureQ[ast],
    Throw[ast]
  ];

  If[$Debug,
    Print["ast: ", ast];
  ];

  If[$Debug,
    Print["abstractRules"];
  ];

  prog = 0;
  start = Now;
  KeyValueMap[Function[{pat, func},
    If[$Debug,
      Print[pat];
    ];
    poss = Position[ast, pat];
    AppendTo[lints, Map[Function[pos, func[pos, ast]], poss]];
    prog++;
    $AbstractLintProgress = Floor[100 * prog / Length[abstractRules]];
    ], abstractRules];
  $AbstractLintTime = Now - start;

  
  (*
  scoping data
  If there are any Abstract rules, then also doing scoping rules
  *)
  scopingData = ScopingData[ast];

  scopingLints = scopingDataObjectToLints /@ scopingData;

  scopingLints = Flatten[scopingLints];

  If[!keepLowlevelScopingLints,
    (*
    remove low-level scoping lints such as:
    unused parameter

    These would be better displayed with syntax highlighting.

    But the FE does not have syntax highlighting for this, so remove for now.


    Obviously keep errors

    And also keep "unused variables"
    *)
    scopingLints =
      Cases[scopingLints,
        InspectionObject[_, _, "Warning" | "Error" | "Fatal", _] |
          InspectionObject["UnusedVariable", _, "Scoping", _]];
  ];

  lints = lints ~Join~ scopingLints;


  If[$IncludeMessageStacks,

    (*
    assume this is running after the user clicked "Analyze input for issues" after an evaluation and use $Line - 1
    *)

    stacks =
      With[{line = $Line - 1, session = $SessionID},
        ReleaseHold /@ Cases[DownValues[MessageMenu`MessageStackList][[All, 1]], Verbatim[HoldPattern][HoldPattern[MessageMenu`MessageStackList[line, _, session]]]]
      ];

    lints = lints ~Join~ Flatten[CodeInspector`MessageStack`codeWithMessageStackInspectAST[ast, #]& /@ stacks];
  ];


  lints = Flatten[lints];

  lints = insertInheritedProperties[#, data, inheritedProperties]& /@ lints;

  lints = Select[lints, isActive];

  lints = filterLints[lints, tagExclusions, severityExclusions, confidence, lintLimit];

  lints
]]



Options[CodeInspectAgg] = {
  PerformanceGoal -> "Speed",
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  "SuppressedRegions" -> {},
  "BatchMode" -> True,
  (*
  Properties to pass from cst to lints
  *)
  "InheritedProperties" -> {},
  (*
  filtering
  *)
  "TagExclusions" -> {},
  "SeverityExclusions" -> {},
  ConfidenceLevel -> 0.0,
  "LintLimit" -> Infinity
}

Attributes[CodeInspectAgg] = {HoldFirst}

CodeInspectAgg[aggIn_, OptionsPattern[]] :=
Catch[
Module[{agg, data, aggregateRules, abstractRules, ast, poss, lints,
  prog, start, suppressedRegions, isActive, inheritedProperties, batchMode,
  tagExclusions, severityExclusions, confidence, lintLimit},

  If[$Debug,
    Print["CodeInspectAgg"];
  ];

  agg = aggIn;
  data = agg[[3]];

  lints = {};

  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];
  suppressedRegions = OptionValue["SuppressedRegions"];
  inheritedProperties = OptionValue["InheritedProperties"];
  batchMode = OptionValue["BatchMode"];
  
  tagExclusions = OptionValue["TagExclusions"];
  severityExclusions = OptionValue["SeverityExclusions"];
  confidence = OptionValue[ConfidenceLevel];
  lintLimit = OptionValue["LintLimit"];

  If[$Debug,
    Print["suppressedRegions: ", suppressedRegions]
  ];

  isActive = makeIsActiveFunc[suppressedRegions];

  If[$Debug,
    Print["aggregateRules"];
  ];

  prog = 0;
  start = Now;
  KeyValueMap[Function[{pat, func},
    If[$Debug,
      Print[pat];
    ];
    poss = Position[agg, pat];
    AppendTo[lints, Map[Function[pos, func[pos, agg]], poss]];
    prog++;
    $AggregateLintProgress = Floor[100 * prog / Length[aggregateRules]];
    ], aggregateRules];
  $AggregateLintTime = Now - start;


  If[empty[abstractRules],

    lints = Flatten[lints];

    lints = insertInheritedProperties[#, data, inheritedProperties]& /@ lints;

    lints = Select[lints, isActive];

    lints = filterLints[lints, tagExclusions, severityExclusions, confidence, lintLimit];

    Throw[lints]
  ];

  ast = Abstract[agg, "BatchMode" -> batchMode];

  agg =.;

  If[FailureQ[ast],
    Throw[ast]
  ];

  If[$Debug,
    Print["ast: ", ast];
  ];

  If[$Debug,
    Print["abstractRules"];
  ];

  prog = 0;
  start = Now;
  KeyValueMap[Function[{pat, func},
    If[$Debug,
      Print[pat];
    ];
    poss = Position[ast, pat];
    AppendTo[lints, Map[Function[pos, func[pos, ast]], poss]];
    prog++;
    $AbstractLintProgress = Floor[100 * prog / Length[abstractRules]];
    ], abstractRules];
  $AbstractLintTime = Now - start;

  lints = Flatten[lints];

  lints = insertInheritedProperties[#, data, inheritedProperties]& /@ lints;

  lints = Select[lints, isActive];

  lints = filterLints[lints, tagExclusions, severityExclusions, confidence, lintLimit];

  lints
]]



Options[CodeInspectAST] = {
  PerformanceGoal -> "Speed",
  "AbstractRules" :> $DefaultAbstractRules,
  "SuppressedRegions" -> {},
  (*
  Properties to pass from cst to lints
  *)
  "InheritedProperties" -> {},
  (*
  filtering
  *)
  "TagExclusions" -> {},
  "SeverityExclusions" -> {},
  ConfidenceLevel -> 0.0,
  "LintLimit" -> Infinity
}

Attributes[CodeInspectAST] = {HoldFirst}

CodeInspectAST[astIn_, OptionsPattern[]] :=
Catch[
Module[{abstractRules, ast, poss, lints,
  prog, start, suppressedRegions, isActive, inheritedProperties, data,
  tagExclusions, severityExclusions, confidence, lintLimit},

  If[$Debug,
    Print["CodeInspectAST"];
  ];

  ast = astIn;
  data = ast[[3]];

  lints = {};

  abstractRules = OptionValue["AbstractRules"];
  suppressedRegions = OptionValue["SuppressedRegions"];
  inheritedProperties = OptionValue["InheritedProperties"];

  tagExclusions = OptionValue["TagExclusions"];
  severityExclusions = OptionValue["SeverityExclusions"];
  confidence = OptionValue[ConfidenceLevel];
  lintLimit = OptionValue["LintLimit"];

  If[$Debug,
    Print["suppressedRegions: ", suppressedRegions]
  ];

  isActive = makeIsActiveFunc[suppressedRegions];

  If[$Debug,
    Print["abstractRules"];
  ];

  lints = {};

  prog = 0;
  start = Now;
  KeyValueMap[Function[{pat, func},
    If[$Debug,
      Print[pat];
    ];
    poss = Position[ast, pat];
    AppendTo[lints, Map[Function[pos, func[pos, ast]], poss]];
    prog++;
    $AbstractLintProgress = Floor[100 * prog / Length[abstractRules]];
    ], abstractRules];
  $AbstractLintTime = Now - start;

  lints = Flatten[lints];
  
  lints = insertInheritedProperties[#, data, inheritedProperties]& /@ lints;

  lints = Select[lints, isActive];

  lints = filterLints[lints, tagExclusions, severityExclusions, confidence, lintLimit];

  lints
]]


AttachAnalysis::usage = "AttachAnalysis[] attaches code analysis pods to the \"Input\" and \"Code\" cells in the evaluation notebook that contain issues.
AttachAnalysis[notebook] attaches code analysis pods to the \"Input\" and \"Code\" cells in notebook that contain issues.
AttachAnalysis[{cell1, cell2, ...}] attaches code analysis pods to the \"Input\" and \"Code\" cells in the list of cells that contain issues.";


AttachAnalysis::nofe = "No front end available.";


AttachAnalysis[
  HoldPattern[notebookOrCells_:EvaluationNotebook[]]
] /; MatchQ[notebookOrCells, _NotebookObject | {__CellObject}] := 
  If[TrueQ[$Notebooks],

    CodeInspector`LinterUI`Private`attachAnalysisAction[notebookOrCells],

    (* If $Notebooks isn't True, then a suitable front end isn't available. Throw a message saying this and return $Failed. *)
    Message[AttachAnalysis::nofe]; $Failed
  ]


(*
returns a function lint -> True|False
*)
makeIsActiveFunc[suppressedRegions_] :=
  Function[{lint},
    AllTrue[suppressedRegions,
      Function[{region},
        !SourceMemberQ[region[[1;;2]], lint] ||
          AllTrue[region[[3]],
            Function[{suppressed}, isTagActive[lint, suppressed]]
          ]
      ]
    ]
  ]


isTagActive[InspectionObject[tag1_, _, _, KeyValuePattern["Argument" -> arg1_]], {tag2_, arg2_}] :=
  !(tag1 === tag2 && arg1 === arg2)

(*
The lint has an Argument, but there is no argument in the suppressed
*)
isTagActive[InspectionObject[_, _, _, KeyValuePattern["Argument" -> _]], {_}] :=
  True

isTagActive[InspectionObject[tag1_, _, _, _], {tag2_, _}] :=
  !(tag1 === tag2)

isTagActive[InspectionObject[tag1_, _, _, _], {tag2_}] :=
  !(tag1 === tag2)


insertInheritedProperties[o:InspectionObject[_, _, _, _], _Association, {}] :=
  o

insertInheritedProperties[InspectionObject[tag_, desc_, sev_, data1_Association], data_Association, inheritedProperties_] :=
  InspectionObject[tag, desc, sev, <| data1, KeyTake[data, inheritedProperties] |>]


End[]

EndPackage[]
