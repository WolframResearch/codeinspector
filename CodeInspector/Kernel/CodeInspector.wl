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
Needs["CodeInspector`Summarize`"]
Needs["CodeInspector`SuppressedRegions`"]
Needs["CodeInspector`TokenRules`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Abstract`"]
Needs["CodeParser`Scoping`"]
Needs["CodeParser`Utils`"]

Needs["PacletManager`"] (* for PacletInformation *)



CodeInspector::old = "The old Lint paclet has been renamed to CodeInspector. Uninstall Lint paclet from your system."

If[PacletFind["Lint"] != {},
  Message[CodeInspector::old]
]


CodeInspector::versions = "CodeParser version `1` and CodeInspector version `2` are different. There may be unexpected problems."

codeParserVersion = "Version" /. PacletInformation["CodeParser"]
codeInspectorVersion = "Version" /. PacletInformation["CodeInspector"]
If[StringSplit[codeParserVersion, "."][[1;;2]] != StringSplit[codeInspectorVersion, "."][[1;;2]],
  Message[CodeInspector::versions, codeParserVersion, codeInspectorVersion]
]



InspectionObject::usage = "InspectionObject[tag, description, severity, data] is a problem found in WL source code."

(*
provide some selectors for Lint and LintedLine objects
*)

InspectionObject[tag_,     _,         _, _]["Tag"] := tag
InspectionObject[   _, desc_,         _, _]["Description"] := desc
InspectionObject[   _,     _, severity_, _]["Severity"] := severity



InspectedLineObject[_, lineNumber_, _,      _, ___]["LineNumber"] := lineNumber
InspectedLineObject[_,           _, _, lints_, ___]["Lints"] := lints






CodeInspect::usage = "CodeInspect[code] returns a list of problems found in code. \
code can be a string, a file, or a list of bytes."

Options[CodeInspect] = {
  PerformanceGoal -> "Speed",

  "TokenRules" :> $DefaultTokenRules,
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  
  "Editor" -> Automatic,

  CharacterEncoding -> "UTF-8",
  
  (*
  Pass through to CodeConcreteParse
  *)
  "TabWidth" -> ("TabWidth" /. Options[CodeConcreteParse])
}


$fileByteCountMinLimit = 0*^6
$fileByteCountMaxLimit = 3*^6



CodeInspect[File[file_String], opts:OptionsPattern[]] :=
Catch[
Module[{performanceGoal, aggregateRules, abstractRules, encoding, full, lints, cst, data, concreteRules,
  editor, suppressedRegions, tokenRules},

  performanceGoal = OptionValue[PerformanceGoal];
  tokenRules = OptionValue["TokenRules"];
  concreteRules = OptionValue["ConcreteRules"];
  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];

  editor = OptionValue["Editor"];

  $ConcreteLintProgress = 0;
  $AggregateLintProgress = 0;
  $AbstractLintProgress = 0;
  $ConcreteLintTime = Quantity[0, "Seconds"];
  $AggregateLintTime = Quantity[0, "Seconds"];
  $AbstractLintTime = Quantity[0, "Seconds"];

  encoding = OptionValue[CharacterEncoding];
  If[encoding =!= "UTF-8",
    Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
  ];

  full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <|"FileName"->file|>]]
  ];

  If[performanceGoal == "Speed",
    If[FileByteCount[full] > $fileByteCountMaxLimit,
      Throw[Failure["FileTooLarge", <|"FileName"->full, "FileSize"->FileSize[full]|>]]
    ];
    If[FileByteCount[full] < $fileByteCountMinLimit,
      Throw[Failure["FileTooSmall", <|"FileName"->full, "FileSize"->FileSize[full]|>]]
    ];
  ];

  cst = CodeConcreteParse[File[full], FilterRules[{opts}, Options[CodeConcreteParse]]];

  If[FailureQ[cst],
    Throw[cst]
  ];

  suppressedRegions = SuppressedRegions[cst];

  lints = CodeInspectCST[
    cst,
    PerformanceGoal -> performanceGoal,
    "TokenRules" -> tokenRules,
    "ConcreteRules" -> concreteRules,
    "AggregateRules" -> aggregateRules,
    "AbstractRules" -> abstractRules,
    "SuppressedRegions" -> suppressedRegions
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
 Module[{aggregateRules, abstractRules, cst, concreteRules, performanceGoal, suppressedRegions, tokenRules},

  performanceGoal = OptionValue[PerformanceGoal];
  tokenRules = OptionValue["TokenRules"];
  concreteRules = OptionValue["ConcreteRules"];
  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];

  $ConcreteLintProgress = 0;
  $AggregateLintProgress = 0;
  $AbstractLintProgress = 0;
  $ConcreteLintTime = Quantity[0, "Seconds"];
  $AggregateLintTime = Quantity[0, "Seconds"];
  $AbstractLintTime = Quantity[0, "Seconds"];

  cst = CodeConcreteParse[string, FilterRules[{opts}, Options[CodeConcreteParse]]];

  If[FailureQ[cst],
    Throw[cst]
  ];

  suppressedRegions = SuppressedRegions[cst];

  CodeInspectCST[
    cst,
    PerformanceGoal -> performanceGoal,
    "TokenRules" -> tokenRules,
    "ConcreteRules" -> concreteRules,
    "AggregateRules" -> aggregateRules,
    "AbstractRules" -> abstractRules,
    "SuppressedRegions" -> suppressedRegions
  ]
]]



CodeInspect[bytes_List, opts:OptionsPattern[]] :=
Catch[
 Module[{aggregateRules, abstractRules, cst, concreteRules, performanceGoal, suppressedRegions, tokenRules},

  performanceGoal = OptionValue[PerformanceGoal];
  tokenRules = OptionValue["TokenRules"];
  concreteRules = OptionValue["ConcreteRules"];
  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];

  $ConcreteLintProgress = 0;
  $AggregateLintProgress = 0;
  $AbstractLintProgress = 0;
  $ConcreteLintTime = Quantity[0, "Seconds"];
  $AggregateLintTime = Quantity[0, "Seconds"];
  $AbstractLintTime = Quantity[0, "Seconds"];

  cst = CodeConcreteParse[bytes, FilterRules[{opts}, Options[CodeConcreteParse]]];

  If[FailureQ[cst],
    Throw[cst]
  ];

  suppressedRegions = SuppressedRegions[cst];

  CodeInspectCST[
    cst,
    PerformanceGoal -> performanceGoal,
    "TokenRules" -> tokenRules,
    "ConcreteRules" -> concreteRules,
    "AggregateRules" -> aggregateRules,
    "AbstractRules" -> abstractRules,
    "SuppressedRegions" -> suppressedRegions
  ]
]]



Options[CodeInspectCST] = {
  PerformanceGoal -> "Speed",
  "TokenRules" :> $DefaultTokenRules,
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  "SuppressedRegions" -> {}
}

Attributes[CodeInspectCST] = {HoldFirst}

CodeInspectCST[cstIn_, OptionsPattern[]] :=
Catch[
Module[{cst, agg, aggregateRules, abstractRules, ast, poss, lints,
  prog, concreteRules, performanceGoal, start,
  scopingData, scopingLints, suppressedRegions, tokenRules},

  If[$Debug,
    Print["CodeInspectCST"];
  ];

  cst = cstIn;

  lints = {};

  performanceGoal = OptionValue[PerformanceGoal];
  tokenRules = OptionValue["TokenRules"];
  concreteRules = OptionValue["ConcreteRules"];
  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];
  suppressedRegions = OptionValue["SuppressedRegions"];

  If[$Debug,
    Print["suppressedRegions: ", suppressedRegions]
  ];

  If[FailureQ[cst],
    Throw[cst]
  ];

  If[empty[tokenRules] && empty[concreteRules] && empty[aggregateRules] && empty[abstractRules],

    lints = Flatten[lints];

    lints = Select[lints,
      Function[{lint},
        AllTrue[suppressedRegions,
          Function[{region},
            AllTrue[region[[3]],
              Function[{suppressed},
                !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || isEnabled[lint, suppressed]
              ]
            ]
          ]
        ]
      ]
    ];

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

    lints = Select[lints,
      Function[{lint},
        AllTrue[suppressedRegions,
          Function[{region},
            AllTrue[region[[3]],
              Function[{suppressed},
                !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || isEnabled[lint, suppressed]
              ]
            ]
          ]
        ]
      ]
    ];

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

    lints = Select[lints,
      Function[{lint},
        AllTrue[suppressedRegions,
          Function[{region},
            AllTrue[region[[3]],
              Function[{suppressed},
                !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || isEnabled[lint, suppressed]
              ]
            ]
          ]
        ]
      ]
    ];

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

    lints = Select[lints,
      Function[{lint},
        AllTrue[suppressedRegions,
          Function[{region},
            AllTrue[region[[3]],
              Function[{suppressed},
                !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || isEnabled[lint, suppressed]
              ]
            ]
          ]
        ]
      ]
    ];

    Throw[lints]
  ];

  ast = Abstract[agg];

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

  lints = lints ~Join~ scopingLints;


  lints = Flatten[lints];

  lints = Select[lints,
    Function[{lint},
      AllTrue[suppressedRegions,
        Function[{region},
          AllTrue[region[[3]],
            Function[{suppressed},
              !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || isEnabled[lint, suppressed]
            ]
          ]
        ]
      ]
    ]
  ];

  lints
]]



Options[CodeInspectAgg] = {
  PerformanceGoal -> "Speed",
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  "SuppressedRegions" -> {}
}

Attributes[CodeInspectAgg] = {HoldFirst}

CodeInspectAgg[aggIn_, OptionsPattern[]] :=
Catch[
Module[{agg, aggregateRules, abstractRules, ast, poss, lints,
  prog, performanceGoal, start, suppressedRegions},

  If[$Debug,
    Print["CodeInspectAgg"];
  ];

  agg = aggIn;

  lints = {};

  performanceGoal = OptionValue[PerformanceGoal];
  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];
  suppressedRegions = OptionValue["SuppressedRegions"];

  If[$Debug,
    Print["suppressedRegions: ", suppressedRegions]
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

    lints = Select[lints,
      Function[{lint},
        AllTrue[suppressedRegions,
          Function[{region},
            AllTrue[region[[3]],
              Function[{suppressed},
                !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || isEnabled[lint, suppressed]
              ]
            ]
          ]
        ]
      ]
    ];

    Throw[lints]
  ];

  ast = Abstract[agg];

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

  lints = Select[lints,
    Function[{lint},
      AllTrue[suppressedRegions,
        Function[{region},
          AllTrue[region[[3]],
            Function[{suppressed},
              !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || isEnabled[lint, suppressed]
            ]
          ]
        ]
      ]
    ]
  ];

  lints
]]



Options[CodeInspectAST] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  "SuppressedRegions" -> {}
}

Attributes[CodeInspectAST] = {HoldFirst}

CodeInspectAST[astIn_, OptionsPattern[]] :=
Catch[
Module[{abstractRules, ast, poss, lints,
  prog, performanceGoal, start, suppressedRegions},

  If[$Debug,
    Print["CodeInspectAST"];
  ];

  ast = astIn;

  lints = {};

  performanceGoal = OptionValue[PerformanceGoal];
  abstractRules = OptionValue["AbstractRules"];
  suppressedRegions = OptionValue["SuppressedRegions"];

  If[$Debug,
    Print["suppressedRegions: ", suppressedRegions]
  ];

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

  lints = Select[lints,
    Function[{lint},
      AllTrue[suppressedRegions,
        Function[{region},
          AllTrue[region[[3]],
            Function[{suppressed},
              !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || isActive[lint, suppressed]
            ]
          ]
        ]
      ]
    ]
  ];
  
  lints
]]



isActive[InspectionObject[tag1_, _, _, KeyValuePattern["Argument" -> arg1_]], {tag2_, arg2_}] :=
  !(tag1 === tag2 && arg1 === arg2)

(*
The lint has an Argument, but there is no argument in the suppressed
*)
isActive[InspectionObject[_, _, _, KeyValuePattern["Argument" -> _]], {_}] :=
  True

isActive[InspectionObject[tag1_, _, _, _], {tag2_, _}] :=
  !(tag1 === tag2)

isActive[InspectionObject[tag1_, _, _, _], {tag2_}] :=
  !(tag1 === tag2)


End[]

EndPackage[]
