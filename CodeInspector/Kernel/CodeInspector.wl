BeginPackage["CodeInspector`"]

(*
Functions
*)

CodeInspect

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
Needs["CodeInspector`DisabledRegions`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`Summarize`"]
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
  editor, disabledRegions},

  performanceGoal = OptionValue[PerformanceGoal];
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

  disabledRegions = DisabledRegions[cst];

  lints = CodeInspectCST[
    cst,
    PerformanceGoal -> performanceGoal,
    "ConcreteRules" -> concreteRules,
    "AggregateRules" -> aggregateRules,
    "AbstractRules" -> abstractRules,
    "DisabledRegions" -> disabledRegions
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
 Module[{aggregateRules, abstractRules, cst, concreteRules, performanceGoal, disabledRegions},

  performanceGoal = OptionValue[PerformanceGoal];
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

  disabledRegions = DisabledRegions[cst];

  CodeInspectCST[
    cst,
    PerformanceGoal -> performanceGoal,
    "ConcreteRules" -> concreteRules,
    "AggregateRules" -> aggregateRules,
    "AbstractRules" -> abstractRules,
    "DisabledRegions" -> disabledRegions
  ]
]]



CodeInspect[bytes_List, opts:OptionsPattern[]] :=
Catch[
 Module[{aggregateRules, abstractRules, cst, concreteRules, performanceGoal, disabledRegions},

  performanceGoal = OptionValue[PerformanceGoal];
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

  disabledRegions = DisabledRegions[cst];

  CodeInspectCST[
    cst,
    PerformanceGoal -> performanceGoal,
    "ConcreteRules" -> concreteRules,
    "AggregateRules" -> aggregateRules,
    "AbstractRules" -> abstractRules,
    "DisabledRegions" -> disabledRegions
  ]
]]


Options[CodeInspectBox] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules
}

CodeInspectBox[box_, OptionsPattern[]] :=
Catch[
 Module[{aggregateRules, abstractRules, cst, concreteRules, performanceGoal, disabledRegions},

  performanceGoal = OptionValue[PerformanceGoal];
  concreteRules = OptionValue["ConcreteRules"];
  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];

  $ConcreteLintProgress = 0;
  $AggregateLintProgress = 0;
  $AbstractLintProgress = 0;
  $ConcreteLintTime = Quantity[0, "Seconds"];
  $AggregateLintTime = Quantity[0, "Seconds"];
  $AbstractLintTime = Quantity[0, "Seconds"];

  cst = CodeConcreteParseBox[box];

  If[FailureQ[cst],
    Throw[cst]
  ];

  disabledRegions = DisabledRegions[cst];

  CodeInspectCST[
    cst,
    PerformanceGoal -> performanceGoal,
    "ConcreteRules" -> concreteRules,
    "AggregateRules" -> aggregateRules,
    "AbstractRules" -> abstractRules,
    "DisabledRegions" -> disabledRegions
  ]
]]



Options[CodeInspectCST] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  "DisabledRegions" -> {}
}

Attributes[CodeInspectCST] = {HoldFirst}

CodeInspectCST[cstIn_, OptionsPattern[]] :=
Catch[
Module[{cst, agg, aggregateRules, abstractRules, ast, poss, lints,
  prog, concreteRules, performanceGoal, start,
  scopingData, scopingLints, disabledRegions},

  If[$Debug,
    Print["CodeInspectCST"];
  ];

  cst = cstIn;

  lints = {};

  performanceGoal = OptionValue[PerformanceGoal];
  concreteRules = OptionValue["ConcreteRules"];
  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];
  disabledRegions = OptionValue["DisabledRegions"];

  If[$Debug,
    Print["disabledRegions: ", disabledRegions]
  ];

  If[FailureQ[cst],
    Throw[cst]
  ];

  If[empty[concreteRules] && empty[aggregateRules] && empty[abstractRules],

    lints = Flatten[lints];

    lints = Select[lints,
      Function[{lint},
        AllTrue[disabledRegions,
          Function[{region},
            !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || !MemberQ[region[[3]], lint[[1]]]
          ]
        ]
      ]
    ];

    Throw[lints]
  ];


  If[$Debug,
    Print["concreteRules"];
  ];

  lints = {};

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
        AllTrue[disabledRegions,
          Function[{region},
            !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || !MemberQ[region[[3]], lint[[1]]]
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
        AllTrue[disabledRegions,
          Function[{region},
            !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || !MemberQ[region[[3]], lint[[1]]]
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
      AllTrue[disabledRegions,
        Function[{region},
          !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || !MemberQ[region[[3]], lint[[1]]]
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
  "DisabledRegions" -> {}
}

Attributes[CodeInspectAgg] = {HoldFirst}

CodeInspectAgg[aggIn_, OptionsPattern[]] :=
Catch[
Module[{agg, aggregateRules, abstractRules, ast, poss, lints,
  prog, performanceGoal, start, disabledRegions},

  If[$Debug,
    Print["CodeInspectAgg"];
  ];

  agg = aggIn;

  lints = {};

  performanceGoal = OptionValue[PerformanceGoal];
  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];
  disabledRegions = OptionValue["DisabledRegions"];

  If[$Debug,
    Print["disabledRegions: ", disabledRegions]
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
        AllTrue[disabledRegions,
          Function[{region},
            !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || !MemberQ[region[[3]], lint[[1]]]
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
      AllTrue[disabledRegions,
        Function[{region},
          !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || !MemberQ[region[[3]], lint[[1]]]
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
  "DisabledRegions" -> {}
}

Attributes[CodeInspectAST] = {HoldFirst}

CodeInspectAST[astIn_, OptionsPattern[]] :=
Catch[
Module[{abstractRules, ast, poss, lints,
  prog, performanceGoal, start, disabledRegions},

  If[$Debug,
    Print["CodeInspectAST"];
  ];

  ast = astIn;

  lints = {};

  performanceGoal = OptionValue[PerformanceGoal];
  abstractRules = OptionValue["AbstractRules"];
  disabledRegions = OptionValue["DisabledRegions"];

  If[$Debug,
    Print["disabledRegions: ", disabledRegions]
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
      AllTrue[disabledRegions,
        Function[{region},
          !SourceMemberQ[region[[1;;2]], lint[[4, Key[Source]]]] || !MemberQ[region[[3]], lint[[1]]]
        ]
      ]
    ]
  ];
  
  lints
]]



End[]

EndPackage[]
