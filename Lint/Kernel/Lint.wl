BeginPackage["Lint`"]

(*
Functions
*)

LintString

LintFile

LintBytes

LintBox

LintCST



LintStringReport

LintFileReport

LintBytesReport

LintBoxReport



(*
Objects
*)

Lint

LintedLine

LintedFile

LintedString

LintedBox



BeginStaticAnalysisIgnore
EndStaticAnalysisIgnore




$ConcreteLintProgress
$ConcreteLintTime
$AggregateLintProgress
$AggregateLintTime
$AbstractLintProgress
$AbstractLintTime

Begin["`Private`"]

Needs["AST`"]
Needs["AST`Abstract`"]
Needs["AST`Utils`"]
Needs["Lint`AbstractRules`"]
Needs["Lint`AggregateRules`"]
Needs["Lint`Boxes`"]
Needs["Lint`ConcreteRules`"]
Needs["Lint`Format`"]
Needs["Lint`Report`"]



Lint::usage = "Lint[tag, description, severity, data] is a problem found in WL source code."

(*
provide some selectors for Lint and LintedLine objects
*)

Lint[tag_,     _,         _, _]["Tag"] := tag
Lint[   _, desc_,         _, _]["Description"] := desc
Lint[   _,     _, severity_, _]["Severity"] := severity



LintedLine[_, lineNumber_,     _, _,      _, ___]["LineNumber"] := lineNumber
LintedLine[_,           _, hash_, _,      _, ___]["Hash"] := hash
LintedLine[_,           _,     _, _, lints_, ___]["Lints"] := lints






LintFile::usage = "LintFile[file] returns a list of Lints found in file."

Options[LintFile] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  CharacterEncoding -> "UTF-8"
}


$fileByteCountMinLimit = 0*^6
$fileByteCountMaxLimit = 3*^6



LintFile[File[file_String], OptionsPattern[]] :=
Catch[
Module[{performanceGoal, aggregateRules, abstractRules, encoding, full, lints, cst, data, concreteRules},

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

  cst = ConcreteParseFile[File[full]];

  If[FailureQ[cst],
    Throw[cst]
  ];

  lints = LintCST[
    cst,
    PerformanceGoal -> performanceGoal,
    "ConcreteRules" -> concreteRules,
    "AggregateRules" -> aggregateRules,
    "AbstractRules" -> abstractRules
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
    lint[[4]] = data;
    lint
    ,
    {lint, lints}
  ];

  lints
]]




LintString::usage = "LintString[string] returns a list of Lints found in string."

Options[LintString] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules
}

LintString[string_String, OptionsPattern[]] :=
Catch[
 Module[{aggregateRules, abstractRules, cst, concreteRules, performanceGoal},

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

  cst = ConcreteParseString[string];

  If[FailureQ[cst],
    Throw[cst]
  ];

  LintCST[
    cst,
    PerformanceGoal -> performanceGoal,
    "ConcreteRules" -> concreteRules,
    "AggregateRules" -> aggregateRules,
    "AbstractRules" -> abstractRules
  ]
]]


LintBytes::usage = "LintBytes[bytes] returns a list of Lints found in bytes."

Options[LintBytes] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules
}

LintBytes[bytes_List, OptionsPattern[]] :=
Catch[
 Module[{aggregateRules, abstractRules, cst, concreteRules, performanceGoal},

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

  cst = ConcreteParseBytes[bytes];

  If[FailureQ[cst],
    Throw[cst]
  ];

  LintCST[
    cst,
    PerformanceGoal -> performanceGoal,
    "ConcreteRules" -> concreteRules,
    "AggregateRules" -> aggregateRules,
    "AbstractRules" -> abstractRules
  ]
]]


Options[LintBox] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules
}

LintBox[box_, OptionsPattern[]] :=
Catch[
 Module[{aggregateRules, abstractRules, cst, concreteRules, performanceGoal},

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

  cst = ConcreteParseBox[box];

  If[FailureQ[cst],
    Throw[cst]
  ];

  LintCST[
    cst,
    PerformanceGoal -> performanceGoal,
    "ConcreteRules" -> concreteRules,
    "AggregateRules" -> aggregateRules,
    "AbstractRules" -> abstractRules]
]]







beginStaticAnalysisIgnoreCallPat0 = CallNode[{LeafNode[Symbol, "BeginStaticAnalysisIgnore" | "Lint`BeginStaticAnalysisIgnore", _]}, {GroupNode[GroupSquare, _, _]}, _]

beginStaticAnalysisIgnoreCallPat = beginStaticAnalysisIgnoreCallPat0 | InfixNode[CompoundExpression, {beginStaticAnalysisIgnoreCallPat0, LeafNode[Token`Semi, _, _], LeafNode[Token`Fake`ImplicitNull, _, _]}, _]


endStaticAnalysisIgnoreCallPat0 = CallNode[{LeafNode[Symbol, "EndStaticAnalysisIgnore" | "Lint`EndStaticAnalysisIgnore", _]}, {GroupNode[GroupSquare, _, _]}, _]

endStaticAnalysisIgnoreCallPat = endStaticAnalysisIgnoreCallPat0 | InfixNode[CompoundExpression, {endStaticAnalysisIgnoreCallPat0, LeafNode[Token`Semi, _, _], LeafNode[Token`Fake`ImplicitNull, _, _]}, _]


Options[LintCST] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules
}

Attributes[LintCST] = {HoldFirst}

LintCST[cstIn_, OptionsPattern[]] :=
Catch[
Module[{cst, agg, aggregateRules, abstractRules, ast, pat, func, poss, lints,
  ignoredNodesSrcMemberFunc, prog, concreteRules, performanceGoal, start,
  ignoredNodes, beginStaticAnalysisIgnoreNodePoss, endPos, siblingsPos, siblings, candidate, endFound},

  If[$Debug,
    Print["LintCST"];
  ];

  cst = cstIn;

  lints = {};

  performanceGoal = OptionValue[PerformanceGoal];
  concreteRules = OptionValue["ConcreteRules"];
  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];

  (*
  Make sure to use Infinity
  *)
  
  ignoredNodes = {};

  beginStaticAnalysisIgnoreNodePoss = Position[cst, beginStaticAnalysisIgnoreCallPat];

  ignoredNodes = Reap[
  Do[
    siblingsPos = Most[beginPos];
    siblings = Extract[cst, {siblingsPos}][[1]];
    endFound = False;
    Do[
      candidate = siblings[[pos]];
      If[MatchQ[candidate, endStaticAnalysisIgnoreCallPat],
        endPos = pos;
        endFound = True;
        Break[]
      ]
      ,
      {pos, Last[beginPos]+1, Length[siblings]}
    ];
    If[endFound,
      staticAnalysisIgnoreChildren = siblings[[(Last[beginPos]+1);;(endPos-1)]];
      Sow[staticAnalysisIgnoreChildren]
      ,
      Message[EndStaticAnalysisIgnore::missing]
    ]
    ,
    {beginPos, beginStaticAnalysisIgnoreNodePoss}
  ]][[2]];

  If[!empty[ignoredNodes],
    ignoredNodes = ignoredNodes[[1]];
    ignoredNodes = Flatten[ignoredNodes];
  ];

  If[$Debug,
    Print["ignoredNodes: ", ignoredNodes];
  ];

  ignoredNodesSrcMemberFunc = SourceMemberQ[ignoredNodes[[All, 3, Key[Source] ]] ];
  
  


  cst = removeIgnoredNodes[cst, ignoredNodesSrcMemberFunc];
  
  If[$Debug,
    Print["cst: ", cst];
  ];

  If[$Debug,
    Print["concreteRules"];
  ];

  lints = {};

  prog = 0;
  start = Now;
  KeyValueMap[(
    If[$Debug,
      Print[#];
    ];
    pat = #1;
    func = #2;
    poss = Position[cst, pat];
    AppendTo[lints, Map[(func[#, cst])&, poss]];
    prog++;
    $ConcreteLintProgress = Floor[100 * prog / Length[concreteRules]];
    )&, concreteRules];
  $ConcreteLintTime = Now - start;


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
  KeyValueMap[(
    If[$Debug,
      Print[#];
    ];
    pat = #1;
    func = #2;
    poss = Position[agg, pat];
    AppendTo[lints, Map[(func[#, agg])&, poss]];
    prog++;
    $AggregateLintProgress = Floor[100 * prog / Length[aggregateRules]];
    )&, aggregateRules];
  $AggregateLintTime = Now - start;



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
  KeyValueMap[(
    If[$Debug,
      Print[#];
    ];
    pat = #1;
    func = #2;
    poss = Position[ast, pat];
    AppendTo[lints, Map[(func[#, ast])&, poss]];
    prog++;
    $AbstractLintProgress = Floor[100 * prog / Length[abstractRules]];
    )&, abstractRules];
  $AbstractLintTime = Now - start;

  lints = Flatten[lints];

  lints
]]



End[]

EndPackage[]
