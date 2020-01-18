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
Needs["Lint`Folds`"]
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



Options[LintCST] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules
}

Attributes[LintCST] = {HoldFirst}

LintCST[cstIn_, OptionsPattern[]] :=
Catch[
Module[{cst, agg, aggregateRules, abstractRules, ast, pat, func, poss, lints, staticAnalysisIgnoreNodes,
  ignoredNodesSrcMemberFunc, prog, concreteRules, performanceGoal, start, missingCloserChildrenNodes,
  ignoredNodes},

  If[$Debug,
    Print["LintCST"];
  ];

  cst = cstIn;

  lints = {};

  performanceGoal = OptionValue[PerformanceGoal];
  concreteRules = OptionValue["ConcreteRules"];
  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];

  agg = Aggregate[cst];

  If[FailureQ[agg],
    Throw[agg]
  ];

  ast = Abstract[agg];

  If[FailureQ[ast],
    Throw[ast]
  ];

  (*
  Make sure to use Infinity, because StaticAnalysisIgnoreNode may be nested inside of PackageNode or ContextNode
  *)
  staticAnalysisIgnoreNodes = Cases[ast[[2]], StaticAnalysisIgnoreNode[_, _, _], Infinity];

  (*
  Do not descend into MissingCloser nodes

  And skip the opener token. This allows the GroupMissingCloserNode itself to be counted in the linter.
  *)
  missingCloserChildrenNodes = Flatten[Cases[cst[[2]], GroupMissingCloserNode[_, children_, _] :> children[[2;;]], Infinity]];

  ignoredNodes = staticAnalysisIgnoreNodes ~Join~ missingCloserChildrenNodes;

  If[$Debug,
    Print["staticAnalysisIgnoreNodes: ", staticAnalysisIgnoreNodes];
    Print["missingCloserChildrenNodes: ", missingCloserChildrenNodes];
    Print["ignoredNodes: ", ignoredNodes];
  ];

  ignoredNodesSrcMemberFunc = SourceMemberQ[ignoredNodes[[All, 3, Key[Source] ]] ];
  
  cst = removeIgnoredNodes[cst, ignoredNodesSrcMemberFunc];

  agg = removeIgnoredNodes[agg, ignoredNodesSrcMemberFunc];

  ast = removeIgnoredNodes[ast, ignoredNodesSrcMemberFunc];

  (*
  agg[[2]] = DeleteCases[agg[[2]], node_ /; SourceMemberQ[staticAnalysisIgnoreNodes[[All, 3, Key[Source]]], node[[3]][Source]]];

  ast[[2]] = DeleteCases[ast[[2]], node_ /; SourceMemberQ[staticAnalysisIgnoreNodes[[All, 3, Key[Source]]], node[[3]][Source]]];
  *)

  If[$Debug,
    Print["cst: ", cst];
    Print["agg: ", agg];
    Print["ast: ", ast];
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
