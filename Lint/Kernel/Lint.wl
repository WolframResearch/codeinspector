BeginPackage["Lint`"]

Lint


LintFile

LintString

LintBox

LintCST



LintedLine



LintFileReport

LintStringReport




LintedFile

LintedString



$Progress
$Start
$Time


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Abstract`"]
Needs["AST`Utils`"]

Needs["Lint`Report`"]
Needs["Lint`AbstractRules`"]
Needs["Lint`AggregateRules`"]
Needs["Lint`Format`"]
Needs["Lint`Folds`"]





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
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  CharacterEncoding -> "UTF-8"
}


$fileByteCountMinLimit = 0*^6
$fileByteCountMaxLimit = 3*^6



LintFile[file_String | File[file_String], OptionsPattern[]] :=
Catch[
 Module[{performanceGoal, aggregateRules, abstractRules, encoding, full, lints, cstAndIssues, data},

 performanceGoal = OptionValue[PerformanceGoal];
 aggregateRules = OptionValue["AggregateRules"];
 abstractRules = OptionValue["AbstractRules"];

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

  cstAndIssues = ConcreteParseFile[full, {FileNode[File, #[[1]], <||>], Cases[#[[2]], _SyntaxIssue]}&];

  If[FailureQ[cstAndIssues],
    Throw[cstAndIssues]
  ];

  lints = LintCST[cstAndIssues[[1]], cstAndIssues[[2]], "AggregateRules" -> aggregateRules, "AbstractRules" -> abstractRules];

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
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules
}

LintString[string_String, OptionsPattern[]] :=
Catch[
 Module[{aggregateRules, abstractRules, cstAndIssues},

  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];

  $Progress = 0;

  cstAndIssues = ConcreteParseString[string, {FileNode[File, #[[1]], <||>], Cases[#[[2]], _SyntaxIssue]}&];

  If[FailureQ[cstAndIssues],
    Throw[cstAndIssues]
  ];

  LintCST[cstAndIssues[[1]], cstAndIssues[[2]], "AggregateRules" -> aggregateRules, "AbstractRules" -> abstractRules]
]]


Options[LintBox] = {
  PerformanceGoal -> "Speed",
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules
}

LintBox[box_, OptionsPattern[]] :=
Catch[
 Module[{aggregateRules, abstractRules, cstAndIssues},

  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];

  $Progress = 0;

  cstAndIssues = ConcreteParseBox[box, {FileNode[File, #[[1]], <||>], Cases[#[[2]], _SyntaxIssue]}&];

  If[FailureQ[cstAndIssues],
    Throw[cstAndIssues]
  ];

  LintCST[cstAndIssues[[1]], cstAndIssues[[2]], "AggregateRules" -> aggregateRules, "AbstractRules" -> abstractRules]
]]



Options[LintCST] = {
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules
}

Attributes[LintCST] = {HoldFirst}

LintCST[cstIn_, issues_, OptionsPattern[]] :=
Catch[
Module[{cst, agg, aggregateRules, abstractRules, ast, pat, func, poss, lints, staticAnalysisIgnoreNodes, ignoredNodesSrcMemberFunc,
  totalRules, prog},

  If[$Debug,
    Print["LintCST"];
  ];

  cst = cstIn;

  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];

  lints = Lint @@@ issues;
  If[$Debug,
    Print["lints: ", lints];
  ];

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

  If[$Debug,
    Print["staticAnalysisIgnoreNodes: ", staticAnalysisIgnoreNodes];
  ];

  ignoredNodesSrcMemberFunc = SourceMemberQ[staticAnalysisIgnoreNodes[[All, 3, Key[Source]]]];

  agg = removeIgnoredNodes[agg, ignoredNodesSrcMemberFunc];

  ast = removeIgnoredNodes[ast, ignoredNodesSrcMemberFunc];

  (*
  agg[[2]] = DeleteCases[agg[[2]], node_ /; SourceMemberQ[staticAnalysisIgnoreNodes[[All, 3, Key[Source]]], node[[3]][Source]]];

  ast[[2]] = DeleteCases[ast[[2]], node_ /; SourceMemberQ[staticAnalysisIgnoreNodes[[All, 3, Key[Source]]], node[[3]][Source]]];
  *)

  If[$Debug,
    Print["agg: ", agg];
  ];

  If[$Debug,
    Print["ast: ", ast];
  ];

  totalRules = Length[aggregateRules] + Length[abstractRules];
  prog = 0;
  $Start = Now;

  If[$Debug,
    Print["aggregateRules"];
  ];

  KeyValueMap[(
    If[$Debug,
      Print[#];
    ];
    pat = #1;
    func = #2;
    poss = Position[agg, pat];
    AppendTo[lints, Map[(func[#, agg])&, poss]];
    prog++;
    $Progress = Floor[100 * prog / totalRules];
    )&, aggregateRules];

  If[$Debug,
    Print["abstractRules"];
  ];

  KeyValueMap[(
    If[$Debug,
      Print[#];
    ];
    pat = #1;
    func = #2;
    poss = Position[ast, pat];
    AppendTo[lints, Map[(func[#, ast])&, poss]];
    prog++;
    $Progress = Floor[100 * prog / totalRules];
    )&, abstractRules];

  $Time = Now - $Start;

  lints = Flatten[lints];

  lints
]]



End[]

EndPackage[]
