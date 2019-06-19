BeginPackage["Lint`"]

Lint::usage = "Lint[tag, description, severity, data] is a problem found in WL source code."

LintFile::usage = "LintFile[file] returns a list of Lints found in file."

LintString::usage = "LintString[string] returns a list of Lints found in string."

LintCST



LintedLine::usage = "LintedLine[lineSource, lineNumber, hash, content, lintList] represents a formatted line of output."



LintFileReport::usage = "LintFileReport[file, lints] returns a LintedFile object."

LintStringReport::usage = "LintStringReport[string, lints] returns a LintedString object."



LintedFile::usage = "LintedFile[file, lintedLines] represents a formatted object of linted lines found in file."

LintedString::usage = "LintedString[string, lintedLines] represents a formatted object of linted lines found in string."




Begin["`Private`"]

Needs["AST`"]
Needs["AST`Abstract`"]
Needs["AST`Utils`"]

Needs["Lint`Report`"]
Needs["Lint`AbstractRules`"]
Needs["Lint`AggregateRules`"]
Needs["Lint`Format`"]
Needs["Lint`Folds`"]




(*
provide some selectors for Lint and LintedLine objects
*)

Lint[tag_,     _,         _, _]["Tag"] := tag
Lint[   _, desc_,         _, _]["Description"] := desc
Lint[   _,     _, severity_, _]["Severity"] := severity



LintedLine[_, lineNumber_,     _, _,      _]["LineNumber"] := lineNumber
LintedLine[_,           _, hash_, _,      _]["Hash"] := hash
LintedLine[_,           _,     _, _, lints_]["Lints"] := lints






Options[LintFile] = {
  PerformanceGoal -> "Speed",
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  CharacterEncoding -> "UTF-8"
}

$fileByteCountLimit = 2*^6



LintFile[file_String | File[file_String], OptionsPattern[]] :=
Catch[
 Module[{performanceGoal, aggregateRules, abstractRules, encoding, full, lints, cstAndIssues},

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
    If[FileByteCount[full] > $fileByteCountLimit,
     Throw[Failure["FileTooLarge", <|"FileName"->full, "FileSize"->FileSize[full]|>]]
     ];
    ];

  cstAndIssues = ConcreteParseFile[full, {FileNode[File, #[[1]], <||>], #[[2]]}&];

  If[FailureQ[cstAndIssues],
    Throw[cstAndIssues]
  ];

  lints = LintCST[cstAndIssues[[1]], cstAndIssues[[2]], "AggregateRules" -> aggregateRules, "AbstractRules" -> abstractRules];

  lints
]]


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

  cstAndIssues = ConcreteParseString[string, {FileNode[File, #[[1]], <||>], #[[2]]}&];

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
Module[{cst, agg, aggregateRules, abstractRules, ast, pat, func, poss, lints, staticAnalysisIgnoreNodes, ignoredNodesSrcMemberFunc},

  cst = cstIn;

  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];

  lints = Lint @@@ issues;

  agg = Aggregate[cst];

  ast = Abstract[agg];

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

  AppendTo[lints,
    KeyValueMap[(
      If[$Debug,
        Print[#];
      ];
      pat = #1;
      func = #2;
      poss = Position[agg, pat];
      Map[(func[#, agg])&, poss]
      )&, aggregateRules]
  ];

  AppendTo[lints, 
    KeyValueMap[(
      If[$Debug,
        Print[#];
      ];
      pat = #1;
      func = #2;
      poss = Position[ast, pat];
      Map[(func[#, ast])&, poss]
      )&, abstractRules]
  ];

  lints = Flatten[lints];

  lints
]


End[]

EndPackage[]
