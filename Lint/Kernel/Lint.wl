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
Needs["Lint`Report`"]
Needs["Lint`Rules`"]
Needs["Lint`Format`"]




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
  "ConcreteRules" :> $DefaultConcreteRules,
  "AbstractRules" :> $DefaultAbstractRules,
  CharacterEncoding -> "UTF-8"
}

$fileByteCountLimit = 2*^6



LintFile[file_String | File[file_String], OptionsPattern[]] :=
Catch[
 Module[{performanceGoal, concreteRules, abstractRules, encoding, full, lints, cstAndIssues},

 performanceGoal = OptionValue[PerformanceGoal];
 concreteRules = OptionValue["ConcreteRules"];
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

  cstAndIssues = ConcreteParseFile[full, {FileNode[File, #[[1]], <||>], #[[3]]}&];

  If[FailureQ[cstAndIssues],
    Throw[cstAndIssues]
  ];

  lints = LintCST[cstAndIssues[[1]], cstAndIssues[[2]], "ConcreteRules" -> concreteRules, "AbstractRules" -> abstractRules];

  lints
]]


Options[LintString] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AbstractRules" :> $DefaultAbstractRules
}

LintString[string_String, OptionsPattern[]] :=
Catch[
 Module[{concreteRules, abstractRules, cstAndIssues},

  concreteRules = OptionValue["ConcreteRules"];
  abstractRules = OptionValue["AbstractRules"];

  cstAndIssues = ConcreteParseString[string, {FileNode[File, #[[1]], <||>], #[[3]]}&];

  If[FailureQ[cstAndIssues],
    Throw[cstAndIssues]
  ];

  LintCST[cstAndIssues[[1]], cstAndIssues[[2]], "ConcreteRules" -> concreteRules, "AbstractRules" -> abstractRules]
]]




Options[LintCST] = {
  "ConcreteRules" :> $DefaultConcreteRules,
  "AbstractRules" :> $DefaultAbstractRules
}

Attributes[LintCST] = {HoldFirst}

LintCST[cstIn_, issues_, OptionsPattern[]] :=
Module[{cst, concreteRules, abstractRules, ast, pat, func, poss, lints},

  cst = cstIn;

  concreteRules = OptionValue["ConcreteRules"];
  abstractRules = OptionValue["AbstractRules"];

  lints = Lint @@@ issues;

  AppendTo[lints,
    KeyValueMap[(
      If[$Debug,
        Print[#];
      ];
      pat = #1;
      func = #2;
      poss = Position[cst, pat];
      Map[(func[#, cst])&, poss]
      )&, concreteRules]
  ];

  ast = Abstract[cst];

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