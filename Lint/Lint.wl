BeginPackage["Lint`"]

Lint::usage = "Lint[tag, description, severity, data] is a problem found in WL source code."

LintFile::usage = "LintFile[file] returns a list of Lints found in file."

LintString::usage = "LintString[string] returns a list of Lints found in string."

LintCST



LintedLine::usage = "LintedLine[lineSource, lineNumber, hash, content, lintList] represents a formatted line of output."

LintedCharacter::usage = "LintedCharacter[char, lintList, options] represents a formatted character of output."



LintFileReport::usage = "LintFileReport[file, lints] returns a list of LintedLines found in file."

LintStringReport::usage = "LintStringReport[string, lints] returns a list of LintedLines found in string."





Begin["`Private`"]

Needs["AST`"]
Needs["AST`Abstract`"]
Needs["Lint`Report`"]
Needs["Lint`Rules`"]
Needs["Lint`Format`"]



Options[LintFile] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AbstractRules" :> $DefaultAbstractRules
}




LintFile[file_String, OptionsPattern[]] :=
Catch[
 Module[{performanceGoal, concreteRules, abstractRules, full, lints, cst},

 performanceGoal = OptionValue[PerformanceGoal];
 concreteRules = OptionValue["ConcreteRules"];
 abstractRules = OptionValue["AbstractRules"];

  full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <|"FileName"->file|>]]
  ];

   If[performanceGoal == "Speed",
    If[FileByteCount[full] > 1*^6,
     Throw[Failure["FileTooLarge", <|"FileName"->full, "FileSize"->FileSize[full]|>]]
     ];
    ];

  cst = ConcreteParseFile[full];

  If[FailureQ[cst],
    Throw[cst]
  ];

  lints = LintCST[cst, "ConcreteRules" -> concreteRules, "AbstractRules" -> abstractRules];

  lints
]]


Options[LintString] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AbstractRules" :> $DefaultAbstractRules
}

LintString[string_String, OptionsPattern[]] :=
Catch[
 Module[{concreteRules, abstractRules, cst},

  concreteRules = OptionValue["ConcreteRules"];
  abstractRules = OptionValue["AbstractRules"];

  cst = ConcreteParseString[string];

  If[FailureQ[cst],
    Throw[cst]
  ];

  LintCST[cst, "ConcreteRules" -> concreteRules, "AbstractRules" -> abstractRules]
]]




Options[LintCST] = {
  "ConcreteRules" :> $DefaultConcreteRules,
  "AbstractRules" :> $DefaultAbstractRules
}

LintCST[cstIn_, OptionsPattern[]] :=
Module[{cst, concreteRules, abstractRules, ast, pat, func, poss, lints},

  cst = cstIn;

  concreteRules = OptionValue["ConcreteRules"];
  abstractRules = OptionValue["AbstractRules"];

  lints = {};

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