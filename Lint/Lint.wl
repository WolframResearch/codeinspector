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
  PerformanceGoal -> "Speed"
}




LintFile[file_String, OptionsPattern[]] :=
Catch[
 Module[{performanceGoal, full, lints, cst},

 performanceGoal = OptionValue[PerformanceGoal];

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

  lints = LintCST[cst];

  lints
]]


Options[LintString] = {
  PerformanceGoal -> "Speed"
}

LintString[string_String, OptionsPattern[]] :=
Catch[
 Module[{cst},

  cst = ConcreteParseString[string];

  If[FailureQ[cst],
    Throw[cst]
  ];

  LintCST[cst]
]]



LintCST[cstIn_, OptionsPattern[]] :=
Module[{cst, ast, pat, func, poss, lints},
  cst = cstIn;

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
      )&, $DefaultConcreteRules]
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
      )&, $DefaultAbstractRules]
  ];

  lints = Flatten[lints];

  lints
]


End[]

EndPackage[]