BeginPackage["Lint`"]

Lint::usage = "Lint[tag, description, severity, data] is a problem found in Wolfram Language source code."

LintFile::usage = "LintFile[file] returns a list of Lints in file."

LintString::usage = "LintString[string] returns a list of Lints in string."

LintAST::usage = "LintAST[ast] returns a list of Lints in ast."



LintedLine::usage = "LintedLine[lineNumber, hash, content, lintList, options] represents a formatted line of output."

LintedCharacter::usage = "LintedCharacter[char, lintList, options] represents a formatted character of output."



LintFileReport::usage = "LintFileReport[file, lints] returns a list of LintedLines in file."

LintStringReport::usage = "LintStringReport[string, lints] returns a LintedLines in string."





Begin["`Private`"]

Needs["AST`"]
Needs["Lint`Report`"]
Needs["Lint`Rules`"]
Needs["Lint`Format`"]



Options[LintFile] = {
  PerformanceGoal -> "Speed"
}



longestParseTime = 0
longestLintTime = 0

LintFile[file_String, OptionsPattern[]] :=
Catch[
 Module[{performanceGoal, lints},

 performanceGoal = OptionValue[PerformanceGoal];

  If[FileType[file] =!= File,
   Throw[Failure["NotAFile", <|"FileName"->file|>]]
   ];

   If[performanceGoal == "Speed",
    If[FileByteCount[file] > 1*^6,
     Throw[Failure["FileTooLarge", <|"FileName"->file, "FileSize"->FileSize[file]|>]]
     ];
    ];

  ast = ParseFile[file];

  If[FailureQ[ast],
    Throw[ast]
  ];

  lints = LintAST[ast];

  lints
]]


Options[LintString] = {

}

LintString[string_String, OptionsPattern[]] :=
Catch[
 Module[{},

  ast = ParseString[string];

  If[FailureQ[ast],
    Throw[ast]
  ];

  LintAST[ast]
]]



LintAST[astIn_, OptionsPattern[]] :=
Module[{ast, pat, func, poss},
  ast = astIn;
  Flatten[
  KeyValueMap[(
    pat = #1;
    func = #2;
    poss = Position[ast, pat];
    Map[(func[#, ast])&, poss]
    )&, $DefaultRules]]
]


End[]

EndPackage[]