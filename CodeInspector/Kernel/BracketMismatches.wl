BeginPackage["CodeInspector`BracketMismatches`"]

CodeInspectBracketMismatches

CodeInspectBracketMismatchesSummarize


CodeInspectBracketMismatchesCST

CodeInspectBracketMismatchesCSTSummarize


CodeInspectBracketMismatchesAgg


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Abstract`"]
Needs["CodeParser`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Summarize`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`Utils`"]



(*
How many bracket mismatches to keep?
*)
$BracketMismatchesLimit = 1



CodeInspectBracketMismatches::usage = "CodeInspectBracketMismatches[code] returns a list of bracket mismatches in code."

Options[CodeInspectBracketMismatches] = {
  PerformanceGoal -> "Speed",
  "TabWidth" -> ("TabWidth" /. Options[CodeConcreteParse]),
  SourceConvention -> (SourceConvention /. Options[CodeConcreteParse])
}


$fileByteCountMinLimit = 0*^6
$fileByteCountMaxLimit = 3*^6



CodeInspectBracketMismatches[File[file_String], opts:OptionsPattern[]] :=
Catch[
 Module[{full, performanceGoal, cst},

 performanceGoal = OptionValue[PerformanceGoal];

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

    CodeInspectBracketMismatchesCST[cst]
]]





CodeInspectBracketMismatches[string_String, opts:OptionsPattern[]] :=
Catch[
Module[{cst},

  cst = CodeConcreteParse[string, FilterRules[{opts}, Options[CodeConcreteParse]]];

  CodeInspectBracketMismatchesCST[cst]
]]




CodeInspectBracketMismatchesCST[cst_] :=
Catch[
Module[{mismatches, agg},

  If[FailureQ[cst],
    Throw[cst]
  ];

  agg = Aggregate[cst];

  mismatches = bracketMismatches[agg];

  mismatches
]]

CodeInspectBracketMismatchesAgg[agg_] :=
Catch[
Module[{mismatches},

  If[FailureQ[agg],
    Throw[agg]
  ];

  mismatches = bracketMismatches[agg];

  mismatches
]]




CodeInspectBracketMismatchesSummarize::usage = "BracketMismatchSummarize[code] returns an inspection summary object."

Options[CodeInspectBracketMismatchesSummarize] = {
  "TabWidth" -> ("TabWidth" /. Options[CodeConcreteParse]),
  SourceConvention -> (SourceConvention /. Options[CodeConcreteParse])
}

CodeInspectBracketMismatchesSummarize[File[file_String], bracketMismatchesIn:{(GroupMissingCloserNode|UnterminatedGroupNode|ErrorNode)[_, _, _]...}:Automatic, opts:OptionsPattern[]] :=
Catch[
Module[{mismatches, full, lines, lintedLines, bytes, str, tabWidth},

  mismatches = bracketMismatchesIn;

  tabWidth = OptionValue["TabWidth"];

  full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <|"FileName"->file|>]]
  ];

  If[FileByteCount[full] == 0,
    Throw[Failure["EmptyFile", <|"FileName"->full|>]]
  ];

  If[mismatches === Automatic,
    mismatches = CodeInspectBracketMismatches[File[full], FilterRules[{opts}, Options[CodeInspectBracketMismatches]]];
  ];

  (*
  Was:
  bytes = Import[full, "Byte"];

  but this is slow
  *)
  bytes = Normal[ReadByteArray[full]] /. EndOfFile -> {};

  str = SafeString[bytes];

  lines = StringSplit[str, {"\r\n", "\n", "\r"}, All];

  lines = replaceTabs[#, 1, "!", tabWidth]& /@ lines;

  lintedLines = bracketMismatchesLinesReport[lines, mismatches];
  InspectedFileObject[full, lintedLines]
]]





CodeInspectBracketMismatchesSummarize[string_String, bracketMismatchesIn:{(GroupMissingCloserNode|UnterminatedGroupNode|ErrorNode)[_, _, _]...}:Automatic, opts:OptionsPattern[]] :=
Catch[
Module[{mismatches, lines, lintedLines, tabWidth},

  mismatches = bracketMismatchesIn;

  tabWidth = OptionValue["TabWidth"];

  If[StringLength[string] == 0,
    Throw[Failure["EmptyString", <||>]]
  ];

  If[mismatches === Automatic,
    mismatches = CodeInspectBracketMismatches[string, FilterRules[{opts}, Options[CodeInspectBracketMismatches]]];
  ];

  lines = StringSplit[string, {"\r\n", "\n", "\r"}, All];

  lines = replaceTabs[#, 1, "!", tabWidth]& /@ lines;

  lintedLines = bracketMismatchesLinesReport[lines, mismatches];
  InspectedStringObject[string, lintedLines]
]]




Options[CodeInspectBracketMismatchesCSTSummarize] = {
  "TabWidth" -> ("TabWidth" /. Options[CodeConcreteParse])
}

CodeInspectBracketMismatchesCSTSummarize[cst_, bracketMismatchesIn:{(GroupMissingCloserNode|UnterminatedGroupNode|ErrorNode)[_, _, _]...}:Automatic, OptionsPattern[]] :=
Catch[
Module[{mismatches, lines, lintedLines, string, tabWidth},

  If[FailureQ[cst],
    Throw[cst]
  ];

  mismatches = bracketMismatchesIn;

  tabWidth = OptionValue["TabWidth"];

  If[mismatches === Automatic,
    mismatches = CodeInspectBracketMismatchesCST[cst];
  ];

  string = ToSourceCharacterString[cst];

  lines = StringSplit[string, {"\r\n", "\n", "\r"}, All];

  lines = replaceTabs[#, 1, "!", tabWidth]& /@ lines;

  lintedLines = bracketMismatchesLinesReport[lines, mismatches];
  InspectedStringObject[string, lintedLines]
]]



bracketMismatches[agg_] :=
Catch[
Module[{mismatches},

  mismatches = Cases[agg, GroupMissingCloserNode[_, _, _] | UnterminatedGroupNode[_, _, _] | ErrorNode[Token`Error`UnexpectedCloser, _, _], {0, Infinity}];

  mismatches
]]



(* how many (, ), or \[Times] to insert per line *)
$markupLimit = 100


$color = severityColor[{
  InspectionObject["GroupMissingCloser", "Missing closer.", "Fatal", <||>],
  InspectionObject["UnexpectedCloser", "Unexpected closer.", "Fatal", <||>]}];


(*
Return list of characters representing the under line
*)
modify[lineIn_String, {missingOpenerStarts_, missingCloserStarts_}, lineNumber_] :=
 Module[{line, infixCols, infixInserters, under,
  rules, missingOpenerCols, missingCloserCols, missingOpenerInserters, missingCloserInserters},

  line = lineIn;

  missingOpenerCols = Cases[missingOpenerStarts, {lineNumber, col_} :> col];
  missingCloserCols = Cases[missingCloserStarts, {lineNumber, col_} :> col];

  missingOpenerInserters = AssociationMap[LintMarkup[LintMissingOpenerIndicatorCharacter, FontWeight->Bold, FontSize->Larger, FontColor->$color]&, missingOpenerCols];
  missingCloserInserters = AssociationMap[LintMarkup[LintMissingCloserIndicatorCharacter, FontWeight->Bold, FontSize->Larger, FontColor->$color]&, missingCloserCols];

  If[$Debug,
    Print["lineNumber: ", lineNumber];
    Print["infixInserters: ", infixInserters];
  ];

  rules = Join[missingOpenerInserters, missingCloserInserters];
  rules = Normal[rules];

  under = Table[" ", {StringLength[line]}];

  (*
  extend line to be able to insert \[Times] after the line, when ImplicitTimes spans lines
  *)
  under = Join[under, {" "}];

  under = ReplacePart[under, rules];

  (*
  to match Listify
  *)
  under = Join[{" "}, under];

  under
  ]



bracketMismatchesLinesReport[linesIn:{___String}, bracketMismatchesIn:{(GroupMissingCloserNode|UnterminatedGroupNode|ErrorNode)[_, _, _]...}] :=
Catch[
 Module[{mismatches, infixs, lines, linesToModify, missingOpeners, missingClosers, missingOpenerStarts,
   missingCloserStarts, maxLineNumberLength},

    If[bracketMismatchesIn === {},
      Throw[{}]
    ];

    mismatches = bracketMismatchesIn;

    lines = linesIn;

    If[$Debug,
      Print["lines: ", lines];
    ];

    mismatches = Take[mismatches, UpTo[$BracketMismatchesLimit]];

    missingOpeners = Cases[mismatches, ErrorNode[Token`Error`UnexpectedCloser, _, _]];

    missingClosers = Cases[mismatches, GroupMissingCloserNode[_, _, _] | UnterminatedGroupNode[_, _, _]];

    missingOpenerStarts = missingOpeners[[All, 3, Key[Source], 1]];

    missingCloserStarts = missingClosers[[All, 3, Key[Source], 1]];

   linesToModify = Union[missingOpenerStarts[[All, 1]], missingCloserStarts[[All, 1]]];

   maxLineNumberLength = Max[IntegerLength /@ linesToModify];

   Table[

     InspectedLineObject[lines[[i]], i, {ListifyLine[lines[[i]], <||>, "EndOfFile" -> (i == Length[lines])],
                                  modify[lines[[i]], {missingOpenerStarts, missingCloserStarts}, i]},
                                  {}, "MaxLineNumberLength" -> maxLineNumberLength]
    ,
    {i, linesToModify}
    ]
]]



End[]

EndPackage[]
