BeginPackage["CodeInspector`Summarize`"]

ListifyLine



$DefaultConfidenceLevel

$DefaultTagExclusions

$DefaultSeverityExclusions


$LintedLineLimit

$DefaultLintLimit


$LineTruncationLimit


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`AbstractRules`"]
Needs["CodeInspector`AggregateRules`"]
Needs["CodeInspector`ConcreteRules`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`TokenRules`"]
Needs["CodeInspector`Utils`"]


$DefaultTagExclusions = {}

$DefaultSeverityExclusions = {"Formatting", "Remark"}

(*
How many linted lines to keep?
*)
$LintedLineLimit = 10

(*
How many lints to keep?
*)
$DefaultLintLimit = 100

(*
Number of characters per line to consider "long" and truncate
*)
$LineTruncationLimit = Infinity

(*
How many lines to include above and below each lint
*)
$EnvironBuffer = 1



$DefaultConfidenceLevel = 0.75





CodeInspectSummarize::usage = "CodeInspectSummarize[code] returns an inspection summary object. \
code can be a string, a file, or a list of bytes."

Options[CodeInspectSummarize] = {
  PerformanceGoal -> "Speed",
  "TokenRules" :> $DefaultTokenRules,
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  (*
  filtering
  *)
  "TagExclusions" :> $DefaultTagExclusions,
  "SeverityExclusions" :> $DefaultSeverityExclusions,
  ConfidenceLevel :> $DefaultConfidenceLevel,
  "LintLimit" :> $DefaultLintLimit,
  (*
  pass through CodeInspect to CodeConcreteParse
  *)
  CharacterEncoding -> "UTF-8",
  SourceConvention -> "LineColumn",
  "TabWidth" -> 1,
  "FileFormat" -> Automatic
}


(*

There was a change in Mathematica 11.2 to allow 

foo[lints : {___Lint} : Automatic] := lints
foo[]  returns Automatic

Related bugs: 338218
*)

lintsInPat = If[$VersionNumber >= 11.2, {___InspectionObject}, _]

CodeInspectSummarize[File[file_String], lintsIn:lintsInPat:Automatic, opts:OptionsPattern[]] :=
Catch[
Module[{lints, full, lines,
  tagExclusions, severityExclusions, confidence, lintLimit,
  lintedLines, bytes, str, tabWidth},

  lints = lintsIn;

  tagExclusions = OptionValue["TagExclusions"];
  severityExclusions = OptionValue["SeverityExclusions"];
  confidence = OptionValue[ConfidenceLevel];
  lintLimit = OptionValue["LintLimit"];

  tabWidth = OptionValue["TabWidth"];

  full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <|"FileName"->file|>]]
  ];

   If[FileByteCount[full] == 0,
   Throw[Failure["EmptyFile", <|"FileName"->full|>]]
   ];

  If[lints === Automatic,
    lints = CodeInspect[File[full], FilterRules[{opts}, Options[CodeInspect]]];
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

  lintedLines = lintLinesReport[lines, lints, tagExclusions, severityExclusions, confidence, lintLimit];

  If[lintedLines == {},
    lintedLines = {
      InspectedLineObject[{
        Column[{
          Text["Settings:"],
          ConfidenceLevel -> confidence,
          "LintLimit" -> lintLimit,
          "TagExclusions" -> tagExclusions,
          "SeverityExclusions" -> severityExclusions
        }]
      }],
      InspectedLineObject[{}],
      InspectedLineObject[{}],
      InspectedLineObject[{Text["No issues."]}]
    }
    ,
    lintedLines = {
      InspectedLineObject[{
        Column[{
          Text["Settings:"],
          ConfidenceLevel -> confidence,
          "LintLimit" -> lintLimit,
          "TagExclusions" -> tagExclusions,
          "SeverityExclusions" -> severityExclusions
        }]
      }],
      InspectedLineObject[{}],
      InspectedLineObject[{}]
    } ~Join~ lintedLines
  ];

  InspectedFileObject[full, lintedLines]
]]


(*
Allow lints themselves to be summarized

Since we have an explicit lint that we want to summarize, then make sure that filtering options
do not interfere with summarizing
*)
CodeInspectSummarize[lint:InspectionObject[_, _, _, KeyValuePattern["File" -> _]], opts:OptionsPattern[]] :=
  Module[{file},

    file = lint[[4, Key["File"]]];

    CodeInspectSummarize[File[file], {lint}, opts, "SeverityExclusions" -> {}, "TagExclusions" -> {}, ConfidenceLevel -> 0.0, "LintLimit" -> Infinity]
  ]



CodeInspectSummarize[string_String, lintsIn:lintsInPat:Automatic, opts:OptionsPattern[]] :=
Catch[
 Module[{lints, lines,
  tagExclusions, severityExclusions, confidence, lintLimit,
  lintedLines, tabWidth},

 lints = lintsIn;

 tagExclusions = OptionValue["TagExclusions"];
 severityExclusions = OptionValue["SeverityExclusions"];
 confidence = OptionValue[ConfidenceLevel];
 lintLimit = OptionValue["LintLimit"];

 tabWidth = OptionValue["TabWidth"];

 If[StringLength[string] == 0,
  Throw[Failure["EmptyString", <||>]]
 ];

 If[lints === Automatic,
    lints = CodeInspect[string, FilterRules[{opts}, Options[CodeInspect]]]
  ];

  lines = StringSplit[string, {"\r\n", "\n", "\r"}, All];

  lines = replaceTabs[#, 1, "!", tabWidth]& /@ lines;

  lintedLines = lintLinesReport[lines, lints, tagExclusions, severityExclusions, confidence, lintLimit];

  If[lintedLines == {},
    lintedLines = {
      InspectedLineObject[{
        Column[{
          Text["Settings:"],
          ConfidenceLevel -> confidence,
          "LintLimit" -> lintLimit,
          "TagExclusions" -> tagExclusions,
          "SeverityExclusions" -> severityExclusions
        }]
      }],
      InspectedLineObject[{}],
      InspectedLineObject[{}],
      InspectedLineObject[{Text["No issues."]}]
    }
    ,
    lintedLines = {
      InspectedLineObject[{
        Column[{
          Text["Settings:"],
          ConfidenceLevel -> confidence,
          "LintLimit" -> lintLimit,
          "TagExclusions" -> tagExclusions,
          "SeverityExclusions" -> severityExclusions
        }]
      }],
      InspectedLineObject[{}],
      InspectedLineObject[{}]
    } ~Join~ lintedLines
  ];

  InspectedStringObject[string, lintedLines]
]]





CodeInspectSummarize[bytes_List, lintsIn:lintsInPat:Automatic, opts:OptionsPattern[]] :=
Catch[
 Module[{lints, lines,
  tagExclusions, severityExclusions, confidence, lintLimit,
  lintedLines, string, tabWidth},

 lints = lintsIn;

 tagExclusions = OptionValue["TagExclusions"];
 severityExclusions = OptionValue["SeverityExclusions"];
 confidence = OptionValue[ConfidenceLevel];
 lintLimit = OptionValue["LintLimit"];

 tabWidth = OptionValue["TabWidth"];

 If[lints === Automatic,
    lints = CodeInspect[bytes, FilterRules[{opts}, Options[CodeInspect]]];
  ];

  string = SafeString[bytes];

  lines = StringSplit[string, {"\r\n", "\n", "\r"}, All];

  lines = replaceTabs[#, 1, "!", tabWidth]& /@ lines;

  lintedLines = lintLinesReport[lines, lints, tagExclusions, severityExclusions, confidence, lintLimit];

  If[lintedLines == {},
    lintedLines = {
      InspectedLineObject[{
        Column[{
          Text["Settings:"],
          ConfidenceLevel -> confidence,
          "LintLimit" -> lintLimit,
          "TagExclusions" -> tagExclusions,
          "SeverityExclusions" -> severityExclusions
        }]
      }],
      InspectedLineObject[{}],
      InspectedLineObject[{}],
      InspectedLineObject[{Text["No issues."]}]
    }
    ,
    lintedLines = {
      InspectedLineObject[{
        Column[{
          Text["Settings:"],
          ConfidenceLevel -> confidence,
          "LintLimit" -> lintLimit,
          "TagExclusions" -> tagExclusions,
          "SeverityExclusions" -> severityExclusions
        }]
      }],
      InspectedLineObject[{}],
      InspectedLineObject[{}]
    } ~Join~ lintedLines
  ];

  InspectedBytesObject[bytes, lintedLines]
]]



InspectionObject::sourceless = "There are InspectionObjects without Source data. This can happen when some abstract syntax is inspected. \
These InspectionObjects cannot be reported. `1`"

InspectedLineObject::truncation = "Truncation limit reached. Inspected line may not display properly."


(*
Return a list of LintedLines
*)
lintLinesReport[linesIn:{___String}, lintsIn:{___InspectionObject}, tagExclusions_, severityExclusions_, confidence_, lintLimit_] :=
Catch[
Module[{lints, lines, sources, warningsLines,
  linesToModify, maxLineNumberLength, lintsPerColumn, sourceLessLints, toRemove, startingPoint, startingPointIndex, elidedLines,
  additionalSources, truncated, environLines, environLinesTentative},
  
  lints = lintsIn;
  If[$Debug,
    Print["lints: ", lints];
  ];

  (*
  in the course of abstracting syntax, Source information may be lost
  Certain Lints may not have Source information attached
  That is fine, but those Lints cannot be reported
  *)
  sourceLessLints = Cases[lints, InspectionObject[_, _, _, data_ /; !MemberQ[Keys[data], Source]]];

  (*
  If[!empty[sourceLessLints],
    Message[Lint::sourceless, sourceLessLints]
  ];
  *)
  
  lints = Complement[lints, sourceLessLints];
  If[$Debug,
    Print["lints: ", lints];
  ];

  If[empty[lints],
    Throw[{}]
  ];

  lines = linesIn;

  If[AnyTrue[lines, (StringLength[#] > $LineTruncationLimit)&],
    truncated = True;
  ];

  lines = StringTake[#, UpTo[$LineTruncationLimit]]& /@ lines;


  lints = filterLints[lints, tagExclusions, severityExclusions, confidence, lintLimit];

  If[empty[lints],
    Throw[{}]
  ];

  (*
  These are the lints we will be working with
  *)

  If[truncated,
    Message[InspectedLineObject::truncation]
  ];

   sources = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[Source -> src_]] :> src];

   additionalSources = Join @@ Cases[lints, InspectionObject[_, _, _, KeyValuePattern["AdditionalSources" -> srcs_]] :> srcs];

   sources = sources ~Join~ additionalSources;

    (*
    sources = DeleteCases[sources, {{line1_, _}, {_, _}} /; MemberQ[Keys[lineNumberExclusions], line1]];
    *)

    If[empty[sources],
      Throw[{}]
    ];

   warningsLines = sources[[All, All, 1]];

   If[$Debug,
    Print["warningsLines: ", warningsLines];
   ];

   environLinesTentative = Clip[(# + {-$EnvironBuffer, $EnvironBuffer}), {1, Length[lines]}]& /@ warningsLines;
   environLinesTentative = Range @@@ environLinesTentative;

   elidedLines = {};

   linesToModify = Range @@@ warningsLines;

   environLines = MapThread[Complement, {environLinesTentative, linesToModify}];

   linesToModify = MapThread[Union, {linesToModify, environLines}];

   If[$Debug,
    Print["linesToModify before: ", linesToModify];
   ];

   linesToModify = (
      If[Length[#] > $LintedLineLimit,
        toRemove = Length[#] - $LintedLineLimit;
        startingPointIndex = Floor[Length[#]/2] - Floor[toRemove/2] + 1;
        startingPoint = #[[startingPointIndex]];
        AppendTo[elidedLines, startingPoint];
        If[toRemove == 1,
          (* if only removing 1 line, then that single line will be changed to display as "...", so do not need to remove anything *)
          #
          ,
          Drop[#, (startingPointIndex+1);;(startingPointIndex+toRemove-1)]]
        ,
        #
      ])& /@ linesToModify;

   linesToModify = Union[Flatten[linesToModify]];
   environLines = Union[Flatten[environLines]];

   If[$Debug,
    Print["linesToModify after: ", linesToModify];
    Print["elidedLines: ", elidedLines];
    Print["environLines: ", environLines];
   ];

   maxLineNumberLength = Max[IntegerLength /@ linesToModify];

   Table[

      If[!MemberQ[elidedLines, i],
        With[
          {lintsPerColumn = createLintsPerColumn[lines[[i]], lints, i, "EndOfFile" -> (i == Length[lines])]}
          ,
          {lineSource = lines[[i]],
            lineNumber = i,
            lineList = ListifyLine[lines[[i]],
              lintsPerColumn, "EndOfFile" -> (i == Length[lines])],
            underlineList = createUnderlineList[lines[[i]], i, lintsPerColumn, "EndOfFile" -> (i == Length[lines])],
            lints = Union[Flatten[Values[lintsPerColumn]]],
            environ = MemberQ[environLines, i]
          }
          ,
          InspectedLineObject[lineSource, lineNumber, { lineList, underlineList }, lints, "MaxLineNumberLength" -> maxLineNumberLength, "Environ" -> environ]
        ]
        ,
        (* elided *)
        With[{environ = MemberQ[environLines, i]},
          InspectedLineObject["", i, {}, {}, "MaxLineNumberLength" -> maxLineNumberLength, "Elided" -> True, "Environ" -> environ]
        ]
      ]
    ,
    {i, linesToModify}
    ]
]]





Options[createUnderlineList] = {
  (*
  Is this line the EndOfFile line?
  *)
  "EndOfFile" -> False
}

createUnderlineList[lineIn_String, lineNumber_Integer, lintsPerColumnIn_Association, opts:OptionsPattern[]] :=
Catch[
 Module[{under, lintsPerColumn, endOfFile, lineIsEmpty, startChar, endChar, startMarker, endMarker, markupPerColumn, line},

  line = lineIn;

  lineIsEmpty = (line == "");

  lintsPerColumn = lintsPerColumnIn;

  If[$Debug,
    Print["lintsPerColumn: ", lintsPerColumn];
  ];

  endOfFile = OptionValue["EndOfFile"];

  markupPerColumn = KeyValueMap[
                      Function[{column, lints},
                        column -> LintMarkup[
                          If[isFirstError[lints, lineNumber, column], LintErrorIndicatorCharacter, LintErrorContinuationIndicatorCharacter],
                          FontWeight->CodeInspector`Format`$LintGridFontWeight, FontColor->severityColor[lints]]
                      ]
                      ,
                      lintsPerColumn
                    ];
  markupPerColumn = Association[markupPerColumn];

  If[$Debug,
    Print["markupPerColumn: ", markupPerColumn];
  ];

  If[KeyExistsQ[lintsPerColumn, 0],
    startChar = lintsPerColumn[0];

    If[$Debug,
      Print["startChar: ", startChar];
    ];

    (*
   Mark hitting EOF with \[FilledSquare]
   *)
    startMarker = If[endOfFile, LintEOFCharacter, LintContinuationCharacter];
    AssociateTo[markupPerColumn, 0 -> LintMarkup[startMarker, FontWeight->CodeInspector`Format`$LintGridFontWeight, FontColor->severityColor[startChar]]];
  ];

  (*
  If the line is empty and already added a start continuation, then don't add an end continuation

  This ensures a single \[Continuation] on blank lines
  *)
  Which[
    KeyExistsQ[lintsPerColumn, StringLength[line]+1] && !(lineIsEmpty && KeyExistsQ[lintsPerColumn, 0]),
      endChar = lintsPerColumn[StringLength[line]+1];

      If[$Debug,
        Print["endChar: ", endChar];
      ];

      endMarker = LintContinuationCharacter;
      AssociateTo[markupPerColumn, StringLength[line]+1 -> LintMarkup[endMarker, FontWeight->CodeInspector`Format`$LintGridFontWeight, FontColor->severityColor[endChar]]];
    ,
    True,
      KeyDropFrom[markupPerColumn, StringLength[line]+1]
  ];

  If[$Debug,
    Print["markupPerColumn: ", markupPerColumn];
  ];

  under = Table[LintSpaceIndicatorCharacter, {StringLength[line]}];
  
  under = Join[{" "}, under, {" "}];

  markupPerColumn = KeyMap[(#+1)&, markupPerColumn];

  markupPerColumn = Normal[markupPerColumn];

  under = ReplacePart[under, markupPerColumn];

  under
  ]
]


Options[createLintsPerColumn] = {
  "EndOfFile" -> False
}

(*
return an association col -> lints
possibly also 0 -> lints and len+1 -> lints
*)
createLintsPerColumn[line_String, lints_List, lineNumber_Integer, OptionsPattern[]] :=
Module[{perColumn, endOfFile},

  If[$Debug,
    Print["createLintsPerColumn: lineNumber: ", lineNumber];
  ];

  endOfFile = OptionValue["EndOfFile"];

  (*
  setup perColumn
  *)
  perColumn = Map[
    Module[{lint, data, srcs, start, end},
    lint = #;
    data = #[[4]];
    srcs = { data[Source] } ~Join~ Lookup[data, "AdditionalSources", {}];
    (
    Function[src,
    Switch[src,

      (* hitting EOF *)
      {{lineNumber, 0}, {lineNumber, 0}} /; endOfFile,
      Association[0 -> lint]
      ,

      (* staying within same line *)
      {{lineNumber, _}, {lineNumber, _}},
      start = src[[1, 2]];
      Which[
        start > $LineTruncationLimit,
          Association[]
        ,
        src[[2, 2]] > $LineTruncationLimit,
          end = $LineTruncationLimit;
          Association[Table[i -> lint, {i, start, end}]]
        ,
        True,
          end = Min[src[[2, 2]], $LineTruncationLimit];
          Association[dropLastButLeaveAtleastOne[Table[i -> lint, {i, start, end}]]]
      ]
      ,

      (* start on this line, but extends into next lines *)
      {{lineNumber, _}, _},
      Association[Table[i -> lint, {i, src[[1, 2]], StringLength[line]}], StringLength[line]+1 -> lint]
      ,

      (* extend from previous lines, and end on this line *)
      {_, {lineNumber, _}},
      Which[
        src[[2, 2]] > $LineTruncationLimit,
          end = $LineTruncationLimit;
          Association[0 -> lint, Table[i -> lint, {i, 1, end}]]
        ,
        True,
          end = Min[src[[2, 2]], $LineTruncationLimit];
          Association[0 -> lint, dropLastButLeaveAtleastOne[Table[i -> lint, {i, 1, end}]]]
      ]
      ,

      (* extend from previous lines, and also extend into next lines  *)
      {{lineNumber1_, _}, {lineNumber2_, _}} /; (lineNumber1 < lineNumber < lineNumber2),
      Association[0 -> lint, Table[i -> lint, {i, 1, StringLength[line]}], StringLength[line]+1 -> lint]
      ,

      (* nothing to do on this line *)
      _,
      <||>
    ]] /@ srcs)]&
    ,
    lints
  ];
  perColumn = Merge[Flatten[perColumn], Identity];

  perColumn
]



dropLastButLeaveAtleastOne[{}] := {}
dropLastButLeaveAtleastOne[{a_}] := {a}
dropLastButLeaveAtleastOne[{most___, a_, b_}] := {most, a}




Options[ListifyLine] = {
  "EndOfFile" -> False
}

(*
lineIn: the line to change
lintsPerColumn: an Association col -> lints

return a list of unchanged characters, LintedCharacters, and " "
pad with " " on either side to allow for \[Continuation] markers and \[Times] markers
*)
ListifyLine[lineIn_String, lintsPerColumnIn_Association, opts:OptionsPattern[]] :=
Module[{line, lintsPerColumn},

  If[$Debug,
    Print["ListifyLine: line: ", lineIn];
  ];

  line = lineIn;

  lintsPerColumn = lintsPerColumnIn;

  (* there may be lints in the gutters, but we do not care here *)
  KeyDropFrom[lintsPerColumn, 0];
  KeyDropFrom[lintsPerColumn, StringLength[line]+1];
  
  line = StringReplace[line, $characterReplacementRules];
  
  line = Characters[line];

  lintsPerColumn = KeyValueMap[(#1 -> LintMarkup[line[[#1]], FontWeight->CodeInspector`Format`$LintGridFontWeight, FontColor->severityColor[#2]])&, lintsPerColumn];

  line = ReplacePart[line, lintsPerColumn];

  line = Join[{" "}, line, {" "}];

  line
]



End[]

EndPackage[]
