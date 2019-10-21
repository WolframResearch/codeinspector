BeginPackage["Lint`Report`"]

ListifyLine



$DefaultTagExclusions

$DefaultSeverityExclusions


$LintedLineLimit



Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]
Needs["Lint`"]
Needs["Lint`Format`"]
Needs["Lint`Utils`"]



$DefaultTagExclusions = {}

$DefaultSeverityExclusions = {"Formatting", "Remark"}

$LintedLineLimit = 10

$LintLimit = 20


$ConfidenceLevel = 0.95




LintFileReport::usage = "LintFileReport[file, lints] returns a LintedFile object."

Options[LintFileReport] = {
  "TagExclusions" -> $DefaultTagExclusions,
  "SeverityExclusions" -> $DefaultSeverityExclusions,
  "LineNumberExclusions" -> <||>,
  "LineHashExclusions" -> {},
  ConfidenceLevel :> $ConfidenceLevel
}


(*

There was a change in Mathematica 11.2 to allow 

foo[lints : {___Lint} : Automatic] := lints
foo[]  returns Automatic

bug 338218
*)


LintFileReport[file_String | File[file_String], lintsIn:{___Lint}:Automatic, OptionsPattern[]] :=
Catch[
 Module[{lints, full, lines, lineNumberExclusions, lineHashExclusions, tagExclusions, severityExclusions,
  lintedLines, unusedLineHashExclusions, hashes, confidence},

 lints = lintsIn;

 (*
  Support None for the various exclusion options
 *)
 tagExclusions = OptionValue["TagExclusions"];
 If[tagExclusions === None,
  tagExclusions = {}
 ];

 severityExclusions = OptionValue["SeverityExclusions"];
 If[severityExclusions === None,
  severityExclusions = {}
 ];

 lineNumberExclusions = OptionValue["LineNumberExclusions"];
 If[lineNumberExclusions === None,
  lineNumberExclusions = {}
 ];

 lineHashExclusions = OptionValue["LineHashExclusions"];
 If[lineHashExclusions === None,
  lineHashExclusions = {}
 ];

 confidence = OptionValue[ConfidenceLevel];

  full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <|"FileName"->file|>]]
  ];

   If[FileByteCount[full] == 0,
   Throw[Failure["EmptyFile", <|"FileName"->full|>]]
   ];

  If[lints === Automatic,
    lints = LintFile[full];
  ];



   (*
    bug 163988
    Use CharacterEncoding -> "ASCII" to guarantee that newlines are preserved
    *)
   lines = Import[full, {"Text", "Lines"}, CharacterEncoding -> "ASCII"];

   If[!empty[lineHashExclusions],
    hashes = (IntegerString[Hash[#], 16, 16])& /@ lines;
    unusedLineHashExclusions = Complement[lineHashExclusions, hashes];
    If[!empty[unusedLineHashExclusions],
      Message[LintFile::unusedLineHashExclusions, full, unusedLineHashExclusions];
    ];
  ];

  lintedLines = lintLinesReport[lines, lints, tagExclusions, severityExclusions, lineNumberExclusions, lineHashExclusions, confidence];
  LintedFile[full, lintedLines]
]]





LintStringReport::usage = "LintStringReport[string, lints] returns a LintedString object."

Options[LintStringReport] = {
  "TagExclusions" -> $DefaultTagExclusions,
  "SeverityExclusions" -> $DefaultSeverityExclusions,
  "LineNumberExclusions" -> <||>,
  "LineHashExclusions" -> {},
  ConfidenceLevel :> $ConfidenceLevel
}


LintStringReport[string_String, lintsIn:{___Lint}:Automatic, OptionsPattern[]] :=
Catch[
 Module[{lints, lines, lineNumberExclusions, lineHashExclusions, tagExclusions, severityExclusions, lintedLines, confidence},

 lints = lintsIn;

 (*
  Support None for the various exclusion options
 *)
 tagExclusions = OptionValue["TagExclusions"];
 If[tagExclusions === None,
  tagExclusions = {}
 ];

 severityExclusions = OptionValue["SeverityExclusions"];
 If[severityExclusions === None,
  severityExclusions = {}
 ];

 lineNumberExclusions = OptionValue["LineNumberExclusions"];
 If[lineNumberExclusions === None,
  lineNumberExclusions = {}
 ];

 lineHashExclusions = OptionValue["LineHashExclusions"];
 If[lineHashExclusions === None,
  lineHashExclusions = {}
 ];

 confidence = OptionValue[ConfidenceLevel];


 If[StringLength[string] == 0,
  Throw[Failure["EmptyString", <||>]]
 ];

 If[lints === Automatic,
    lints = LintString[string];
  ];


 (*
    bug 163988
    Use CharacterEncoding -> "ASCII" to guarantee that newlines are preserved
    *)
  lines = ImportString[string, {"Text", "Lines"}, CharacterEncoding -> "ASCII"];

  lintedLines = lintLinesReport[lines, lints, tagExclusions, severityExclusions, lineNumberExclusions, lineHashExclusions, confidence];
  LintedString[string, lintedLines]
]]






Lint::sourceless = "There are Lints without Source data. This can happen when some abstract syntax is linted. \
These Lints cannot be reported. `1`"



(*
Return a list of LintedLines
*)
lintLinesReport[linesIn:{___String}, lintsIn:{___Lint}, tagExclusions_List, severityExclusions_List, lineNumberExclusionsIn_Association, lineHashExclusionsIn_List, confidence_] :=
Catch[
Module[{lints, lines, hashes, lineNumberExclusions, lineHashExclusions, lintsExcludedByLineNumber, tmp, sources, warningsLines,
  linesToModify, maxLineNumberLength, lintsPerColumn, sourceLessLints, toRemove, startingPoint, startingPointIndex, elidedLines,
  additionalSources, shadowing, confidenceTest, existsTest, badLints},
  
  lints = lintsIn;
  If[$Debug,
    Print["lints: ", lints];
  ];

  (*
  in the course of abstracting syntax, Source information may be lost
  Certain Lints may not have Source information attached
  That is fine, but those Lints cannot be reported
  *)
  sourceLessLints = Cases[lints, Lint[_, _, _, data_ /; !MemberQ[Keys[data], Source]]];

  If[!empty[sourceLessLints],
    Message[Lint::sourceless, sourceLessLints]
  ];

  lints = Complement[lints, sourceLessLints];
  If[$Debug,
    Print["lints: ", lints];
  ];

  If[empty[lints],
    Throw[{}]
  ];

  (*
  Add a fake line.

  Syntax errors may continue to the end of the file (EOF), and the source location of EOF is {lastLine+1, 0}.
  i.e., it is after all content in the file.

  Also,
  ImportString["\n", {"Text", "Lines"}, CharacterEncoding -> "ASCII"] returns {""} and I need it to return {"", ""}
  That is, there are implied lines both *before* and *after* a \n.
  So just fudge it and add a blank line. This gets us in sync with expectations of source locations in other editors, etc.
  discussed at length in bug 363161

  We want to hash the fake line that is added at the end.
  *)
  lines = linesIn;
  lines = Append[lines, ""];

  hashes = (IntegerString[Hash[#], 16, 16])& /@ lines;

  lineNumberExclusions = lineNumberExclusionsIn;

  lineNumberExclusions = expandLineNumberExclusions[lineNumberExclusions];


  lineHashExclusions = lineHashExclusionsIn;

  (* Association of lineNumber -> All *)
  tmp = Association[Table[If[MemberQ[lineHashExclusions, hashes[[i]]], i -> All, Nothing], {i, 1, Length[lines]}]];
  lineNumberExclusions = lineNumberExclusions ~Join~ tmp;


  If[!empty[tagExclusions],
    lints = DeleteCases[lints, Lint[Alternatives @@ tagExclusions, _, _, _]];
    If[$Debug,
      Print["lints: ", lints];
    ];

    If[empty[lints],
      Throw[{}]
    ];
  ];

  If[!empty[severityExclusions],
    lints = DeleteCases[lints, Lint[_, _, Alternatives @@ severityExclusions, _]];
    If[$Debug,
      Print["lints: ", lints];
    ];

    If[empty[lints],
      Throw[{}]
    ];
  ];

  (*
  lints that match the line numbers and tags in lineNumberExclusions
  *)
  lintsExcludedByLineNumber = Catenate[KeyValueMap[Function[{line, tags},
      If[tags === All,
        Cases[lints, Lint[_, _, _, KeyValuePattern[Source -> {{line1_ /; line1 == line, _}, {_, _}}]]]
        ,
        Cases[lints, Lint[tag_ /; MemberQ[tags, tag], _, _, KeyValuePattern[Source -> {{line1_ /; line1 == line, _}, {_, _}}]]]
      ]
    ],
    lineNumberExclusions]];

  lints = Complement[lints, lintsExcludedByLineNumber];
  If[$Debug,
    Print["lints: ", lints];
  ];

  If[empty[lints],
    Throw[{}]
  ];



  existsTest = Not @* KeyExistsQ[ConfidenceLevel];
  badLints = Cases[lints, Lint[_, _, _, data_?existsTest]];
  If[!empty[badLints],
    Message[Lint::confidence, badLints]
  ];

  confidenceTest = GreaterEqualThan[confidence];
  lints = Cases[lints, Lint[_, _, _, KeyValuePattern[ConfidenceLevel -> c_?confidenceTest]]];

  (*
  If a Fatal lint and an Error lint both have the same Source, then only keep the Fatal lint
  *)
  shadowing = Select[lints, Function[lint, AnyTrue[lints, shadows[lint, #]&]]];

  If[$Debug,
    Print["shadowing: ", shadowing];
  ];

  lints = Complement[lints, shadowing];
  If[$Debug,
    Print["lints: ", lints];
  ];

  If[Length[lints] > $LintLimit,
    lints = Take[lints, $LintLimit];
    If[$Debug,
      Print["lints: ", lints];
    ];
  ];

   sources = Cases[lints, Lint[_, _, _, KeyValuePattern[Source -> src_]] :> src];

   additionalSources = Join @@ Cases[lints, Lint[_, _, _, KeyValuePattern["AdditionalSources" -> srcs_]] :> srcs];

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

   elidedLines = {};

   linesToModify = Range @@@ warningsLines;

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

   If[$Debug,
    Print["linesToModify after: ", linesToModify];
    Print["elidedLines: ", elidedLines];
   ];

   maxLineNumberLength = Max[IntegerLength /@ linesToModify];

   Table[

      If[!MemberQ[elidedLines, i],
        With[
          {lintsPerColumn = createLintsPerColumn[lines[[i]], lints, i, "EndOfFile" -> (i == Length[lines])]}
          ,
          {lineSource = lines[[i]], lineNumber = i, hash = hashes[[i]],
            lineList = ListifyLine[lines[[i]], lintsPerColumn, "EndOfFile" -> (i == Length[lines])],
            underlineList = createUnderlineList[lines[[i]], lintsPerColumn, "EndOfFile" -> (i == Length[lines])],
            lints = Union[Flatten[Values[lintsPerColumn]]]
          }
          ,
          LintedLine[lineSource, lineNumber, hash, { lineList, underlineList }, lints, "MaxLineNumberLength" -> maxLineNumberLength]
        ]
        ,
        LintedLine["", i, "", {}, {}, "MaxLineNumberLength" -> maxLineNumberLength, "Elided" -> True]
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

createUnderlineList[line_String, lintsPerColumnIn_Association, opts:OptionsPattern[]] :=
Catch[
 Module[{under, lintsPerColumn, endOfFile, lineIsEmpty, startChar, endChar, startMarker, endMarker, markupPerColumn,
  firstError, indicator},

 lineIsEmpty = (line == "");

  lintsPerColumn = lintsPerColumnIn;

  If[$Debug,
    Print["lintsPerColumn: ", lintsPerColumn];
  ];

  endOfFile = OptionValue["EndOfFile"];


  firstError = True;

  markupPerColumn = Map[(
                          If[firstError,
                            indicator = LintErrorIndicator;
                            firstError = False
                            ,
                            indicator = LintErrorContinuationIndicator;
                          ];
                          LintMarkup[indicator, FontWeight->Bold, FontSize->Larger, FontColor->severityColor[#]]
                        )&, lintsPerColumn];

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
    startMarker = If[endOfFile, LintEOF, LintContinuation];
    AssociateTo[markupPerColumn, 0 -> LintMarkup[startMarker, FontWeight->Bold, FontSize->Larger, FontColor->severityColor[startChar]]];
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

      endMarker = LintContinuation;
      AssociateTo[markupPerColumn, StringLength[line]+1 -> LintMarkup[endMarker, FontWeight->Bold, FontSize->Larger, FontColor->severityColor[endChar]]];
    ,
    True,
      KeyDropFrom[markupPerColumn, StringLength[line]+1]
  ];

  If[$Debug,
    Print["markupPerColumn: ", markupPerColumn];
  ];


  under = Table[LintSpaceIndicator, {StringLength[line]}];
  
  under = Join[{" "}, under, {" "}];

  markupPerColumn = KeyMap[#+1&, markupPerColumn];

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

  endOfFile = OptionValue["EndOfFile"];

  (*
  setup perColumn
  *)
  perColumn = Map[
    Module[{lint, data, srcs},
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
      Association[Table[i -> lint, {i, src[[1, 2]], src[[2, 2]]}]]
      ,

      (* start on this line, but extends into next lines *)
      {{lineNumber, _}, _},
      Association[Table[i -> lint, {i, src[[1, 2]], StringLength[line]}], StringLength[line]+1 -> lint]
      ,

      (* extend from previous lines, and end on this line *)
      {_, {lineNumber, _}},
      Association[0 -> lint, Table[i -> lint, {i, 1, src[[2, 2]]}]]
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

  line = lineIn;

  lintsPerColumn = lintsPerColumnIn;

  (* there may be lints in the gutters, but we do not care here *)
  KeyDropFrom[lintsPerColumn, 0];
  KeyDropFrom[lintsPerColumn, StringLength[line]+1];


  line = Characters[line];
  (*
  We want everything to be 1 character wide.
  This keeps things simple
  *)
  line = ReplaceAll[line, "\t" -> " "];

  lintsPerColumn = KeyValueMap[#1 -> LintMarkup[line[[#1]], FontWeight->Bold, FontColor->severityColor[#2]]&, lintsPerColumn];

  line = ReplacePart[line, lintsPerColumn];

  line = Join[{" "}, line, {" "}];

  line
]



End[]

EndPackage[]
