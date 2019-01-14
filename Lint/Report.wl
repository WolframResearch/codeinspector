BeginPackage["Lint`Report`"]

ListifyLine



Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]
Needs["Lint`"]
Needs["Lint`Format`"]
Needs["Lint`Utils`"]


Options[LintFileReport] = {
  "TagExclusions" -> {},
  "SeverityExclusions" -> {"Remark"},
  "LineNumberExclusions" -> <||>,
  "LineHashExclusions" -> {}
}


(*
cannot have
LintFileReport[file_String, opts:OptionsPattern[]]

because LintFileReport[file, {}] leads to infinite recursion
*)


LintFileReport[file_String, lints:{___Lint}, OptionsPattern[]] :=
Catch[
 Module[{full, lines, lineNumberExclusions, lineHashExclusions, tagExclusions, endsWithNewline, severityExclusions},

 tagExclusions = OptionValue["TagExclusions"];
 severityExclusions = OptionValue["SeverityExclusions"];
 lineNumberExclusions = OptionValue["LineNumberExclusions"];
 lineHashExclusions = OptionValue["LineHashExclusions"];

  full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <|"FileName"->file|>]]
  ];

   If[FileByteCount[full] == 0,
   Throw[Failure["EmptyFile", <|"FileName"->full|>]]
   ];

   (*
    bug 163988
    Use CharacterEncoding -> "ASCII" to guarantee that newlines are preserved
    *)
   lines = Import[full, {"Text", "Lines"}, CharacterEncoding -> "ASCII"];

   (*
  Add a fake line

  ImportString["\n", {"Text", "Lines"}, CharacterEncoding -> "ASCII"] returns {""} and I argue that it should return {"", ""}
  That is, there are implied lines both *before* and *after* a \n.
  So just fudge it and add a blank line. This gets us in sync with expectations of source locations in other editors, etc.
  bug ?
   *)
  endsWithNewline = (Import[full, {"Byte", -1}] == 10);
  If[endsWithNewline,
    lines = Append[lines, ""];
  ];

  lintLinesReport[lines, lints, tagExclusions, severityExclusions, lineNumberExclusions, lineHashExclusions]
]]


Options[LintStringReport] = {
  "TagExclusions" -> {},
  "SeverityExclusions" -> {"Remark"},
  "LineNumberExclusions" -> <||>,
  "LineHashExclusions" -> {}
}

(*
cannot have
LintStringReport[string, opts:OptionsPattern[]]

because LintStringReport[string, {}] leads to infinite loop
*)

LintStringReport[string_String, lints:{___Lint}, OptionsPattern[]] :=
Catch[
 Module[{lines, lineNumberExclusions, lineHashExclusions, tagExclusions, endsWithNewline, severityExclusions},


 tagExclusions = OptionValue["TagExclusions"];
 severityExclusions = OptionValue["SeverityExclusions"];
 lineNumberExclusions = OptionValue["LineNumberExclusions"];
 lineHashExclusions = OptionValue["LineHashExclusions"];

 If[StringLength[string] == 0,
  Throw[Failure["EmptyString", <||>]]
 ];

 (*
    bug 163988
    Use CharacterEncoding -> "ASCII" to guarantee that newlines are preserved
    *)
  lines = ImportString[string, {"Text", "Lines"}, CharacterEncoding -> "ASCII"];

  (*
  Add a fake line

  ImportString["\n", {"Text", "Lines"}, CharacterEncoding -> "ASCII"] returns {""} and I argue that it should return {"", ""}
  That is, there are implied lines both *before* and *after* a \n.
  So just fudge it and add a blank line. This gets us in sync with expectations of source locations in other editors, etc.
   *)
  endsWithNewline = (ImportString[string, {"Byte", -1}] == 10);
  If[endsWithNewline,
    lines = Append[lines, ""];
  ];

  lintLinesReport[lines, lints, tagExclusions, severityExclusions, lineNumberExclusions, lineHashExclusions]
]]









lintLinesReport[linesIn_List, lintsIn:{___Lint}, tagExclusions_List, severityExclusions_List, lineNumberExclusionsIn_Association, lineHashExclusionsIn_List] :=
Catch[
Module[{lints, lines, hashes, lineNumberExclusions, lineHashExclusions, lintsExcludedByLineNumber, tmp},
  lints = lintsIn;
  
  (*
  Add a fake line.

  Syntax errors may continue to the end of the file (EOF), and the source location of EOF is {lastLine+1, 0}.
  i.e., it is after all content in the file.

  We want to hash the fake line that is added at the end.

  But we only want to truncate the lines after they have been hashed.
  It would be wrong to hash the truncated lines.
  *)
  lines = linesIn;
  lines = Append[lines, ""];
  hashes = (IntegerString[Hash[#], 16, 16])& /@ lines;
  lines = StringTake[#, UpTo[$lineWidth]]& /@ lines;

  lineNumberExclusions = lineNumberExclusionsIn;

  lineNumberExclusions = expandLineNumberExclusions[lineNumberExclusions];


  lineHashExclusions = lineHashExclusionsIn;

  (* Association of lineNumber -> All *)
  tmp = Association[Table[If[MemberQ[lineHashExclusions, hashes[[i]]], i -> All, Nothing], {i, 1, Length[lines]}]];
  lineNumberExclusions = lineNumberExclusions ~Join~ tmp;


  If[!empty[tagExclusions],
    lints = DeleteCases[lints, Lint[Alternatives @@ tagExclusions, _, _, _]];
  ];

  If[!empty[severityExclusions],
    lints = DeleteCases[lints, Lint[_, _, Alternatives @@ severityExclusions, _]];
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

  If[lints == {},
    Throw[{}]
  ];

   sources = Cases[lints, Lint[_, _, _, opts_] :> opts[Source]];

    (*
    sources = DeleteCases[sources, {{line1_, _}, {_, _}} /; MemberQ[Keys[lineNumberExclusions], line1]];
    *)

    If[empty[sources],
      Throw[{}]
    ];

   warningsLines = sources[[All, All, 1]];

   linesToModify = Union[Take[Sort[Flatten[Range @@@ warningsLines]], UpTo[$lineLimit]]];

   maxLineNumberLength = Max[IntegerLength /@ linesToModify];

   Table[

   	lintsPerColumn = createLintsPerColumn[lines[[i]], lints, i, "EndOfFile" -> (i == Length[lines])];

    	LintedLine[lines[[i]], i, hashes[[i]], { ListifyLine[lines[[i]], lintsPerColumn, "EndOfFile" -> (i == Length[lines])],
    											createUnderlineList[lines[[i]], lintsPerColumn, "EndOfFile" -> (i == Length[lines])]},
    				Union[Flatten[Values[lintsPerColumn]]], "MaxLineNumberLength" -> maxLineNumberLength]
    ,
    {i, linesToModify}
    ]
]]







$lineWidth = 500
$lineLimit = 100


Options[createUnderlineList] = {
  (*
  Is this line the EndOfFile line?
  *)
  "EndOfFile" -> False
}

createUnderlineList[line_String, lintsPerColumnIn_Association, opts:OptionsPattern[]] :=
Catch[
 Module[{warningsCols, under, warningInserters, lintsPerColumn, sorted, endOfFile, lineIsEmpty, keys},

 lineIsEmpty = (line == "");

  lintsPerColumn = lintsPerColumnIn;

  endOfFile = OptionValue["EndOfFile"];

  lintsPerColumn = Map[LintedCharacter[LintErrorIndicator, #, FontWeight->Bold, FontSize->Larger]&, lintsPerColumn];

  If[KeyExistsQ[lintsPerColumn, 0],
  	startChar = lintsPerColumn[0];
  	(*
   Mark hitting EOF with \[FilledSquare]
   *)
  	startMarker = If[endOfFile, LintEOF, LintContinuation];
  	AssociateTo[lintsPerColumn, 0 -> LintedCharacter[startMarker, startChar[[2]], FontWeight->Bold, FontSize->Larger]];
  ];

  (*
  If the line is empty and already added a start continuation, then don't add an end continuation

  This ensures a single \[Continuation] on blank lines
  *)
  If[KeyExistsQ[lintsPerColumn, StringLength[line]+1] && !(lineIsEmpty && KeyExistsQ[lintsPerColumn, 0]),
  	endChar = lintsPerColumn[StringLength[line]+1];
  	endMarker = LintContinuation;
  	AssociateTo[lintsPerColumn, StringLength[line]+1 -> LintedCharacter[endMarker, endChar[[2]], FontWeight->Bold, FontSize->Larger]];
  	,
  	KeyDropFrom[lintsPerColumn, StringLength[line]+1]
  ];

  (* and make sure nothing past $lineWidth is left *)
  keys = Keys[lintsPerColumn];
  Scan[(If[# > $lineWidth, KeyDropFrom[lintsPerColumn, #]])&, keys];
	


  under = Table[LintSpaceIndicator, {StringLength[line]}];
  
  under = Join[{" "}, under, {" "}];

  lintsPerColumn = KeyMap[#+1&, lintsPerColumn];

  lintsPerColumn = Normal[lintsPerColumn];

  under = ReplacePart[under, lintsPerColumn];

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
    Module[{lint, data},
    lint = #;
    data = #[[4]];
    Switch[lint,

      (* hitting EOF *)
      Lint[_, _, _, KeyValuePattern[Source -> {{lineNumber, 0}, {lineNumber, 0}}]] /; endOfFile,
      Association[0 -> lint]
      ,

      (* staying within same line *)
      Lint[_, _, _, KeyValuePattern[Source -> {{lineNumber, _}, {lineNumber, _}}]],
      Association[Table[i -> lint, {i, data[Source][[1, 2]], data[Source][[2, 2]]}]]
      ,

      (* start on this line, but extends into next lines *)
      Lint[_, _, _, KeyValuePattern[Source -> {{lineNumber, _}, _}]],
      Association[Table[i -> lint, {i, data[Source][[1, 2]], StringLength[line]}], StringLength[line]+1 -> lint]
      ,

      (* extend from previous lines, and end on this line *)
      Lint[_, _, _, KeyValuePattern[Source -> {_, {lineNumber, _}}]],
      Association[0 -> lint, Table[i -> lint, {i, 1, data[Source][[2, 2]]}]]
      ,

      (* extend from previous lines, and also extend into next lines  *)
      Lint[_, _, _, KeyValuePattern[Source -> {{lineNumber1_, _}, {lineNumber2_, _}} /; (lineNumber1 < lineNumber < lineNumber2)]],
      Association[0 -> lint, Table[i -> lint, {i, 1, StringLength[line]}], StringLength[line]+1 -> lint]
      ,

      (* nothing to do on this line *)
      _,
      <||>
    ]]&
    ,
    lints
  ];
  perColumn = Merge[perColumn, Identity];

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
Module[{line, lintsPerColumn, keys},
  line = lineIn;

  lintsPerColumn = lintsPerColumnIn;

  (* there may be lints in the gutters, but we do not care here *)
  KeyDropFrom[lintsPerColumn, 0];
  KeyDropFrom[lintsPerColumn, StringLength[line]+1];
  
  (* and make sure nothing past $lineWidth is left *)
  keys = Keys[lintsPerColumn];
  Scan[(If[# > $lineWidth, KeyDropFrom[lintsPerColumn, #]])&, keys];


  line = Characters[line];
  (*
  We want everything to be 1 character wide.
  This keeps things simple
  *)
  line = ReplaceAll[line, "\t" -> " "];

  lintsPerColumn = KeyValueMap[#1 -> LintedCharacter[line[[#1]], #2, FontWeight->Bold]&, lintsPerColumn];

  line = ReplacePart[line, lintsPerColumn];

  line = Join[{" "}, line, {" "}];

  line
]



End[]

EndPackage[]