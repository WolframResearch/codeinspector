BeginPackage["CodeInspector`ConcreteRules`"]

$DefaultConcreteRules


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Folds`"]
Needs["CodeParser`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`Utils`"]



(*

Rules are of the form: pat -> func where pat is the node pattern to match on and func is the processing function for the node.

Functions are of the form: function[pos_, ast_] where pos is the position of the node in the AST, and ast is the AST itself.
  And function must return a list of Lints. 


A rule of thumb is to make patterns as specific as possible, to offload work of calling the function.

*)

$DefaultConcreteRules = <|

PrefixNode[_, _, _] -> scanPrefixDispatch,

PostfixNode[_, _, _] -> scanPostfixDispatch,

BinaryNode[_, _, _] -> scanBinaryDispatch,

TernaryNode[_, _, _] -> scanTernaryDispatch,

(*
Tags: ImplicitTimesAcrossLines
*)
InfixNode[Times, {___, LeafNode[Token`Newline, _, _], LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]..., LeafNode[Token`Fake`ImplicitTimes, _, _], ___}, _] -> scanImplicitTimesAcrossLines,

CallNode[{_, ___, LeafNode[Token`Newline, _, _], ___}, _, _] -> scanCalls,

ErrorNode[_, _, _] -> scanErrorNodes,

SyntaxErrorNode[_, _, _] -> scanSyntaxErrorNodes,

GroupMissingCloserNode[_, _, _] -> scanGroupMissingCloserNodes,

UnterminatedGroupNode[_, _, _] -> scanUnterminatedGroupNodes,

GroupMissingOpenerNode[_, _, _] -> scanGroupMissingOpenerNodes,

KeyValuePattern[SyntaxIssues -> _] -> scanSyntaxIssues,


GroupNode[GroupParen, {
  LeafNode[Token`OpenParen, "(", _],
  LeafNode[Token`Boxes`MultiWhitespace, _, _]...,
  BoxNode[GridBox, _, _],
  LeafNode[Token`Boxes`MultiWhitespace, _, _]...,
  LeafNode[Token`CloseParen, ")", _]}, _] -> scanParenGridBox,


BoxNode[StyleBox | AdjustmentBox | FormBox, _, _] -> scanSuspiciousBoxes,


Nothing
|>



Attributes[scanPrefixDispatch] = {HoldRest}

scanPrefixDispatch[pos_List, cstIn_] :=
Catch[
Module[{cst, node, tag, issues, reaped},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];

  tag = node[[1]];

  issues = {};

  reaped =
  Reap[
  Switch[node,
    PrefixNode[_, {___, LeafNode[Token`Newline, _, _], ___}, _],
      Sow[scanPrefixMultiline[pos, cst]]
  ]
  ][[2]];

  issues = issues ~Join~ Flatten[reaped];

  issues
]]



Attributes[scanPostfixDispatch] = {HoldRest}

scanPostfixDispatch[pos_List, cstIn_] :=
Catch[
Module[{cst, node, tag, issues, reaped},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];

  tag = node[[1]];

  issues = {};

  reaped =
  Reap[
  Switch[node,
    PostfixNode[_, {___, LeafNode[Token`Newline, _, _], ___}, _],
      Sow[scanPostfixMultiline[pos, cst]]
  ]
  ][[2]];

  issues = issues ~Join~ Flatten[reaped];

  issues
]]



Attributes[scanBinaryDispatch] = {HoldRest}

scanBinaryDispatch[pos_List, cstIn_] :=
Catch[
Module[{cst, node, tag, issues, reaped},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];

  tag = node[[1]];

  issues = {};

  reaped =
  Reap[
  Switch[tag,
    Unset,
      Switch[node,
        BinaryNode[Unset, {___, LeafNode[Token`Equal, _, _], LeafNode[Whitespace | Token`Boxes`MultiWhitespace | Token`Comment, _, _] | GroupNode[Comment, _, _], (LeafNode[Whitespace | Token`Boxes`MultiWhitespace | Token`Comment, _, _] | GroupNode[Comment, _, _])..., LeafNode[Token`Dot, _, _]}, _],
          Sow[scanUnsets[pos, cst]]
      ]
  ]
  ][[2]];

  issues = issues ~Join~ Flatten[reaped];

  reaped =
  Reap[
  Switch[node,
    BinaryNode[_, {___, LeafNode[Token`Newline, _, _], ___}, _],
      Sow[scanBinaryMultiline[pos, cst]]
  ]
  ][[2]];

  issues = issues ~Join~ Flatten[reaped];

  issues
]]



Attributes[scanTernaryDispatch] = {HoldRest}

scanTernaryDispatch[pos_List, cstIn_] :=
Catch[
Module[{cst, node, tag, issues, reaped},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];

  tag = node[[1]];

  issues = {};

  reaped =
  Reap[
  Switch[tag,
    TagUnset,
      Switch[node,
        TernaryNode[TagUnset, {___, LeafNode[Token`Equal, _, _], LeafNode[Whitespace | Token`Boxes`MultiWhitespace | Token`Comment, _, _] | GroupNode[Comment, _, _], (LeafNode[Whitespace | Token`Boxes`MultiWhitespace | Token`Comment, _, _] | GroupNode[Comment, _, _])..., LeafNode[Token`Dot, _, _]}, _],
          Sow[scanTagUnsets[pos, cst]]
      ]
  ]
  ][[2]];

  issues = issues ~Join~ Flatten[reaped];

  reaped =
  Reap[
  Switch[node,
    TernaryNode[_, {___, LeafNode[Token`Newline, _, _], ___}, _],
      Sow[scanTernaryMultiline[pos, cst]]
  ]
  ][[2]];

  issues = issues ~Join~ Flatten[reaped];

  issues
]]



Attributes[scanUnsets] = {HoldRest}

(*
Related bugs: 281967
*)
scanUnsets[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, equalPos, dotPos,
  trivias},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  equalPos = Position[children, LeafNode[Token`Equal, _, _], {1}][[1]];
  dotPos = Position[children, LeafNode[Token`Dot, _, _], {1}][[1]];

  (*
  Span is only documented to work with Part
  *)
  trivias = Part[children, (equalPos[[1]])+1 ;; (dotPos[[1]])-1];

  Do[
    (*
    was "Trivia between = and ." but changed to "Whitespace between = and ." after 3/16/22 CodeTools meeting
    *)
    AppendTo[issues, InspectionObject["TriviaBetweenEqualDot", "Whitespace between ``=`` and ``.``.", "Warning",
      <| Source -> trivia[[3, Key[Source]]],
        "AdditionalDocumentationLinks" -> {{"paclet:tutorial/OperatorInputForms#16182", "Spaces to Avoid"}},
        "AdditionalDescriptions" -> {"You should avoid inserting any spaces between the different characters in composite operators such as ``=.``."},
        ConfidenceLevel -> 0.95,
        CodeActions -> {
          CodeAction["Remove whitespace", DeleteTriviaNode, <| Source -> trivia[[3, Key[Source]]] |>]}
      |>]
    ];
    ,
    {trivia, trivias}
  ];

  issues
]]


Attributes[scanTagUnsets] = {HoldRest}

scanTagUnsets[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, equalPos, dotPos,
  trivias},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  equalPos = Position[children, LeafNode[Token`Equal, _, _], {1}][[1]];
  dotPos = Position[children, LeafNode[Token`Dot, _, _], {1}][[1]];

  (*
  Span is only documented to work with Part
  *)
  trivias = Part[children, (equalPos[[1]])+1 ;; (dotPos[[1]])-1];

  Do[
    AppendTo[issues, InspectionObject["TriviaBetweenEqualDot", "Whitespace between ``=`` and ``.``.", "Warning",
      <| Source -> trivia[[3, Key[Source]]],
        "AdditionalDocumentationLinks" -> {{"paclet:tutorial/OperatorInputForms#16182", "Spaces to Avoid"}},
        "AdditionalDescriptions" -> {"You should avoid inserting any spaces between the different characters in composite operators such as ``=.``."},
        ConfidenceLevel -> 0.95,
        CodeActions -> {
          CodeAction["Remove whitespace", DeleteTriviaNode, <| Source -> trivia[[3, Key[Source]]] |>]}
      |>]
    ];
    ,
    {trivia, trivias}
  ];

  issues
]]



Attributes[scanImplicitTimesAcrossLines] = {HoldRest}

(*
This works for all Source conventions
*)
scanImplicitTimesAcrossLines[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, srcs, i},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  i = Length[children];

  While[1 <= i,

    While[1 <= i && !MatchQ[children[[i]], LeafNode[Token`Fake`ImplicitTimes, _, _]],
       i--;
    ];

    If[1 > i,
      Break[]
    ];

    i--;

    While[1 <= i && MatchQ[children[[i]], LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]],
       i--;
    ];

    If[1 <= i && MatchQ[children[[i]], LeafNode[Token`Newline, _, _]],
      AppendTo[srcs, children[[i, 3, Key[Source]]]];
      i--;
    ];
  ];

  Scan[(
    AppendTo[issues, InspectionObject["ImplicitTimesAcrossLines", "Implicit ``Times`` across lines.", "Error",
      <| Source -> #,
        ConfidenceLevel -> 0.95,
        CodeActions -> {
          CodeAction["Insert ``*``", InsertNode, <| Source -> #, "InsertionNode" -> LeafNode[Token`Star, "*", <||>] |>],
          CodeAction["Insert ``;``", InsertNode, <| Source -> #, "InsertionNode" -> LeafNode[Token`Semi, ";", <||>] |>],
          CodeAction["Insert ``,``", InsertNode, <| Source -> #, "InsertionNode" -> LeafNode[Token`Comma, ",", <||>] |>] }
      |>]];
    )&, srcs];

  issues
]]



Attributes[scanCalls] = {HoldRest}

scanCalls[pos_List, cstIn_] :=
Module[{cst, node, tag, groupSquare, groupSquareChildren, openSquare, openSquareData},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  tag = node[[1]];
  groupSquare = node[[2]];

  groupSquareChildren = groupSquare[[2]];

  openSquare = groupSquareChildren[[1]];

  openSquareData = openSquare[[3]];

  (*
  Use source of [
  *)

  {InspectionObject["CallDifferentLine", "Call is on different lines.", "Warning", <| openSquareData, ConfidenceLevel -> 0.95 |>]}
]



Attributes[scanErrorNodes] = {HoldRest}

scanErrorNodes[pos_List, cstIn_] :=
Module[{cst, node, tag, data, tagString, children, issues, multilineStrings, commaSrc},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  tag = node[[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  Switch[tag,
    Token`Error`ExpectedEqual,
      AppendTo[issues, InspectionObject["ExpectedEqual", "Expected ``=``.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`Number,
      AppendTo[issues, InspectionObject["Number", "Number parsing error.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`UnhandledCharacter,
      AppendTo[issues, InspectionObject["UnhandledCharacter", "Unhandled character: ``" <> children <> "``.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`ExpectedLetterlike,
      AppendTo[issues, InspectionObject["ExpectedLetterlike", "Expected letterlike.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`Aborted,
      AppendTo[issues, InspectionObject["Aborted", "Aborted.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`ExpectedOperand,
      AppendTo[issues, InspectionObject["ExpectedOperand", "Expected an operand.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`ExpectedTag,
      (*
      Related bugs: 413005
      *)
      AppendTo[issues, InspectionObject["ExpectedTag", "Expected a tag.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0, "AdditionalDescriptions" -> {"Tags can contain any characters that can appear in symbol names."} |>]]
    ,
    Token`Error`ExpectedFile,
      AppendTo[issues, InspectionObject["ExpectedFile", "Expected a file.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`UnsupportedCharacter,
      AppendTo[issues, InspectionObject["UnsupportedCharacter", "Unsupported character.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`UnsupportedToken,
      AppendTo[issues, InspectionObject["UnsupportedToken", "Unsupported token.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`UnexpectedCloser,
      AppendTo[issues, InspectionObject["UnexpectedCloser", "Unexpected closer.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`UnterminatedComment,
      AppendTo[issues, InspectionObject["UnterminatedComment", "Unterminated comment.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`UnterminatedString,
      AppendTo[issues, InspectionObject["UnterminatedString", "Unterminated string.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]];
      (*
      Finding the correct string with the missing quote is difficult.
      So also flag any multiline strings as a Warning
      This will help find the actual offending string
      *)
      multilineStrings = Cases[cst, LeafNode[String, _, KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]], Infinity];
      Scan[Function[s,
        (AppendTo[issues,
          InspectionObject["MultilineString", "Multiline string.", "Warning",
            (* just mark the opening quote here *)
            <| Source -> { { #[[1]], #[[2]] }, { #[[1]], #[[2]] + 1  } }, ConfidenceLevel -> 0.9 |>]])&[s[[3, Key[Source], 1]]];
        ], multilineStrings
      ];
    ,
    Token`Error`UnterminatedFileString,
      AppendTo[issues, InspectionObject["UnterminatedFileString", "Unterminated file string.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`PrefixImplicitNull,
      commaSrc = nextSrc[data[Source]];
      AppendTo[issues, InspectionObject["Comma", "Extra ``,``.", "Error", <|
        Source -> data[Source],
        ConfidenceLevel -> 1.0,
        CodeActions -> {
          CodeAction["Delete ``,``", DeleteText, <| Source -> commaSrc |>]
        }
      |>]]
    ,
    Token`Error`InfixImplicitNull,
      commaSrc = prevSrc[data[Source]];
      AppendTo[issues, InspectionObject["Comma", "Extra ``,``.", "Error", <|
        Source -> data[Source],
        ConfidenceLevel -> 1.0,
        CodeActions -> {
          CodeAction["Delete ``,``", DeleteText, <| Source -> commaSrc |>]
        }
      |>]]
    ,
    Token`Error`OldFESyntax,
      AppendTo[issues, InspectionObject["OldFESyntax", "Old FE syntax.", "Fatal", <|
        Source -> data[Source],
        ConfidenceLevel -> 1.0,
        "AdditionalDescriptions" -> {"Try resaving the notebook with a newer FE."}
      |>]]
    ,
    _,
      tagString = Block[{$ContextPath = {"Token`Error`", "System`"}, $Context = "CodeInspector`Scratch`"}, ToString[tag]];
      AppendTo[issues, InspectionObject[tagString, "Syntax error.", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
  ];

  issues
]


nextSrc[{{line_, col_}, {line_, col_}}] :=
  {{line, col}, {line, col + 1}}

nextSrc[Before[src_]] :=
  src

prevSrc[{{line_, col_}, {line_, col_}}] :=
  {{line, col - 1}, {line, col}}

prevSrc[After[src_]] :=
  src



Attributes[scanSyntaxErrorNodes] = {HoldRest}

scanSyntaxErrorNodes[pos_List, cstIn_] :=
Module[{cst, node, tag, data, tagString, children},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  tag = node[[1]];
  children = node[[2]];
  data = node[[3]];

  Switch[tag,
    SyntaxError`ExpectedTilde,
      {InspectionObject["ExpectedTilde", "Expected ``~``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    SyntaxError`ExpectedSet,
      {InspectionObject["ExpectedSet", "Expected ``=`` or ``:=`` or ``=.``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    SyntaxError`ExpectedSymbol,
      {InspectionObject["ExpectedSymbol", "Expected a symbol.", "Error", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    SyntaxError`OldFESyntax,
      {InspectionObject["OldFESyntax", "Old FE syntax.", "Fatal", <|
        data,
        ConfidenceLevel -> 1.0,
        "AdditionalDescriptions" -> {"Try resaving the notebook with a newer FE."}
      |>]}
    ,
    SyntaxError`BuggyFESyntax,
      {InspectionObject["BuggyFESyntax", "Buggy FE syntax.", "Fatal", <|
        data,
        ConfidenceLevel -> 1.0
      |>]}
    ,
    _,
      tagString = Block[{$ContextPath = {"SyntaxError`", "System`"}, $Context = "CodeInspector`Scratch`"}, ToString[tag]];
      {InspectionObject[tagString, "Syntax error.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
  ]
]



Attributes[scanGroupMissingCloserNodes] = {HoldRest}

scanGroupMissingCloserNodes[pos_List, cstIn_] :=
Module[{cst, node, data, opener, openerData},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  data = node[[3]];

  (*
  Only report the opener

  The contents can be arbitrarily complex
  *)
  opener = node[[2, 1]];
  openerData = opener[[3]];

  {InspectionObject["GroupMissingCloser", "Missing closer.", "Fatal", <| openerData, ConfidenceLevel -> 1.0 |>]}
]



Attributes[scanUnterminatedGroupNodes] = {HoldRest}

scanUnterminatedGroupNodes[pos_List, cstIn_] :=
Module[{cst, node, data, opener, openerData},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  data = node[[3]];

  (*
  Only report the opener

  The contents can be arbitrarily complex
  *)
  opener = node[[2, 1]];
  openerData = opener[[3]];

  {InspectionObject["UnterminatedGroup", "Missing closer.", "Fatal", <| openerData, ConfidenceLevel -> 1.0 |>]}
]



Attributes[scanGroupMissingOpenerNodes] = {HoldRest}

scanGroupMissingOpenerNodes[pos_List, cstIn_] :=
Module[{cst, node, data, closer, closerData},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  data = node[[3]];

  (*
  Only report the closer

  The contents can be arbitrarily complex
  *)
  closer = node[[2, -1]];
  closerData = closer[[3]];

  {InspectionObject["GroupMissingOpener", "Missing opener.", "Fatal", <| closerData, ConfidenceLevel -> 1.0 |>]}
]



Attributes[scanSyntaxIssues] = {HoldRest}

(*
Just directly convert SyntaxIssues to Lints
*)
scanSyntaxIssues[pos_List, cstIn_] :=
Module[{cst, data, issues, syntaxIssues, issuesToReturn, formatIssues, encodingIssues},
  cst = cstIn;
  data = Extract[cst, {pos}][[1]];
  issues = data[SyntaxIssues];

  issuesToReturn = {};

  syntaxIssues = Cases[issues, SyntaxIssue[_, _, _, _]];

  issuesToReturn = issuesToReturn ~Join~ (InspectionObject[#[[1]], #[[2]], #[[3]], #[[4]]]& /@ syntaxIssues);

  formatIssues = Cases[issues, FormatIssue[_, _, _, _]];

  issuesToReturn = issuesToReturn ~Join~ (InspectionObject[#[[1]], #[[2]], #[[3]], #[[4]]]& /@ formatIssues);

  encodingIssues = Cases[issues, EncodingIssue[_, _, _, _]];

  issuesToReturn = issuesToReturn ~Join~ (InspectionObject[#[[1]], #[[2]], #[[3]], #[[4]]]& /@ encodingIssues);

  issuesToReturn
]



Attributes[scanParenGridBox] = {HoldRest}

scanParenGridBox[pos_List, cstIn_] :=
Module[{cst, node, tag, children, ws1, ws2, issues, data, open, close},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  tag = node[[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  {open, close, ws1, ws2} =
    Replace[children, {
        open:LeafNode[Token`OpenParen, "(", _],
        ws1Seq:LeafNode[Token`Boxes`MultiWhitespace, _, _]...,
        BoxNode[GridBox, _, _],
        ws2Seq:LeafNode[Token`Boxes`MultiWhitespace, _, _]...,
        close:LeafNode[Token`CloseParen, ")", _]
      } :> {open, close, {ws1Seq}, {ws2Seq}}
    ];

  Switch[ws1,
    (*
    all good
    *)
    {LeafNode[Token`Boxes`MultiWhitespace, "\[NoBreak]", _]},
      Null
    ,
    _,
      (*
      This is "stylistic" so make a Remark
      *)
      AppendTo[issues, InspectionObject["ParenGridBox", "``\\[NoBreak]`` should be between ( and grid box.", "Remark", <|
        Source -> open[[3, Key[Source]]],
        ConfidenceLevel -> 0.95,
        "AdditionalDescriptions" -> {"This helps prevent linebreaking from separating the ( and the grid box."},
        CodeActions -> {
          CodeAction["Insert ``\\[NoBreak]``", InsertNodeAfter, <|
            Source -> open[[3, Key[Source]]],
            "InsertionNode" -> LeafNode[Token`Boxes`MultiWhitespace, "\[NoBreak]", <||>]
          |>]
        }
      |>]]
  ];

  Switch[ws2,
    (*
    all good
    *)
    {LeafNode[Token`Boxes`MultiWhitespace, "\[NoBreak]", _]},
      Null
    ,
    _,
      (*
      This is "stylistic" so make a Remark
      *)
      AppendTo[issues, InspectionObject["ParenGridBox", "``\\[NoBreak]`` should be between grid box and ).", "Remark", <|
        Source -> close[[3, Key[Source]]],
        ConfidenceLevel -> 0.95,
        "AdditionalDescriptions" -> {"This helps prevent linebreaking from separating the grid box and the )."},
        CodeActions -> {
          CodeAction["Insert ``\\[NoBreak]``", InsertNode, <|
            Source -> close[[3, Key[Source]]],
            "InsertionNode" -> LeafNode[Token`Boxes`MultiWhitespace, "\[NoBreak]", <||>]
          |>]
        }
      |>]]
  ];

  issues
]



Attributes[scanSuspiciousBoxes] = {HoldRest}

scanSuspiciousBoxes[pos_List, cstIn_] :=
Module[{cst, node, data, tag},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  tag = node[[1]];
  data = node[[3]];

  {InspectionObject["SuspiciousBox", "Suspicious box: ``" <> ToString[tag] <> "``.", "Warning", <| data, ConfidenceLevel -> 1.0 |>]}
]



Attributes[scanPrefixMultiline] = {HoldRest}

scanPrefixMultiline[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, srcs, pairs},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  children = aggregate /@ children;

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source]]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  pairs = Partition[children, 2, 1];

  Do[

    Switch[p,
      {a_, b_} /; a[[3, Key[Source], 2, 1]] != b[[3, Key[Source], 1, 1]],
        AppendTo[srcs, p[[1, 3, Key[Source]]]];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, InspectionObject["DifferentLine", "Operands are on different lines.", "Warning",
      <| Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, srcs];

  issues
]]



Attributes[scanPostfixMultiline] = {HoldRest}

scanPostfixMultiline[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, highConfSrcs, lowConfSrcs, pairs},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  highConfSrcs = {};
  lowConfSrcs = {};

  issues = {};

  children = aggregate /@ children;

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source]]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  pairs = Partition[children, 2, 1];

  Do[

    Switch[p,
      (*
      People like putting & on another line for some reason
      *)
      {a_, b:LeafNode[Token`Amp, _, _]} /; a[[3, Key[Source], 2, 1]] != b[[3, Key[Source], 1, 1]],
        AppendTo[lowConfSrcs, p[[2, 3, Key[Source]]]]
      ,
      {a_, b_} /; a[[3, Key[Source], 2, 1]] != b[[3, Key[Source], 1, 1]],
        AppendTo[highConfSrcs, p[[2, 3, Key[Source]]]];
    ];

    ,
    {p, pairs}
  ];

  highConfSrcs = DeleteDuplicates[highConfSrcs];
  lowConfSrcs = DeleteDuplicates[lowConfSrcs];

  Scan[(
    AppendTo[issues, InspectionObject["DifferentLine", "Operands are on different lines.", "Warning",
      <| Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, highConfSrcs];

  Scan[(
    AppendTo[issues, InspectionObject["DifferentLine", "Operands are on different lines.", "Warning",
      <| Source -> #,
        ConfidenceLevel -> 0.85
      |>]];
    )&, lowConfSrcs];

  issues
]]



Attributes[scanBinaryMultiline] = {HoldRest}

scanBinaryMultiline[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, srcs, pairs},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  children = aggregate /@ children;

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source]]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  (*
  with a // b, only test the pair {a, //}
  *)
  pairs = {{children[[1]], children[[2]]}};

  Do[

    Switch[p,
      {a_, b_} /; a[[3, Key[Source], 2, 1]] != b[[3, Key[Source], 1, 1]],
        AppendTo[srcs, p[[2, 3, Key[Source]]]];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, InspectionObject["DifferentLine", "Operands are on different lines.", "Warning",
      <| Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, srcs];

  issues
]]



Attributes[scanTernaryMultiline] = {HoldRest}

scanTernaryMultiline[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, srcs, filtered, pairs},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  children = aggregate /@ children;

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source]]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  (*
  With a ~f~ b, we only want to look at {~, f} and {f, ~}
  *)
  filtered = children[[2;;-2]];

  pairs = Partition[filtered, 2, 1];

  Do[

    Switch[p,
      {a_, b_} /; a[[3, Key[Source], 2, 1]] != b[[3, Key[Source], 1, 1]],
        AppendTo[srcs, p[[1, 3, Key[Source]]]];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, InspectionObject["DifferentLine", "Operands are on different lines.", "Warning",
      <| Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, srcs];

  issues
]]



End[]


EndPackage[]
