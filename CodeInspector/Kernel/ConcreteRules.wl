BeginPackage["CodeInspector`ConcreteRules`"]

$DefaultConcreteRules


Begin["`Private`"]

Needs["CodeParser`"]
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

(*
BinaryNode[Span, _, _] -> scanBinarySpans,
*)

(*
TernaryNode[Span, _, _] -> scanTernarySpans,
*)

(*
Tags: ImplicitTimesAcrossLines
*)
InfixNode[Times, {___, LeafNode[Token`Fake`ImplicitTimes, _, _], LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]..., LeafNode[Token`Newline, _, _], ___}, _] -> scanImplicitTimesAcrossLines,

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


(*
Attributes[scanBinarySpans] = {HoldRest}

scanBinarySpans[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, poss, i, siblingsPos, siblings},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};


  (*
  Already checked for LineCol style
  *)

  poss = Position[children, LeafNode[Token`SemiSemi, _, _]];

  If[!MatchQ[Last[children], LeafNode[Token`Fake`ImplicitAll, _, _]],
    (* something real *)
    i = poss[[1, 1]];

    i++;
    While[i < Length[children],
      Switch[children[[i]],
        LeafNode[Token`ToplevelNewline | Token`InternalNewline, _, _],
          AppendTo[issues, InspectionObject["EndOfLine", "Suspicious ``Span`` is at end of line.", "Warning",
            <| Source -> children[[ poss[[1, 1]], 3, Key[Source] ]],
               ConfidenceLevel -> 0.95 |>]
          ];
          Break[]
        ,
        LeafNode[Whitespace | Token`Comment | Token`LineContinuation, _, _],
          i++
        ,
        _,
          (*
          Some non-trivia
          *)
          Break[]
      ]
    ];
    ,
    (*
    Last[children] is ImplicitAll

    implicit All
    check sibling nodes
    *)
    siblingsPos = Most[pos];
    siblings = Extract[cst, {siblingsPos}][[1]];
    siblingsAfter = siblings[[ (Last[pos] + 1);; ]];

    Switch[siblingsAfter,
      {LeafNode[Whitespace | Token`Comment | Token`LineContinuation, _, _]..., LeafNode[Token`ToplevelNewline | Token`InternalNewline, _, _], ___},
        (*
        There is a newline after some other trivia
        *)
        AppendTo[issues, InspectionObject["EndOfLine", "Suspicious ``Span`` is at end of line.", "Warning",
          <| Source -> children[[ poss[[1, 1]], 3, Key[Source] ]],
             ConfidenceLevel -> 0.95 |>]
        ];
      ,
      {LeafNode[Whitespace | Token`Comment | Token`LineContinuation, _, _]...},
        (*
        There is only trivia.
        This could be inside of a group (and maybe not end of line) or EOF (which should be warned, but currently too hard)
        FIXME: Allow LintString["a;;"] to return a warning
        *)
        (*
        AppendTo[issues, Lint["EndOfLine", "Suspicious ``Span`` is at end of line.", "Warning",
          <| Source -> children[[ poss[[1, 1]], 3, Key[Source] ]],
             ConfidenceLevel -> 0.95 |>]
        ];*)
        Null
    ];
  ];

  issues
]]
*)


(*
Attributes[scanTernarySpans] = {HoldRest}

scanTernarySpans[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, poss, i, j},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};


  (*
  Already checked for LineCol style
  *)


  poss = Position[children, LeafNode[Token`SemiSemi, _, _]];

  i = poss[[1, 1]];
  j = poss[[2, 1]];

  i++;
  While[i < j,
    Switch[children[[i]],
      LeafNode[Token`ToplevelNewline | Token`InternalNewline, _, _],
        AppendTo[issues, InspectionObject["EndOfLine", "Suspicious ``Span`` is at end of line.", "Warning",
          <| Source -> children[[ poss[[1, 1]], 3, Key[Source] ]],
             ConfidenceLevel -> 0.95 |>]
        ];
        Break[]
      ,
      LeafNode[Whitespace | Token`Comment | Token`LineContinuation, _, _],
        i++
      ,
      _,
        (*
        Some non-trivia
        *)
        Break[]
    ]
  ];

  j++;
  While[j < Length[children],
    Switch[children[[j]],
      LeafNode[Token`ToplevelNewline, | Token`InternalNewline, _, _],
        AppendTo[issues, InspectionObject["EndOfLine", "Suspicious ``Span`` is at end of line.", "Warning",
          <| Source -> children[[ poss[[2, 1]], 3, Key[Source] ]],
             ConfidenceLevel -> 0.95 |>]
        ];
        Break[]
      ,
      LeafNode[Whitespace | Token`Comment | Token`LineContinuation, _, _],
        j++
      ,
      _,
        (*
        Some non-trivia
        *)
        Break[]
    ]
  ];

  issues
]]
*)



Attributes[scanImplicitTimesAcrossLines] = {HoldRest}

(*
This works for all Source conventions
*)
scanImplicitTimesAcrossLines[pos_List, aggIn_] :=
Catch[
Module[{agg, node, children, data, issues, srcs, i, implicitTimes},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  i = 1;

  While[i <= Length[children],

    While[i <= Length[children] && !MatchQ[children[[i]], LeafNode[Token`Fake`ImplicitTimes, _, _]],
       i++;
    ];

    If[i > Length[children],
      Break[]
    ];

    implicitTimes = children[[i]];

    i++;

    While[i <= Length[children] && MatchQ[children[[i]], LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]],
       i++;
    ];

    If[i <= Length[children] && MatchQ[children[[i]], LeafNode[Token`Newline, _, _]],
      AppendTo[srcs, implicitTimes[[3, Key[Source]]]];
      i++;
    ];
  ];

  Scan[(
    AppendTo[issues, InspectionObject["ImplicitTimesAcrossLines", "Implicit ``Times`` across lines.", "Error",
      <|Source -> #,
        ConfidenceLevel -> 0.95,
        CodeActions -> {
                  CodeAction["Insert ``*``", InsertNode, <|Source->#, "InsertionNode"->LeafNode[Token`Star, "*", <||>] |>],
                  CodeAction["Insert ``;``", InsertNode, <|Source->#, "InsertionNode"->LeafNode[Token`Semi, ";", <||>] |>],
                  CodeAction["Insert ``,``", InsertNode, <|Source->#, "InsertionNode"->LeafNode[Token`Comma, ",", <||>]|>] }
      |>]];
    )&, srcs];

  issues
]]



Attributes[scanCalls] = {HoldRest}

scanCalls[pos_List, cstIn_] :=
 Module[{cst, node, tag, children, groupSquare, groupSquareChildren, openSquare, openSquareData},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  tag = node[[1]];
  children = node[[2]];

  groupSquare = children[[1]];

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
      AppendTo[issues, InspectionObject["UnhandledCharacter", "Unhandled character: \"" <> children <> "\"", "Fatal", <| Source -> data[Source], ConfidenceLevel -> 1.0 |>]]
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
    SyntaxError`OldFESyntax,
      {InspectionObject["OldFESyntax", "Old FE syntax.", "Fatal", <|
        data,
        ConfidenceLevel -> 1.0,
        "AdditionalDescriptions" -> {"Try resaving the notebook with a newer FE."}
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



End[]


EndPackage[]
