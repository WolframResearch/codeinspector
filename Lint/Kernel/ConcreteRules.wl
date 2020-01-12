BeginPackage["Lint`ConcreteRules`"]

$DefaultConcreteRules


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]
Needs["Lint`"]
Needs["Lint`Format`"]
Needs["Lint`Utils`"]



(*

Rules are of the form: pat -> func where pat is the node pattern to match on and func is the processing function for the node.

Functions are of the form: function[pos_, ast_] where pos is the position of the node in the AST, and ast is the AST itself.
  And function must return a list of Lints. 


A rule of thumb is to make patterns as specific as possible, to offload work of calling the function.

*)

$DefaultConcreteRules = <|

PrefixNode[
  _,
  _,
  KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]] -> scanPrefixs,

PostfixNode[
  _,
  _,
  KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]] -> scanPostfixs,

(*
Tags: ImplicitTimesAcrossLines
*)
InfixNode[Times,
  children_ /; !FreeQ[children, LeafNode[Token`Fake`ImplicitTimes, _, _], 1],
  KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]] -> scanImplicitTimes,

(*
Tags: DotDifferentLine
*)
InfixNode[Dot, _,
  KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]] -> scanDots,

(*
Tags: StraySemicolon
*)
InfixNode[CompoundExpression, _,
  KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]] -> scanCompoundExpressions,

BinaryNode[Span, _, KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]] -> scanSpans,

TernaryNode[TernaryTilde, _, KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]] -> scanTernaryTildes,

CallNode[{_, ___, LeafNode[Token`Newline, _, _], ___}, _, _] -> scanCalls,


ErrorNode[_, _, _] -> scanErrorNodes,

SyntaxErrorNode[_, _, _] -> scanSyntaxErrorNodes,

GroupMissingCloserNode[_, _, _] -> scanGroupMissingCloserNodes,

KeyValuePattern[SyntaxIssues -> _] -> scanSyntaxIssues,



Nothing
|>







Attributes[scanPrefixs] = {HoldRest}

scanPrefixs[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, srcs},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source] ]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  (*
  We want to ignore trivia
  *)
  filtered = DeleteCases[children, LeafNode[Whitespace | Token`Comment | Token`Newline | Token`LineContinuation, _, _] ];

  pairs = Partition[filtered, 2, 1];

  Do[

    Switch[p,
      {a_, b_} /; a[[3, Key[Source], 2, 1]] != b[[ 3, Key[Source], 1, 1 ]],
        AppendTo[srcs, p[[2, 3, Key[Source] ]] ];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, Lint["PrefixDifferentLine", "Operands are on different lines.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, srcs];

  issues
]]







Attributes[scanPostfixs] = {HoldRest}

scanPostfixs[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, srcs},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source] ]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  (*
  We want to ignore trivia
  *)
  filtered = DeleteCases[children, LeafNode[Whitespace | Token`Comment | Token`Newline | Token`LineContinuation, _, _] ];

  pairs = Partition[filtered, 2, 1];

  Do[

    Switch[p,
      {a_, b_} /; a[[3, Key[Source], 2, 1]] != b[[ 3, Key[Source], 1, 1 ]],
        AppendTo[srcs, p[[2, 3, Key[Source] ]] ];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, Lint["PostfixDifferentLine", "Operands are on different lines.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, srcs];

  issues
]]







Attributes[scanImplicitTimes] = {HoldRest}

scanImplicitTimes[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, pairs, srcs, filtered},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source] ]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  (*
  We want to ignore trivia
  *)
  filtered = DeleteCases[children, LeafNode[Whitespace | Token`Comment | Token`Newline | Token`LineContinuation, _, _] ];

  pairs = Partition[filtered, 2, 1];

  Do[

    If[!MatchQ[p, {_, LeafNode[Token`Fake`ImplicitTimes, _, _]} |
                    {LeafNode[Token`Fake`ImplicitTimes, _, _], _}],
      Continue[]
    ];

    Switch[p,
      {n_, i:LeafNode[Token`Fake`ImplicitTimes, _, _]} /; n[[3, Key[Source], 2, 1]] != i[[ 3, Key[Source], 1, 1 ]],
        AppendTo[srcs, p[[2, 3, Key[Source] ]] ];
      ,
      {i:LeafNode[Token`Fake`ImplicitTimes, _, _], n_} /; i[[3, Key[Source], 2, 1]] != n[[ 3, Key[Source], 1, 1 ]],
        AppendTo[srcs, p[[1, 3, Key[Source] ]] ];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, Lint["ImplicitTimesAcrossLines", "Implicit ``Times`` across lines.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.95,
        CodeActions -> {
                  CodeAction["Insert ``;``", InsertNode, <|Source->#, "InsertionNode"->LeafNode[Token`Semi, ";", <||>] |>],
                  CodeAction["Insert ``,``", InsertNode, <|Source->#, "InsertionNode"->LeafNode[Token`Comma, ",", <||>]|>] }
      |>]];
    )&, srcs];

  issues
]]





Attributes[scanDots] = {HoldRest}

scanDots[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, srcs},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source] ]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  (*
  We want to ignore trivia
  *)
  filtered = DeleteCases[children, LeafNode[Whitespace | Token`Comment | Token`Newline | Token`LineContinuation, _, _] ];

  pairs = Partition[filtered, 2, 1];

  Do[
    If[!MatchQ[p, {_, LeafNode[Token`Dot, _, _]} |
                    {LeafNode[Token`Dot, _, _], _}],
      Continue[]
    ];
    Switch[p,
      {n_, i:LeafNode[Token`Dot, _, _]} /; n[[3, Key[Source], 2, 1]] != i[[ 3, Key[Source], 1, 1 ]],
        AppendTo[srcs, p[[2, 3, Key[Source] ]] ];
      ,
      {i:LeafNode[Token`Dot, _, _], n_} /; i[[3, Key[Source], 2, 1]] != n[[ 3, Key[Source], 1, 1 ]],
        AppendTo[srcs, p[[1, 3, Key[Source] ]] ];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, Lint["DotDifferentLine", "Operands for ``.`` are on different lines.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, srcs];

  issues
]]




Attributes[scanCompoundExpressions] = {HoldRest}

scanCompoundExpressions[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, pairs, filtered, srcs},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source] ]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  (*
  We want to ignore trivia
  *)
  filtered = DeleteCases[children, LeafNode[Whitespace | Token`Comment | Token`Newline | Token`LineContinuation, _, _] ];

  pairs = Partition[filtered, 2, 1];

  Do[

    If[!MatchQ[p, {_, LeafNode[Token`Semi, _, _]}],
      Continue[]
    ];

    Switch[p,
      {n_, i:LeafNode[Token`Semi, _, _]} /; n[[3, Key[Source], 2, 1]] != i[[ 3, Key[Source], 1, 1 ]],
        AppendTo[srcs, p[[2, 3, Key[Source] ]] ];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, Lint["DifferentLine", "Operand for ``;`` is on different line.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, srcs];

  issues
]]








Attributes[scanSpans] = {HoldRest}

scanSpans[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, pairs, srcs, filtered, poss, i},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source] ]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  poss = Position[children, LeafNode[Token`SemiSemi, _, _]];

  i = poss[[1, 1]];

  i += 1;
  While[i <= Length[children],
    Switch[children[[i]],
      LeafNode[Token`Newline, _, _],
        AppendTo[issues, Lint["EndOfLine", "Suspicious ``Span`` is at end of line.", "Warning", <|
          Source -> children[[ poss[[1, 1]], 3, Key[Source] ]],
          ConfidenceLevel -> 0.95 |>]];
        Break[]
      ,
      _,
        i += 1
    ]
  ];

  (*
  We want to ignore trivia
  *)
  filtered = DeleteCases[children, LeafNode[Whitespace | Token`Comment | Token`Newline | Token`LineContinuation, _, _] ];

  pairs = Partition[filtered, 2, 1];

  srcs = {};
  Do[

    If[!MatchQ[p, {_, LeafNode[Token`SemiSemi, _, _]} |
                    {LeafNode[Token`SemiSemi, _, _], _}],
      Continue[]
    ];

    Switch[p,
      {n_, i:LeafNode[Token`SemiSemi, _, _]} /; n[[3, Key[Source], 2, 1]] != i[[ 3, Key[Source], 1, 1 ]],
        AppendTo[srcs, p[[2, 3, Key[Source] ]] ];
      ,
      {i:LeafNode[Token`SemiSemi, _, _], n_} /; i[[3, Key[Source], 2, 1]] != n[[ 3, Key[Source], 1, 1 ]],
        AppendTo[srcs, p[[1, 3, Key[Source] ]] ];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, Lint["SpanDifferentLine", "Operands for ``;;`` are on different lines.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, srcs];

  issues
]]




Attributes[scanTernaryTildes] = {HoldRest}

scanTernaryTildes[pos_List, cstIn_] :=
Catch[
Module[{cst, node, children, data, issues, srcs},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source] ]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  (*
  We want to ignore trivia
  *)
  filtered = DeleteCases[children, LeafNode[Whitespace | Token`Comment | Token`Newline | Token`LineContinuation, _, _] ];

  (*
  With a ~f~ b, we only want to look at {~, f} and {f, ~}
  *)
  filtered = filtered[[2;;-2]];

  pairs = Partition[filtered, 2, 1];

  Do[

    Switch[p,
      {a_, b_} /; a[[3, Key[Source], 2, 1]] != b[[ 3, Key[Source], 1, 1 ]],
        AppendTo[srcs, p[[2, 3, Key[Source] ]] ];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, Lint["TernaryTildeDifferentLine", "Operands are on different lines.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.95
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

  {Lint["CallDifferentLine", "Call is on different lines.", "Warning", <| openSquareData, ConfidenceLevel -> 0.95 |>]}
]





Attributes[scanErrorNodes] = {HoldRest}

scanErrorNodes[pos_List, cstIn_] :=
 Module[{cst, node, tag, data, tagString, children, leaf},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  tag = node[[1]];
  children = node[[2]];
  data = node[[3]];

  tagString = Block[{$ContextPath = {"SyntaxError`", "System`"}, $Context = "Lint`Scratch`"}, ToString[tag]];

  Switch[tagString,
    "Aborted",
        leaf = children[[1]];
        {Lint["Aborted", "Aborted.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ExpectedOperand",
        {Lint["ExpectedOperand", "Expected an operand.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    _,
        {Lint[tagString, "Syntax error.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
  ]
]




Attributes[scanSyntaxErrorNodes] = {HoldRest}

scanSyntaxErrorNodes[pos_List, cstIn_] :=
 Module[{cst, node, tag, data, tagString, children},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  tag = node[[1]];
  children = node[[2]];
  data = node[[3]];

  tagString = Block[{$ContextPath = {"SyntaxError`", "System`"}, $Context = "Lint`Scratch`"}, ToString[tag]];

  Switch[tagString,
    "ExpectedOperand",
        {Lint["ExpectedOperand", "Expected an operand.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ExpectedTilde",
        {Lint["ExpectedTilde", "Expected ``~``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ColonError",
        {Lint["ColonError", "Invalid syntax for ``:``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ExpectedSet",
        {Lint["ExpectedSet", "Expected ``=`` or ``:=`` or ``=.``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ExpectedIntegrand",
        {Lint["ExpectedIntegrand", "Expected integrand.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "UnexpectedCloser",
        {Lint["UnexpectedCloser", "Unexpected closer.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    _,
        {Lint[tagString, "Syntax error.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
  ]
]




Attributes[scanGroupMissingCloserNodes] = {HoldRest}

scanGroupMissingCloserNodes[pos_List, cstIn_] :=
 Module[{cst, node, data},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  data = node[[3]];

  {Lint["GroupMissingCloser", "Missing closer.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
]



Attributes[scanSyntaxIssues] = {HoldRest}

(*
Just directly convert SyntaxIssues to Lints
*)
scanSyntaxIssues[pos_List, cstIn_] :=
Module[{cst, data, issues, syntaxIssues},
  cst = cstIn;
  data = Extract[cst, {pos}][[1]];
  issues = data[SyntaxIssues];

  syntaxIssues = Cases[issues, SyntaxIssue[_, _, _, _]];

  Lint @@@ syntaxIssues
]





End[]


EndPackage[]
