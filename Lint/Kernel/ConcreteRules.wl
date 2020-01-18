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

BinaryNode[Span, _, _] -> scanBinarySpans,

TernaryNode[Span, _, _] -> scanTernarySpans,

CallNode[{_, ___, LeafNode[Token`Newline, _, _], ___}, _, _] -> scanCalls,


ErrorNode[_, _, _] -> scanErrorNodes,

SyntaxErrorNode[_, _, _] -> scanSyntaxErrorNodes,

GroupMissingCloserNode[_, _, _] -> scanGroupMissingCloserNodes,

KeyValuePattern[SyntaxIssues -> _] -> scanSyntaxIssues,



Nothing
|>



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
        LeafNode[Token`Newline, _, _],
          AppendTo[issues, Lint["EndOfLine", "Suspicious ``Span`` is at end of line.", "Warning",
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
      {LeafNode[Whitespace | Token`Comment | Token`LineContinuation, _, _]..., LeafNode[Token`Newline, _, _], ___},
        (*
        There is a newline after some other trivia
        *)
        AppendTo[issues, Lint["EndOfLine", "Suspicious ``Span`` is at end of line.", "Warning",
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
      LeafNode[Token`Newline, _, _],
        AppendTo[issues, Lint["EndOfLine", "Suspicious ``Span`` is at end of line.", "Warning",
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
      LeafNode[Token`Newline, _, _],
        AppendTo[issues, Lint["EndOfLine", "Suspicious ``Span`` is at end of line.", "Warning",
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

  tagString = Block[{$ContextPath = {"Token`Error`", "System`"}, $Context = "Lint`Scratch`"}, ToString[tag]];

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
