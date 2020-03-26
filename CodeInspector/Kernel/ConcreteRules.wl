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

CallNode[{_, ___, LeafNode[Token`Newline, _, _], ___}, _, _] -> scanCalls,


(*
Something like  a_..b  or  _...

Prior to 12.2,  a_..b  was parsed as Times[(a_).., b]
12.2 and onward,  a_..b  is parsed as Dot[a_., b]

Prior to 12.2,  a_..b  and  _...  were parsed differently by the kernel vs. the FE

The rules here catch cases that come from the FE and might be surprising.

Related bugs: 390755
*)

InfixNode[Dot, {
  LeafNode[Token`UnderDot, _, _], LeafNode[Token`Dot, _, _], _}, _] -> scanUnderNodes,

InfixNode[Dot, {
  PatternOptionalDefaultNode[PatternOptionalDefault, {_, LeafNode[Token`UnderDot, _, _]}, _],
  LeafNode[Token`Dot, _, _], _}, _] -> scanUnderNodes,

PostfixNode[Repeated, {
  LeafNode[Token`UnderDot, _, _], LeafNode[Token`DotDot, _, _]}, _] -> scanUnderNodes,

PostfixNode[Repeated, {
  PatternOptionalDefaultNode[PatternOptionalDefault, {_, LeafNode[Token`UnderDot, _, _]}, _], 
  LeafNode[Token`DotDot, _, _]}, _] -> scanUnderNodes,





ErrorNode[_, _, _] -> scanErrorNodes,

SyntaxErrorNode[_, _, _] -> scanSyntaxErrorNodes,

GroupMissingCloserNode[_, _, _] -> scanGroupMissingCloserNodes,

KeyValuePattern[SyntaxIssues -> _] -> scanSyntaxIssues,



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







Attributes[scanUnderNodes] = {HoldRest}

scanUnderNodes[pos_List, cstIn_] :=
 Module[{cst, node, data},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  data = node[[3]];

  {InspectionObject["KernelFEDifference", "Syntax may be interpreted differently depending on system.", "Warning", <| data, ConfidenceLevel -> 0.95 |>]}
]












Attributes[scanErrorNodes] = {HoldRest}

scanErrorNodes[pos_List, cstIn_] :=
 Module[{cst, node, tag, data, tagString, children, leaf, issues, multilineStrings},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  tag = node[[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  Switch[tag,
    Token`Error`Aborted,
      leaf = children[[1]];
      AppendTo[issues, InspectionObject["Aborted", "Aborted.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`ExpectedOperand,
      AppendTo[issues, InspectionObject["ExpectedOperand", "Expected an operand.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`UnterminatedComment,
      AppendTo[issues, InspectionObject["UnterminatedComment", "Unterminated comment.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]]
    ,
    Token`Error`UnterminatedString,
      AppendTo[issues, InspectionObject["UnterminatedString", "Unterminated string.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]];
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
            <| Source -> { { #[[1]], #[[2]] }, { #[[1]], #[[2]] + 1  } }, ConfidenceLevel -> 0.9 |>]])&[s[[3, Key[Source], 1]] ];
        ], multilineStrings
      ];
    ,
    _,
      tagString = Block[{$ContextPath = {"Token`Error`", "System`"}, $Context = "CodeInspector`Scratch`"}, ToString[tag]];
      AppendTo[issues, InspectionObject[tagString, "Syntax error.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]]
  ];

  issues
]




Attributes[scanSyntaxErrorNodes] = {HoldRest}

scanSyntaxErrorNodes[pos_List, cstIn_] :=
 Module[{cst, node, tag, data, tagString, children},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  tag = node[[1]];
  children = node[[2]];
  data = node[[3]];

  Switch[tag,
    SyntaxError`ExpectedOperand,
      {InspectionObject["ExpectedOperand", "Expected an operand.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    SyntaxError`ExpectedTilde,
      {InspectionObject["ExpectedTilde", "Expected ``~``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    SyntaxError`ColonError,
      {InspectionObject["ColonError", "Invalid syntax for ``:``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    SyntaxError`ExpectedSet,
      {InspectionObject["ExpectedSet", "Expected ``=`` or ``:=`` or ``=.``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    SyntaxError`ExpectedIntegrand,
      {InspectionObject["ExpectedIntegrand", "Expected integrand.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    SyntaxError`UnexpectedCloser,
      {InspectionObject["UnexpectedCloser", "Unexpected closer.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    _,
      tagString = Block[{$ContextPath = {"SyntaxError`", "System`"}, $Context = "CodeInspector`Scratch`"}, ToString[tag]];
      {InspectionObject[tagString, "Syntax error.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
  ]
]




Attributes[scanGroupMissingCloserNodes] = {HoldRest}

scanGroupMissingCloserNodes[pos_List, cstIn_] :=
 Module[{cst, node, data},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  data = node[[3]];

  {InspectionObject["GroupMissingCloser", "Missing closer.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
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

  InspectionObject @@@ syntaxIssues
]





End[]


EndPackage[]
