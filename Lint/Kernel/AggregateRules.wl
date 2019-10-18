BeginPackage["Lint`AggregateRules`"]

$DefaultAggregateRules


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

$DefaultAggregateRules = <|

(*
Aggregate lints
*)

(*
Tags: ImplicitTimesAcrossLines
*)
InfixNode[Times, children_ /; !FreeQ[children, LeafNode[Token`Fake`ImplicitTimes, _, _], 1], KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]] -> scanImplicitTimes,
InfixNode[Times, children_ /; !FreeQ[children, LeafNode[Token`Fake`ImplicitTimes, _, _], 1] &&
                                !FreeQ[children, LeafNode[Blank | BlankSequence | BlankNullSequence | OptionalDefault, _, _] |
                                                  _BlankNode |
                                                  _BlankSequenceNode |
                                                  _BlankNullSequenceNode |
                                                  _PatternBlankNode |
                                                  _PatternBlankSequenceNode |
                                                  _PatternBlankNullSequenceNode |
                                                  _OptionalDefaultPatternNode, 1], _] -> scanImplicitTimesBlanks,

InfixNode[Times, children_ /; !FreeQ[children, LeafNode[Token`Fake`ImplicitTimes, _, _], 1] &&
                                !FreeQ[children, LeafNode[String, _, _], 1], _] -> scanImplicitTimesStrings,

(*
Tags: DotDifferentLine
*)
InfixNode[Dot, _, _] -> scanDots,

(*
Tags: SuspiciousSpan
*)
BinaryNode[Span, _, _] -> scanSpans,
TernaryNode[Span, _, _] -> scanSpans,


(*
Tags: StraySemicolon
*)
InfixNode[CompoundExpression, _, _] -> scanCompoundExpressions,

(*

a_?b[x]
probably meant to have a_?(b[x])

will also scan for:
a_?b[x]&

*)
CallNode[BinaryNode[PatternTest, _, _], {_}, _] -> scanPatternTestCalls,

BinaryNode[PatternTest, {_, _, LeafNode[Symbol, "Association"|"String"|"Integer"|"Real"|"Failure", _]}, _] -> scanBadSymbolPatternTests,


(*
a->b&
probably meant to have a->(b&)
*)
PostfixNode[Function, {BinaryNode[Rule|RuleDelayed, _, _], _}, _] -> scanRuleFunctions,

(*
a?b&
probably meant to have a?(b&)
*)
PostfixNode[Function, {BinaryNode[PatternTest, _, _], _}, _] -> scanPatternTestFunctions,

(*

a_?b[x]& is handled here
*)
(*
PostfixNode[Function, {CallNode[BinaryNode[PatternTest, _, _], {_}, _], _}, _] -> scanPatternTestCallFunctions,
*)

(*

experimental

BinaryNode[Pattern, {_, InfixNode[Alternatives, _, _]}, _] -> scanAlternativesPatterns,
*)

BinaryNode[Optional, {PatternBlankNode[PatternBlank, {_, _}, _], _, _}, _] -> scanPatternBlankOptionals,
BinaryNode[Optional, {PatternBlankSequenceNode[PatternBlankSequence, {_, _}, _], _, _}, _] -> scanPatternBlankOptionals,
BinaryNode[Optional, {PatternBlankNullSequenceNode[PatternBlankNullSequence, {_, _}, _], _, _}, _] -> scanPatternBlankOptionals,



StartOfLineNode[Information, _, _] -> scanInformation,



(*
TODO: maybe should be experimental?
*)

InfixNode[StringExpression, {InfixNode[Alternatives, _, _], __}, _] -> scanAlternativesStringExpression,




TernaryNode[TernaryTilde, {_, _, Except[LeafNode[Symbol, _, _]], _, _}, _] -> scanTernaryTildeExpectedSymbol,



(*

bad scan

BinaryNode[Optional, {BlankNode[Blank, _, _], _}, _] -> scanBlankOptionals,
BinaryNode[Optional, {BlankSequenceNode[BlankSequence, _, _], _}, _] -> scanBlankOptionals,
BinaryNode[Optional, {BlankNullSequenceNode[BlankNullSequence, _, _], _}, _] -> scanBlankOptionals,
*)



(*
Tags: SyntaxError
*)
SyntaxErrorNode[_, _, _] -> scanSyntaxErrorNodes,


(*
Tags: SyntaxError MaxExpressionDepth etc.
*)
(*
KeyValuePattern[SyntaxIssues -> _] -> scanSyntaxIssues,
*)


(*

TODO: figure out good heuristic for when:

16^xxxx

was intended to be 16^^xxxx

we might have:

16^4444

16^ffff

16^f4f4

16^4f4f

*)
(*
BinaryNode[Power, {
  LeafNode[Integer, "16", _], 
  LeafNode[Token`Caret, "^", _], 
  _}, _] -> scan,

InfixNode[ImplicitTimes, {
  BinaryNode[Power, {
    LeafNode[Integer, "16", _], 
    LeafNode[Token`Caret, "^", _], 
    LeafNode[Integer, "45", _]}, _],
  LeafNode[Token`Fake`ImplicitTimes, "", _],
  __ }, _] -> scan,
*)

Nothing
|>




Attributes[scanImplicitTimes] = {HoldRest}

scanImplicitTimes[pos_List, aggIn_] :=
Catch[
Module[{agg, node, children, data, issues, line, nextLine, oldN, oldNSrc},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source] ]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  oldN = children[[1]];
  oldNSrc = oldN[[3, Key[Source] ]];
  line = oldNSrc[[2, 1]];
  Do[
    nextLine = n[[3, Key[Source], 1, 1]];
    If[n[[1]] === Token`Fake`ImplicitTimes && line != nextLine,
      AppendTo[issues,
        Lint["ImplicitTimesAcrossLines", "Implicit ``Times`` across lines.", "Warning",
          <|Source->{{line, oldNSrc[[2,2]]+1}, {line, oldNSrc[[2,2]]+1}},
            ConfidenceLevel -> 0.95,
            CodeActions -> {
              CodeAction["Insert ``;``", InsertNode, <|Source->{{line, oldNSrc[[2,2]]+1}, {line, oldNSrc[[2,2]]+1}}, "InsertionNode"->LeafNode[Token`Semi, ";", <||>] |>],
              CodeAction["Insert ``,``", InsertNode, <|Source->{{line, oldNSrc[[2,2]]+1}, {line, oldNSrc[[2,2]]+1}}, "InsertionNode"->LeafNode[Token`Comma, ",", <||>]|>] }
          |>]];
      Break[];
    ];
    oldN = n;
    oldNSrc = oldN[[3, Key[Source] ]];
    line = oldNSrc[[2, 1]];
    ,
    {n, children[[2;;]]}
  ];

  issues
]]



Attributes[scanImplicitTimesBlanks] = {HoldRest}

scanImplicitTimesBlanks[pos_List, aggIn_] :=
Catch[
Module[{agg, node, data, children, issues, pairs, src},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  pairs = Partition[children[[;;;;2]], 2, 1];

  Do[
    If[!contiguousQ[p[[1, 3, Key[Source]]], p[[2, 3, Key[Source]]]],
      Continue[]
    ];

    If[(!MatchQ[p[[1]], LeafNode[Blank | BlankSequence | BlankNullSequence | OptionalDefault, _, _] |
                        _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode |
                        _PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode |
                        _OptionalDefaultPatternNode]) ||
      (!MatchQ[p[[2]],  LeafNode[Blank | BlankSequence | BlankNullSequence | OptionalDefault, _, _] |
                        _BlankNode | _BlankSequenceNode | _BlankNullSequenceNode |
                        _PatternBlankNode | _PatternBlankSequenceNode | _PatternBlankNullSequenceNode |
                        _OptionalDefaultPatternNode]),
      Continue[]
    ];

    src = p[[2, 3, Key[Source] ]];

    AppendTo[issues, Lint["ContiguousImplicitTimesBlanks", "Unexpected " <> format[ToInputFormString[p[[2]]]], "Error",
      <|Source->src,
        ConfidenceLevel -> 0.95,
        CodeActions -> {
          CodeAction["Delete unexpected " <> format[ToInputFormString[p[[2]]]], DeleteNode, <|Source->src|>] }
      |>]];
    ,
    {p, pairs}
  ];

  issues
]]



Attributes[scanImplicitTimesStrings] = {HoldRest}

scanImplicitTimesStrings[pos_List, aggIn_] :=
Module[{agg, node, data, issues, src, children, case},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  case = FirstCase[children, LeafNode[Token`Fake`ImplicitTimes, _, _]];

  src = case[[ 3, Key[Source] ]];

  AppendTo[issues, Lint["ImplicitTimesStrings", "Implicit ``Times`` between ``String``s", "Warning",
    <|Source->src,
      ConfidenceLevel -> 0.75,
      CodeActions -> {
        CodeAction["Insert ``*``", InsertNode, <|Source->src, "InsertionNode"->LeafNode[Token`Star, "*", <||>]|>]}
    |>]];

  issues
]




Attributes[scanDots] = {HoldRest}

scanDots[pos_List, aggIn_] :=
Catch[
Module[{agg, node, children, data, issues, line, nextLine, oldN, oldNSrc},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source]]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  oldN = children[[1]];
  oldNSrc = oldN[[3, Key[Source] ]];
  line = oldNSrc[[2, 1]];
  Do[
    nextLine = n[[3, Key[Source], 1, 1]];
    If[oldN[[1]] === Token`Dot && line != nextLine,
      AppendTo[issues, Lint["DotDifferentLine", "Operands for ``.`` are on different lines.", "Warning",
        <|data,
          ConfidenceLevel -> 0.75,
          CodeActions -> {
              CodeAction["Replace ``.`` with ``;``", ReplaceNode, <|Source->oldNSrc, "ReplacementNode"->LeafNode[Token`Semi, ";", <||>] |>],
              CodeAction["Replace ``.`` with ``,``", ReplaceNode, <|Source->oldNSrc, "ReplacementNode"->LeafNode[Token`Comma, ",", <||>]|>] }
        |>]];
      Break[];
    ];
    oldN = n;
    oldNSrc = oldN[[3, Key[Source] ]];
    line = oldNSrc[[2, 1]];
    ,
    {n, children[[2;;]]}
  ];

  issues
]]


Attributes[scanSpans] = {HoldRest}

scanSpans[pos_List, aggIn_] :=
Catch[
 Module[{agg, node, children, data, issues, src},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  (*
  The position {2, _} tests for top-level, because the cst looks like FileNode[File, { SpanNode }, <||>]
  *)
  If[MatchQ[pos, {2, _}],
    (* top node, no parent *)

    Switch[node,
      BinaryNode[Span, _, _],

        src = node[[2, 2, 3, Key[Source] ]];

        AppendTo[issues, Lint["SuspiciousSpan", "Suspicious ``;;`` at top-level.", "Warning",
          <|  Source -> src,
              ConfidenceLevel -> 0.95,
              CodeActions -> {
                CodeAction["Replace ``;;`` with ``;``", ReplaceNode,
                  <|  Source -> src,
                      "ReplacementNode"->LeafNode[Token`Semi, ";", <||>]
                  |>] }
          |>]];
      ,
      TernaryNode[Span, _, _],

        (*
        Pick second ;;
        *)
        src = node[[2, 4, 3, Key[Source] ]];

        AppendTo[issues, Lint["SuspiciousSpan", "Suspicious ``;;`` at top-level.", "Warning",
          <|  Source -> src,
              ConfidenceLevel -> 0.95,
              CodeActions -> {
                CodeAction["Replace ``;;`` with ``;``", ReplaceNode,
                  <|  Source -> src,
                      "ReplacementNode"->LeafNode[Token`Semi, ";", <||>]
                  |>] }
          |>]];

    ]
  ];


  (*
  
  The parser returns a Warning about different lines

  *)
  (*
  line = children[[1]][[3]][Source][[2,1]];
  Do[
    nextLine = n[[3]][Source][[1,1]];
    If[line != nextLine,
      AppendTo[issues, Lint["SuspiciousSpan", "Suspicious use of ``;;``.\n\
Did you mean ``;``?", "Warning", data]];
      Break[];
    ];
    line = n[[3]][Source][[2,1]];
    ,
    {n, children[[3;;;;2]]}
  ];
  *)

  issues
]]



(*

CompoundExpression is handled as an Infix operator by the parser

The parser itself does not give any warnings for CompoundExpression


StraySemicolon is to find things like this:

(f[];
; Throw[$Failed, $tag])

*)


Attributes[scanCompoundExpressions] = {HoldRest}

scanCompoundExpressions[pos_List, aggIn_] :=
Catch[
Module[{agg, node, children, data, issues, straySemis, rand, semi},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  straySemis = SequenceCases[children, {LeafNode[Token`Fake`ImplicitNull, "", _], semi:LeafNode[Token`Semi, ";", _]} :> semi];

  Scan[(AppendTo[issues, Lint["UnexpectedSemicolon", "``;`` may not be needed.", "Warning", <|#[[3]], ConfidenceLevel -> 0.95|>]])&, straySemis];

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source]]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  (rand = #[[1]];
    semi = #[[2]];
    If[ rand[[3, Key[Source], 2, 1]] != semi[[3, Key[Source], 1,1]],
      AppendTo[issues,
        Lint["DifferentLine", "Operand for ``;`` is on different line.", "Warning", <|semi[[3]], ConfidenceLevel -> 0.95|>]];
    ];)& /@ Partition[children, 2];

  issues
]]


Attributes[scanPatternTestCalls] = {HoldRest}

(*
warn about a_?b[x] which actually parses as (a_?b)[x] and not a_?(b[x])
*)
scanPatternTestCalls[pos_List, aggIn_] :=
Catch[
 Module[{agg, node, data, children, patternTest, args, patternTestChildren, patternTestArg1, patternTestArg2, issues,
  parent, parentPos, src, replacementNode1, replacementNode2, replacementNode},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  patternTest = node[[1]];
  children = node[[2]];

  patternTestChildren = patternTest[[2]];
  patternTestArg1 = patternTestChildren[[1]];
  patternTestArg2 = patternTestChildren[[3]];
  args = children[[1]];
  data = node[[3]];


  If[Length[pos] >= 2,
    parentPos = Drop[pos, -2];
    parent = Extract[agg, {parentPos}][[1]];

    If[MatchQ[parent, PostfixNode[Function, _, _]],
      Throw[scanPatternTestCallFunctions[parentPos, agg]]
     ];
  ];



  issues = {};

  Switch[patternTestArg2,
    GroupNode[GroupParen, _, _],
      (*
      If it is already  a?(b)  then assume intention and do not warn
      *)
      Throw[issues]
    ,
    LeafNode[Symbol, "Function", _],
      (*
      If it is  a?Function[args]  then make it an error

      The chance that (a?Function)[args] was intentional is... miniscule

      TODO: add anything callable like Function here 
      *)

      replacementNode = BinaryNode[PatternTest, {
                                          patternTestArg1,
                                          LeafNode[Token`Question, "?", <||>],
                                          GroupNode[GroupParen, {
                                            LeafNode[Token`OpenParen, "(", <||>],
                                            CallNode[patternTestArg2, {args}, <||>],
                                            LeafNode[Token`CloseParen, ")", <||>] }, <||>]}, <||>];

      src = data[Source];
      AppendTo[issues, Lint["SuspiciousPatternTestCallFunction", "Suspicious use of ``?``", "Error",
        <|
          Source->src,
          ConfidenceLevel -> 0.95,
          CodeActions -> { CodeAction["Wrap parens around RHS", ReplaceNode, <| Source -> src, "ReplacementNode" -> replacementNode |>] }
        |>]];
      Throw[issues]
  ];

  (*

  this is noisy, so make Remark for now

  *)
  src = data[Source];

  replacementNode1 = BinaryNode[PatternTest, {
                                        patternTestArg1,
                                        LeafNode[Token`Question, "?", <||>],
                                        GroupNode[GroupParen, {
                                          LeafNode[Token`OpenParen, "(", <||>],
                                          CallNode[patternTestArg2, {args}, <||>],
                                          LeafNode[Token`CloseParen, ")", <||>] }, <||>]}, <||>];

  replacementNode2 = CallNode[GroupNode[GroupParen, {
                                                    LeafNode[Token`OpenParen, "(", <||>],
                                                    patternTest,
                                                    LeafNode[Token`CloseParen, ")", <||>] }, <||>], children, <||>];

  AppendTo[issues, Lint["SuspiciousPatternTestCall", "Suspicious use of ``?``", "Remark", <| Source->src, ConfidenceLevel -> 0.55, CodeActions -> {
          CodeAction["Wrap parens around RHS", ReplaceNode, <| Source-> src, "ReplacementNode" -> replacementNode1|>],
          CodeAction["Wrap parens around LHS", ReplaceNode, <| Source-> src, "ReplacementNode" -> replacementNode2|>] } |>]];

  issues

]]



Attributes[scanBadSymbolPatternTests] = {HoldRest}

scanBadSymbolPatternTests[pos_List, aggIn_] :=
Catch[
 Module[{agg, node, data, children, patternTestArg1, patternTestArg2, issues},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];

  patternTestArg1 = children[[1]];
  patternTestArg2 = children[[3]];
  data = node[[3]];

  issues = {};

  (*
  catch cases like a_?Association  when it should have been a_?AssociationQ
  *)
  Switch[patternTestArg2,
    LeafNode[Symbol, "Association", _],
      AppendTo[issues, Lint["AssociationCall", "Calling ``Association`` as a function.\n\
Did you mean ``AssociationQ``?", "Error", <| patternTestArg2[[3]], ConfidenceLevel -> 0.95 |>]];
    ,
    LeafNode[Symbol, "String", _],
      AppendTo[issues, Lint["StringCall", "Calling ``String`` as a function.\n\
Did you mean ``StringQ``?", "Error", <| patternTestArg2[[3]], ConfidenceLevel -> 0.95 |>]];
    ,
    LeafNode[Symbol, "Integer", _],
      AppendTo[issues, Lint["IntegerCall", "Calling ``Integer`` as a function.\n\
Did you mean ``IntegerQ``?", "Error", <| patternTestArg2[[3]], ConfidenceLevel -> 0.95 |>]];
    ,
    LeafNode[Symbol, "Real", _],
      AppendTo[issues, Lint["RealCall", "Calling ``Real`` as a function.\n\
Did you mean ``RealQ``?", "Error", <| patternTestArg2[[3]], ConfidenceLevel -> 0.95 |>]];
    ,
    LeafNode[Symbol, "Failure", _],
      AppendTo[issues, Lint["FailureCall", "Calling ``Failure`` as a function.\n\
Did you mean ``FailureQ``?", "Error", <| patternTestArg2[[3]], ConfidenceLevel -> 0.95 |>]];
    ,
    _,
      Throw[Failure["Internal", <||>]]
  ];

  issues

]]



Attributes[scanRuleFunctions] = {HoldRest}

(*
warn about a->b& which parses as (a->b)& and not a->(b&)
*)
scanRuleFunctions[pos_List, aggIn_] :=
Catch[
 Module[{agg, node, data, children, rule, ruleHead, ruleChild1, ruleChild2, parentPos, parent},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  rule = children[[1]];
  ruleChild1 = rule[[2]][[1]];

  (*
  we want to reduce the number of false positives so here are some heuristics:
  *)

  (*
  heuristic
  if # or ## occur in LHS of Rule, then there is no problem, assume (a->b)& is the intended parse
  *)
  If[!FreeQ[ruleChild1, LeafNode[Slot | SlotSequence, _, _]],
    Throw[{}]
  ];

  If[pos != {},
    parentPos = Most[pos];
    parent = Extract[agg, {parentPos}][[1]];
    While[parentPos != {} && (ListQ[parent] || MatchQ[parent, GroupNode[GroupSquare, _, _] | InfixNode[Comma, _, _]]),
     parentPos = Most[parentPos];
     parent = Extract[agg, {parentPos}][[1]];
     ];

     (*
     heuristic
     if surrounded with (), then assume it is intentional, i.e., (a->b&) is intentional
     *)
     If[MatchQ[parent, GroupNode[GroupParen, _, _]],
      Throw[{}]
     ];

     (*
     heuristic
      if inside @, then assume it is intentional, i.e., a->b& @ x is intentional
      if inside /@, then assume it is intentional, i.e., a->b& /@ x is intentional
     *)
     If[MatchQ[parent, BinaryNode[BinaryAt | Map | Composition | RightComposition | Apply, {node, _, _}, _]],
      Throw[{}]
     ];

     If[$Debug,
      Print[parent]
     ];

     (*
     heuristic
      if inside Map[], then assume it is intentional, i.e., Map[a->b&, x] is intentional

      Map and MapAt can have multiple arguments
     *)
     If[MatchQ[parent, CallNode[LeafNode[Symbol, "Map" | "MapAt", _], {
                          GroupNode[GroupSquare, { _,
                            InfixNode[Comma, {node, ___}, _ ], _ }, _]}, _]],
      Throw[{}]
     ];

     (*
     heuristic
      if inside Reap[], then assume it is intentional, i.e., Reap[xxx, xxx, a->b&] is intentional
     *)
     If[MatchQ[parent, CallNode[LeafNode[Symbol, "Reap", _], {
                          GroupNode[GroupSquare, { _,
                            InfixNode[Comma, {_, _, _, _, node}, _ ], _ }, _]}, _]],
      Throw[{}]
     ];

     (*
     heuristic
      if function is immediately applied, then assume it is intentional, i.e., a->b&[x] is intentional
     *)
     If[MatchQ[parent, CallNode[node, _, _]],
      Throw[{}]
     ];
    ];

  ruleHead = rule[[1]];
  ruleChild2 = rule[[2]][[3]];

  {Lint["SuspiciousRuleFunction", "Suspicious use of ``&``.\n\
The precedence of ``&`` is surprisingly low.\n\
``" <> SymbolName[ruleHead] <> "`` " <> format[ToInputFormString[rule]] <> " is inside a ``Function``.\n\
Did you mean " <>
      format[ToInputFormString[BinaryNode[ruleHead, {
        ruleChild1,
        rule[[2]][[2]],
        GroupNode[GroupParen, {
          LeafNode[Token`OpenParen, "(", <||>],
          PostfixNode[Function, {
            ruleChild2,
            LeafNode[Token`Amp, "&", <||>] }, <||>],
          LeafNode[Token`CloseParen, ")", <||>] }, <||>] }, <||>]]] <> " or " <>
      format[ToInputFormString[PostfixNode[Function, {
                          GroupNode[GroupParen, {
                            LeafNode[Token`OpenParen, "(", <||>],
                            rule,
                            LeafNode[Token`CloseParen, ")", <||>] }, <||>],
                          LeafNode[Token`Amp, "&", <||>] }, <||>]]] <> "?", "Warning", <| data, ConfidenceLevel -> 0.75 |> ]}
]]



Attributes[scanPatternTestFunctions] = {HoldRest}

(*
warn about a?b& which parses as (a?b)& and not a?(b&)
*)
scanPatternTestFunctions[pos_List, aggIn_] :=
Catch[
 Module[{agg, node, data, children, patternTest, patternTestChildren, patternTestArg1, patternTestArg2},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  patternTest = children[[1]];
  patternTestChildren = patternTest[[2]];
  patternTestArg1 = patternTestChildren[[1]];
  patternTestArg2 = patternTestChildren[[3]];
  {Lint["SuspiciousPatternTestFunction", "Suspicious use of ``&``.\n\
The precedence of ``&`` is surprisingly low and the precedence of ``?`` is surprisingly high.\n\
``?`` is inside a ``Function``.\n\
Did you mean " <>
  format[ToInputFormString[BinaryNode[PatternTest, {
                        patternTestArg1,
                        LeafNode[Token`Question, "?", <||>],
                        GroupNode[GroupParen, {
                          LeafNode[Token`OpenParen, "(", <||>],
                          PostfixNode[Function, {
                            patternTestArg2,
                            LeafNode[Token`Amp, "&", <||>] }, <||>],
                          LeafNode[Token`CloseParen, ")", <||>] }, <||>] }, <||>]]] <> 
          " or " <> format[ToInputFormString[PostfixNode[Function, {
                                            GroupNode[GroupParen, {
                                              LeafNode[Token`OpenParen, "(", <||>],
                                              patternTest,
                                              LeafNode[Token`CloseParen, ")", <||>] }, <||>]}, <||>]]] <> "?", "Warning", <| data, ConfidenceLevel -> 0.75 |>]}
]]





(*

this is called from scanPatternTestCalls and not the dispatcher

*)

Attributes[scanPatternTestCallFunctions] = {HoldRest}

(*
warn about a?b[#]& which parses as (a?b[#])& and not a?(b[#]&)
*)
scanPatternTestCallFunctions[pos_List, aggIn_] :=
Catch[
 Module[{agg, node, data, children, call, patternTest, callChildren, patternTestChildren, patternTestArg1, patternTestArg2},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  call = children[[1]];
  patternTest = call[[1]];
  callChildren = call[[2]];
  patternTestChildren = patternTest[[2]];
  patternTestArg1 = patternTestChildren[[1]];
  patternTestArg2 = patternTestChildren[[2]];

  {Lint["SuspiciousPatternTestCallFunction", "Suspicious use of ``&``.\n\
The precedence of ``&`` is surprisingly low and the precedence of ``?`` is surprisingly high.\n\
Call to ``PatternTest`` " <> format[ToInputFormString[call]] <> " is inside a ``Function``.\n\
Did you mean " <> format[ToInputFormString[BinaryNode[PatternTest, {
                                        patternTestArg1,
                                        LeafNode[Token`Question, "?", <||>],
                                        GroupNode[GroupParen, {
                                          LeafNode[Token`OpenParen, "(", <||>],
                                          PostfixNode[Function, {
                                            CallNode[patternTestArg2, callChildren, <||>],
                                            LeafNode[Token`Amp, "&", <||>] }, <||>],
                                          LeafNode[Token`CloseParen, ")", <||>] }, <||>] }, <||>]]] <>
  " or " <> format[ToInputFormString[PostfixNode[Function, {
                                    CallNode[GroupNode[GroupParen, {
                                      LeafNode[Token`OpenParen, "(", <||>],
                                      patternTest,
                                      LeafNode[Token`CloseParen, ")", <||>] }, <||>], callChildren, <||>],
                                    LeafNode[Token`Amp, "&", <||>] }, <||>]]] <> "?",
    "Warning", <| data, ConfidenceLevel -> 0.75 |>]}
]]




(*

experimental

still need to nail down heuristics



rough algorithm to use for SuspiciousAlternativesPattern:

in the rules list:
alt_Alternatives /; !FreeQ[alt, Pattern | PatternBlank | etc]

in the function:
collect candidate patterns
   scan through all candidate:
       if of the form x_ /; blahQ[x], then do not need to add x to candidates
if there are candidates:
    go to parent node and see if something like x:f[]|{x:f[]}
        keep track of this parent pattern for further diagnostics
    scan through all branches of Alternatives:
        if a branch does not have every candidate:
            there will be a warning
            if candidate is parent pattern
                then issue special warning
            else:
                then issue warning





Attributes[scanAlternativesPatterns] = {HoldRest}

(*
warn about a:b|c which parses as a:(b|c) and not (a:b)|c
*)
scanAlternativesPatterns[pos_List, cstIn_] :=
Catch[
 Module[{cst, node, data, children, patternArg1, alternatives, alternativesChildren, alternativesFirst, alternativesRest},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  patternArg1 = children[[1]];
  alternatives = children[[2]];
  alternativesChildren = alternatives[[2]];
  alternativesFirst = First[alternativesChildren];
  alternativesRest = Rest[alternativesChildren];

  {Lint["SuspiciousAlternativesPattern", {"Suspicious use of ", LintBold["|"], ". The precedence of ", LintBold["|"], " is higher than ", LintBold[":"], ". Did you mean ",
          LintBold[ToInputFormString[InfixNode[Alternatives, {GroupNode[GroupParen, {BinaryNode[Pattern, {patternArg1, alternativesFirst}, <||>]}, <||>]}~Join~alternativesRest, <||>]]],
          " or ", LintBold[ToInputFormString[BinaryNode[Pattern, {patternArg1, GroupNode[GroupParen, {alternatives}, <||>]}, <||>]]], " ?"}, "Remark", data]}
]]
*)


Attributes[scanPatternBlankOptionals] = {HoldRest}

(*
warn about a_:b  which is Optional[Pattern[a, _], b]  and not a:b
*)
scanPatternBlankOptionals[pos_List, aggIn_] :=
Catch[
 Module[{agg, node, data, children, patternBlank, patternBlankChildren, pattern, opt},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  patternBlank = children[[1]];
  patternBlankChildren = patternBlank[[2]];
  pattern = patternBlankChildren[[1]];
  opt = children[[3]];

  (*
  bring in heuristics for when a_:b is valid
  If b has Patterns or Blanks, then b is NOT a valid optional and warn
  *)
  If[FreeQ[opt, LeafNode[Blank | BlankSequence | BlankNullSequence | OptionalDefault, _, _] |
                _BlankNode |
                _BlankSequenceNode |
                _BlankNullSequenceNode |
                _PatternBlankNode |
                _PatternBlankSequenceNode |
                _PatternBlankNullSequenceNode |
                _OptionalDefaultPatternNode |
                BinaryNode[Pattern, _, _] |
                (* also check for Alternatives *)
                InfixNode[Alternatives, _, _]
                ],
    Throw[{}]
  ];

  {Lint["SuspiciousPatternBlankOptional", "Suspicious use of ``:``.\n\
Did you mean " <> format[ToInputFormString[BinaryNode[Pattern, {
                        pattern,
                        LeafNode[Token`Fake`PatternColon, ":", <||>],
                        opt}, <||>]]] <> "?\n\
This may be ok if " <> format[ToInputFormString[pattern]] <> " is used as a pattern.", "Warning", <| data, ConfidenceLevel -> 0.85|>]}
]]





Attributes[scanInformation] = {HoldRest}

scanInformation[pos_List, aggIn_] :=
Module[{agg, node, data, children, tok},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  tok = children[[1]];

  {Lint["SuspiciousInformation", "Suspicious use of ``" <> tok["String"] <> "``.", "Error", <| data, ConfidenceLevel -> 0.55 |>]}
]












(*
HIGH FALSE POSITIVES

need better heuristics

*)
Attributes[scanAlternativesStringExpression] = {HoldRest}

(*
a | b ~~ c
*)
scanAlternativesStringExpression[pos_List, aggIn_] :=
Catch[
 Module[{agg, node, data, children, stringExpArgRest, alternatives, alternativesChildren, alternativesMost, alternativesLast},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  alternatives = children[[1]];
  alternativesChildren = alternatives[[2, ;;;;2]];
  alternativesMost = Most[alternativesChildren];
  alternativesLast = Last[alternativesChildren];

  stringExpArgRest = children[[3;;;;2]];

  {Lint["SuspiciousAlternativesStringExpression", "Suspicious use of ``|``. The precedence of ``|`` is higher than ``~~``.\n\
Did you mean " <> format[ToInputFormString[InfixNode[Alternatives,
                                          Riffle[alternativesMost ~Join~ {GroupNode[GroupParen, {
                                                                    LeafNode[Token`OpenParen, "(", <||>],
                                                                    InfixNode[StringExpression,
                                                                      Riffle[
                                                                        {alternativesLast} ~Join~ stringExpArgRest,
                                                                        LeafNode[Token`TildeTilde, "~~", <||>]], <||>],
                                                                    LeafNode[Token`CloseParen, ")", <||>]}, <||>]},
                                                  LeafNode[Token`Bar, "|", <||>]], <||>]]] <>
" or " <> format[ToInputFormString[InfixNode[StringExpression,
                                      Riffle[{GroupNode[GroupParen, {
                                                  LeafNode[Token`OpenParen, "(", <||>],
                                                  alternatives,
                                                  LeafNode[Token`CloseParen, ")", <||>]}, <||>]} ~Join~
                                                stringExpArgRest,
                                              LeafNode[Token`TildeTilde, "~~", <||>]], <||>]]] <> "?", "Remark", <| data, ConfidenceLevel -> 0.75 |>]}
]]








Attributes[scanTernaryTildeExpectedSymbol] = {HoldRest}

scanTernaryTildeExpectedSymbol[pos_List, aggIn_] :=
Catch[
 Module[{agg, node, data, children, middle},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];

  middle = children[[3]];
  data = middle[[3]];

  (*
  Middle expression is usually a symbol.
  *)
  {Lint["ExpectedSymbol", "Suspicious syntax.", "Warning", <| data, ConfidenceLevel -> 0.55 |>]}
]]






(*

bad scan

Using 2-arg Optional is needed sometimes



In[1]:= MatchQ[f[1, 2], f[_, _, _.]]

Out[1]= False

In[2]:= MatchQ[f[1, 2], f[_, _, _ : sentinel]]

Out[2]= True




Attributes[scanBlankOptionals] = {HoldRest}

(*
warn about _:b  which is Optional[_, b]  and does not make sense
*)
scanBlankOptionals[pos_List, cstIn_] :=
Catch[
 Module[{cst, node, data, children, blank, opt},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  blank = children[[1]];
  opt = children[[2]];

  (*
  a bit noisy and no immediate good suggestion for change, Remark for now
  *)

  {Lint["SuspiciousBlankOptional", {"Suspicious use of ", LintBold[":"], " ", LintBold[ToInputFormString[blank]],
    " is not a named pattern and ", LintBold["Optional"], " has no effect."}, "Remark", data]}
]]

*)





Attributes[scanSyntaxErrorNodes] = {HoldRest}

scanSyntaxErrorNodes[pos_List, aggIn_] :=
 Module[{agg, node, tag, data, tagString, children, leaf},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  tag = node[[1]];
  children = node[[2]];
  data = node[[3]];

  tagString = Block[{$ContextPath = {"SyntaxError`", "System`"}, $Context = "Lint`Scratch`"}, ToString[tag]];

  Switch[tagString,
    "UnhandledCharacter",
        leaf = children[[1]];
        {Lint["UnhandledCharacter", "Unhandled character: " <> format[leaf[[2]]] <> ".", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "UnterminatedComment",
        {Lint["UnterminatedComment", "Unterminated comment.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ExpectedEqual",
        {Lint["ExpectedEqual", "Expected ``=``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "EmptyString",
        {Lint["EmptyString", "Empty string.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "UnterminatedString",
        {Lint["UnterminatedString", "Unterminated string.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "InvalidBase",
        {Lint["InvalidBase", "Invalid base.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "UnrecognizedDigit",
        {Lint["UnrecognizedDigit", "Unrecognized digit.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ExpectedAccuracy",
        {Lint["ExpectedAccuracy", "Expected accuracy.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ExpectedExponent",
        {Lint["ExpectedExponent", "Expected exponent.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "UnhandledDot",
        {Lint["UnhandledDot", "Unhandled ``.``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ExpectedTilde",
        {Lint["ExpectedTilde", "Expected ``~``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ExpectedSet",
        {Lint["ExpectedSet", "Expected ``=`` or ``:=`` or ``=.``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ColonError",
        {Lint["ColonError", "Invalid syntax for ``:``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "ExpectedPossibleExpression",
        {Lint["ExpectedPossibleExpression", "Expected an expression.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    _,
        {Lint[tagString, "Syntax error.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
  ]
]



(*
Attributes[scanSyntaxIssues] = {HoldRest}

(*
Just directly convert SyntaxIssues to Lints
*)
scanSyntaxIssues[pos_List, cstIn_] :=
Module[{cst, data, issues},
  cst = cstIn;
  data = Extract[cst, {pos}][[1]];
  issues = data[SyntaxIssues];

  Lint @@@ issues
]
*)



End[]


EndPackage[]
