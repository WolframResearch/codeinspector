BeginPackage["CodeInspector`AggregateRules`"]

$DefaultAggregateRules


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`Utils`"]



blankPat =
  LeafNode[Token`Under | Token`UnderUnder | Token`UnderUnderUnder | Token`UnderDot, _, _] |
  CompoundNode[
    Blank | BlankSequence | BlankNullSequence |
    PatternBlank | PatternBlankSequence | PatternBlankNullSequence | PatternOptionalDefault, _, _]



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
  children_ /;
    !FreeQ[children, LeafNode[Token`Fake`ImplicitTimes, _, _], 1] &&
    !FreeQ[children, blankPat | BinaryNode[PatternTest | Condition, {blankPat, ___}, _], 1], _] -> scanImplicitTimesBlanks,

InfixNode[Times,
  children_ /;
    !FreeQ[children, LeafNode[Token`Fake`ImplicitTimes, _, _], 1] &&
    !FreeQ[children, LeafNode[String, _, _], 1], _] -> scanImplicitTimesStrings,



InfixNode[Dot, _, _] -> scanDots,

PostfixNode[Repeated, _, _] -> scanRepeateds,

TernaryNode[TernaryTilde, _, KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]] -> scanTernaryTildes,


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

BinaryNode[Optional, {CompoundNode[PatternBlank | PatternBlankSequence | PatternBlankNullSequence, {_, _}, _], _, _}, _] -> scanPatternBlankOptionals,


(*
StartOfLineNode[Information, _, _] -> scanInformation,
*)


(*
TODO: maybe should be experimental?
*)

InfixNode[StringExpression, {InfixNode[Alternatives, _, _], __}, _] -> scanAlternativesStringExpression,




TernaryNode[TernaryTilde, {_, _, Except[LeafNode[Symbol, _, _]], _, _}, _] -> scanTernaryTildeExpectedSymbol,


PrefixNode[Plus, _, _] -> scanPrefixPlus,

(*

bad scan

BinaryNode[Optional, {BlankNode[Blank, _, _], _}, _] -> scanBlankOptionals,
BinaryNode[Optional, {BlankSequenceNode[BlankSequence, _, _], _}, _] -> scanBlankOptionals,
BinaryNode[Optional, {BlankNullSequenceNode[BlankNullSequence, _, _], _}, _] -> scanBlankOptionals,
*)



(*
scan for something like Rule___

Last[StringSplit[sym, "`"]] will return the symbol name, e.g. A`Bar => Bar
*)
CompoundNode[PatternBlank | PatternBlankSequence | PatternBlankNullSequence | PatternOptionalDefault, {
  LeafNode[Symbol, sym_?uppercaseSymbolNameQ, _],
  ___}, _] -> scanUppercasePatternBlank,


CompoundNode[Blank | BlankSequence | BlankNullSequence, {_,
  LeafNode[Symbol, (s_ /; StringEndsQ[s, "Q"]) | "Positive" | "Negative" | "NonPositive" | "NonNegative", _]}, _] ->
  scanBlankPredicate,


BinaryNode[PatternTest, {LeafNode[Symbol, _, _], _, LeafNode[Symbol, _, _]}, _] ->
  scanSymbolPatternTest,


InfixNode[MessageName, children_ /; Length[children] > 3, _] -> scanMessageName,


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





Attributes[scanPrefixs] = {HoldRest}

scanPrefixs[pos_List, aggIn_] :=
Catch[
Module[{agg, node, children, data, issues, srcs, pairs},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

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
    AppendTo[issues, InspectionObject["PrefixDifferentLine", "Operands are on different lines.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, srcs];

  issues
]]



Attributes[scanPostfixs] = {HoldRest}

scanPostfixs[pos_List, aggIn_] :=
Catch[
Module[{agg, node, children, data, issues, highConfSrcs, lowConfSrcs, pairs},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  highConfSrcs = {};
  lowConfSrcs = {};

  issues = {};

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
    AppendTo[issues, InspectionObject["PostfixDifferentLine", "Operands are on different lines.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, highConfSrcs];

  Scan[(
    AppendTo[issues, InspectionObject["PostfixDifferentLine", "Operands are on different lines.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.85
      |>]];
    )&, lowConfSrcs];

  issues
]]



Attributes[scanImplicitTimesBlanks] = {HoldRest}

scanImplicitTimesBlanks[pos_List, aggIn_] :=
Catch[
Module[{agg, node, data, children, issues, pairs, warningSrcs, errorSrcs},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  warningSrcs = {};
  errorSrcs = {};

  issues = {};

  pairs = Partition[children, 2, 1];

  Do[
    (*
    Make sure there is a blank next to a Token`Fake`ImplicitTimes
    *)
    If[!MatchQ[p, {blankPat | BinaryNode[PatternTest | Condition, {blankPat, ___}, _], LeafNode[Token`Fake`ImplicitTimes, _, _]} |
                    {LeafNode[Token`Fake`ImplicitTimes, _, _], blankPat | BinaryNode[PatternTest | Condition, {blankPat, ___}, _]}],
      Continue[]
    ];

    (*
    mark more specific patterns as errors
  
    The error cases are:
    _ <implicit Times>
    __ <implicit Times>
    ___ <implicit Times>
    _. <implicit Times>
    a_ <implicit Times>
    a__ <implicit Times>
    a___ <implicit Times>
    a_. <implicit Times>

    <implicit Times> _
    <implicit Times> __
    <implicit Times> ___
    <implicit Times> _.
    <implicit Times> _a
    <implicit Times> __a
    <implicit Times> ___a
    <implicit Times> _?f
    <implicit Times> __?f
    <implicit Times> ___?f
    <implicit Times> _.?f
    <implicit Times> _a?f
    <implicit Times> __a?f
    <implicit Times> ___a?f
    *)
    Switch[p,
      {LeafNode[Token`Under | Token`UnderUnder | Token`UnderUnderUnder | Token`UnderDot, _, _] |
        CompoundNode[
          (*
          NOT THESE
          Blank |
          BlankSequence |
          BlankNullSequence |
          *)
          PatternBlank |
          PatternBlankSequence |
          PatternBlankNullSequence |
          PatternOptionalDefault, _, _]
        ,
        LeafNode[Token`Fake`ImplicitTimes, _, _]}
        ,
        AppendTo[errorSrcs, p[[2, 3, Key[Source]]]];
      ,
      {LeafNode[Token`Fake`ImplicitTimes, _, _],
        LeafNode[Token`Under | Token`UnderUnder | Token`UnderUnderUnder | Token`UnderDot, _, _] |
        CompoundNode[
          Blank |
          BlankSequence |
          BlankNullSequence
          (*
          NOT THESE
          PatternBlank |
          PatternBlankSequence |
          PatternBlankNullSequence |
          PatternOptionalDefault
          *), _, _] |
        BinaryNode[PatternTest | Condition, {
          LeafNode[Token`Under | Token`UnderUnder | Token`UnderUnderUnder | Token`UnderDot, _, _] |
          CompoundNode[
            Blank |
            BlankSequence |
            BlankNullSequence
            (*
            NOT THESE
            PatternBlank |
            PatternBlankSequence |
            PatternBlankNullSequence |
            PatternOptionalDefault
            *), _, _], ___ }, _]}
        ,
        AppendTo[errorSrcs, p[[1, 3, Key[Source]]]];
      ,
      {_, LeafNode[Token`Fake`ImplicitTimes, _, _]},
        AppendTo[warningSrcs, p[[2, 3, Key[Source]]]];
      ,
      {LeafNode[Token`Fake`ImplicitTimes, _, _], _},
        AppendTo[warningSrcs, p[[1, 3, Key[Source]]]];
    ];

    ,
    {p, pairs}
  ];

  warningSrcs = DeleteDuplicates[warningSrcs];
  errorSrcs = DeleteDuplicates[errorSrcs];

  Scan[(
    AppendTo[issues, InspectionObject["ImplicitTimesBlanks", "Suspicious implicit ``Times`` with blanks.", "Warning",
      <|Source->#,
        ConfidenceLevel -> 0.85,
        CodeActions -> {
          CodeAction["Insert ``*``", InsertNode, <|Source->#, "InsertionNode"->LeafNode[Token`Star, "*", <||>]|>]}
      |>]];
    )&, warningSrcs];

  Scan[(
    AppendTo[issues, InspectionObject["ImplicitTimesBlanks", "Suspicious implicit ``Times`` with blanks.", "Error",
      <|Source->#,
        ConfidenceLevel -> 0.85,
        CodeActions -> {
          CodeAction["Insert ``*``", InsertNode, <|Source->#, "InsertionNode"->LeafNode[Token`Star, "*", <||>]|>]}
      |>]];
    )&, errorSrcs];

  issues
]]



Attributes[scanImplicitTimesStrings] = {HoldRest}

scanImplicitTimesStrings[pos_List, aggIn_] :=
Module[{agg, node, data, issues, children, pairs, srcs},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  pairs = Partition[children, 2, 1];

  Do[

    (*
    Make sure there is a string next to a Token`Fake`ImplicitTimes
    *)
    If[!MatchQ[p, {LeafNode[String, _, _], LeafNode[Token`Fake`ImplicitTimes, _, _]} |
                    {LeafNode[Token`Fake`ImplicitTimes, _, _], LeafNode[String, _, _]}],
      Continue[]
    ];

    Switch[p,
      {_, LeafNode[Token`Fake`ImplicitTimes, _, _]},
        AppendTo[srcs, p[[2, 3, Key[Source]]]];
      ,
      {LeafNode[Token`Fake`ImplicitTimes, _, _], _},
        AppendTo[srcs, p[[1, 3, Key[Source]]]];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, InspectionObject["ImplicitTimesStrings", "Suspicious implicit ``Times`` with strings.", "Warning",
      <|Source->#,
        ConfidenceLevel -> 0.75,
        CodeActions -> {
          CodeAction["Insert ``*``", InsertNode, <|Source->#, "InsertionNode"->LeafNode[Token`Star, "*", <||>]|>]}
      |>]];
    )&, srcs];

  issues
]





Attributes[scanDots] = {HoldRest}

scanDots[pos_List, aggIn_] :=
Catch[
Module[{agg, node, children, data, issues, srcs, pairs, underPoss, dot},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  (*
  Only check if LineCol-style
  *)
  If[MatchQ[children[[1, 3, Key[Source]]], {{_Integer, _Integer}, {_Integer, _Integer}}],

    srcs = {};

    pairs = Partition[children, 2, 1];

    Do[
      If[!MatchQ[p, {_, LeafNode[Token`Dot, _, _]} |
                      {LeafNode[Token`Dot, _, _], _}],
        Continue[]
      ];
      Switch[p,
        {n_, i:LeafNode[Token`Dot, _, _]} /; n[[3, Key[Source], 2, 1]] != i[[3, Key[Source], 1, 1]],
          AppendTo[srcs, p[[2, 3, Key[Source]]]];
        ,
        {i:LeafNode[Token`Dot, _, _], n_} /; i[[3, Key[Source], 2, 1]] != n[[3, Key[Source], 1, 1]],
          AppendTo[srcs, p[[1, 3, Key[Source]]]];
      ];

      ,
      {p, pairs}
    ];

    srcs = DeleteDuplicates[srcs];

    Scan[(
      AppendTo[issues, InspectionObject["DotDifferentLine", "Operands for ``.`` are on different lines.", "Warning",
        <|Source -> #,
          ConfidenceLevel -> 0.85
        |>]];
      )&, srcs];

  ];


  (*
  Check for  a_..b

  Prior to 12.2,  a_..b  was parsed as Times[(a_).., b]

  12.2 and onward,  a_..b  is parsed as Dot[a_., b]

  https://mathematica.stackexchange.com/questions/203434/repeated-string-pattern-difference-between-the-frontend-and-wolframscript

  Related bugs: 390755
  *)
  underPoss = Position[children, LeafNode[Token`UnderDot, _, _] | CompoundNode[PatternOptionalDefault, _, _], {1}];

  Function[{pos1},
    If[pos1[[1]] + 1 <= Length[children] && MatchQ[Extract[children, pos1 + 1], LeafNode[Token`Dot, _, _]],
      dot = Extract[children, pos1[[1]] + 1];
      AppendTo[issues, InspectionObject["BackwardsCompatibility", "This syntax changed in ``WL 12.2``. Earlier versions treated this syntax incorrectly.", "Warning", <| dot[[3]], ConfidenceLevel -> 1.0 |>]]
    ]
  ] /@ underPoss;


  issues
]]


(*
Check for  _... and a_...

Prior to 12.2,  _...  was parsed as (_)...

12.2 and onward,  _...  is parsed as (_.)..

https://mathematica.stackexchange.com/questions/203434/repeated-string-pattern-difference-between-the-frontend-and-wolframscript

Related bugs: 390755

No need to also compatibility check for _.... because this was invalid prior to 12.2

*)

Attributes[scanRepeateds] = {HoldRest}

scanRepeateds[pos_List, cstIn_] :=
 Module[{cst, node, children, rand, issues, rator},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  rand = children[[1]];

  issues = {};

  If[MatchQ[rand, LeafNode[Token`UnderDot, _, _] | CompoundNode[PatternOptionalDefault, _, _]],
    rator = children[[-1]];
    AppendTo[issues, InspectionObject["BackwardsCompatibility", "This syntax changed in ``WL 12.2``. Earlier versions treated this syntax incorrectly.", "Warning", <| rator[[3]], ConfidenceLevel -> 1.0 |>]]
  ];

  issues
]








Attributes[scanTernaryTildes] = {HoldRest}

scanTernaryTildes[pos_List, aggIn_] :=
Catch[
Module[{agg, node, children, data, issues, srcs, filtered, pairs},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

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
    AppendTo[issues, InspectionObject["TernaryTildeDifferentLine", "Operands are on different lines.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, srcs];

  issues
]]








Attributes[scanSpans] = {HoldRest}

scanSpans[pos_List, aggIn_] :=
Catch[
 Module[{agg, node, children, data, issues, src, pairs, srcs},
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

        src = node[[2, 2, 3, Key[Source]]];

        AppendTo[issues, InspectionObject["SuspiciousSpan", "Suspicious ``;;`` at top-level.", "Warning",
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
        src = node[[2, 4, 3, Key[Source]]];

        AppendTo[issues, InspectionObject["SuspiciousSpan", "Suspicious ``;;`` at top-level.", "Warning",
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
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source]]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  
  pairs = Partition[children, 2, 1];

  srcs = {};
  Do[

    If[!MatchQ[p, {_, LeafNode[Token`SemiSemi, _, _]} |
                    {LeafNode[Token`SemiSemi, _, _], _}],
      Continue[]
    ];

    Switch[p,
      {n_, i:LeafNode[Token`SemiSemi, _, _]} /; n[[3, Key[Source], 2, 1]] != i[[3, Key[Source], 1, 1]],
        AppendTo[srcs, p[[2, 3, Key[Source]]]];
      ,
      {i:LeafNode[Token`SemiSemi, _, _], n_} /; i[[3, Key[Source], 2, 1]] != n[[3, Key[Source], 1, 1]],
        AppendTo[srcs, p[[1, 3, Key[Source]]]];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, InspectionObject["SpanDifferentLine", "Operands for ``;;`` are on different lines.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, srcs];

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
Module[{agg, node, children, data, issues, pairs, srcs, straySemis, semi},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  srcs = {};

  issues = {};

  (*
  Only check if LineCol-style
  *)
  If[!MatchQ[children[[1, 3, Key[Source]]], {{_Integer, _Integer}, {_Integer, _Integer}}],
    Throw[issues]
  ];

  pairs = Partition[children, 2, 1];

  Do[

    If[!MatchQ[p, {_, LeafNode[Token`Semi, _, _]}],
      Continue[]
    ];

    Switch[p,
      {n_, i:LeafNode[Token`Semi, _, _]} /; n[[3, Key[Source], 2, 1]] != i[[3, Key[Source], 1, 1]],
        AppendTo[srcs, p[[2, 3, Key[Source]]]];
    ];

    ,
    {p, pairs}
  ];

  srcs = DeleteDuplicates[srcs];

  Scan[(
    AppendTo[issues, InspectionObject["DifferentLine", "Operand for ``;`` is on different line.", "Warning",
      <|Source -> #,
        ConfidenceLevel -> 0.95
      |>]];
    )&, srcs];


  straySemis = SequenceCases[children, {LeafNode[Token`Fake`ImplicitNull, "", _], semi:LeafNode[Token`Semi, ";", _]} :> semi];

  Scan[(AppendTo[issues, InspectionObject["UnexpectedSemicolon", "``;`` may not be needed.", "Warning", <|#[[3]], ConfidenceLevel -> 0.95|>]])&, straySemis];


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
      AppendTo[issues, InspectionObject["SuspiciousPatternTestCallFunction", "Suspicious use of ``?``.", "Error",
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

  AppendTo[issues, InspectionObject["SuspiciousPatternTestCall", "Suspicious use of ``?``.", "Remark", <| Source->src, ConfidenceLevel -> 0.55, CodeActions -> {
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
      AppendTo[issues,
        InspectionObject["AssociationCall", "Calling ``Association`` as a function.", "Error", <|
          Source -> patternTestArg2[[3, Key[Source]]],
          CodeActions -> { CodeAction["Replace with AssociationQ", ReplaceNode, <|
            "ReplacementNode" -> ToNode[AssociationQ],
            Source -> patternTestArg2[[3, Key[Source]]]
          |>] },
          ConfidenceLevel -> 0.95 |>
        ]
      ];
    ,
    LeafNode[Symbol, "String", _],
      AppendTo[issues,
        InspectionObject["StringCall", "Calling ``String`` as a function.", "Error", <|
          Source -> patternTestArg2[[3, Key[Source]]],
          CodeActions -> { CodeAction["Replace with StringQ", ReplaceNode, <|
            "ReplacementNode" -> ToNode[StringQ],
            Source -> patternTestArg2[[3, Key[Source]]]
          |>] },
          ConfidenceLevel -> 0.95 |>
        ]
      ];
    ,
    LeafNode[Symbol, "Integer", _],
      AppendTo[issues,
        InspectionObject["IntegerCall", "Calling ``Integer`` as a function.", "Error", <|
          Source -> patternTestArg2[[3, Key[Source]]],
          CodeActions -> { CodeAction["Replace with IntegerQ", ReplaceNode, <|
            "ReplacementNode" -> ToNode[IntegerQ],
            Source -> patternTestArg2[[3, Key[Source]]]
          |>] },
          ConfidenceLevel -> 0.95 |>
        ]
      ];
    ,
    LeafNode[Symbol, "Real", _],
      AppendTo[issues,
        InspectionObject["RealCall", "Calling ``Real`` as a function.", "Error", <|
          Source -> patternTestArg2[[3, Key[Source]]],
          CodeActions -> { CodeAction["Replace with Developer`RealQ", ReplaceNode, <|
            "ReplacementNode" -> ToNode[ToNode[Developer`RealQ]],
            Source -> patternTestArg2[[3, Key[Source]]]
          |>] },
          ConfidenceLevel -> 0.95 |>
        ]
      ];
    ,
    LeafNode[Symbol, "Failure", _],
      AppendTo[issues,
        InspectionObject["FailureCall", "Calling ``Failure`` as a function.", "Error", <|
          Source -> patternTestArg2[[3, Key[Source]]],
          CodeActions -> { CodeAction["Replace with FailureQ", ReplaceNode, <|
            "ReplacementNode" -> ToNode[FailureQ],
            Source -> patternTestArg2[[3, Key[Source]]]
          |>] },
          ConfidenceLevel -> 0.95 |>
        ]
      ];
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
 Module[{agg, node, data, children, rule, ruleHead, ruleChild1, ruleChild2, parentPos, parent,
  choice1, choice2},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  rule = children[[1]];
  ruleChild1 = rule[[2, 1]];

  (*
  we want to reduce the number of false positives so here are some heuristics:
  *)

  (*
  heuristic
  if # or ## occur in LHS of Rule, then there is no problem, assume (a->b)& is the intended parse
  *)
  If[!FreeQ[ruleChild1, LeafNode[Token`Hash | Token`HashHash, _, _] | CompoundNode[Slot | SlotSequence, _, _]],
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
  ruleChild2 = rule[[2, 3]];

  choice1 = BinaryNode[ruleHead, {
    ruleChild1,
    LeafNode[Token`Whitespace, " ", <||>],
    rule[[2, 2]],
    LeafNode[Token`Whitespace, " ", <||>],
    GroupNode[GroupParen, {
      LeafNode[Token`OpenParen, "(", <||>],
      PostfixNode[Function, {
        ruleChild2,
        LeafNode[Token`Amp, "&", <||>] }, <||>],
      LeafNode[Token`CloseParen, ")", <||>] }, <||>] }, <||>];

  choice2 = PostfixNode[Function, {
    GroupNode[GroupParen, {
      LeafNode[Token`OpenParen, "(", <||>],
      BinaryNode[ruleHead, {
        ruleChild1,
        LeafNode[Token`Whitespace, " ", <||>],
        rule[[2, 2]],
        LeafNode[Token`Whitespace, " ", <||>],
        ruleChild2
      }, rule[[3]]],
      LeafNode[Token`CloseParen, ")", <||>] }, <||>],
    LeafNode[Token`Amp, "&", <||>] }, <||>];

  {InspectionObject["SuspiciousRuleFunction", "Suspicious use of ``&``.\n\
The precedence of ``&`` is surprisingly low.\n\
``" <> SymbolName[ruleHead] <> "`` is inside ``Function``.", "Warning", <|
    Source -> data[[Key[Source]]],
    ConfidenceLevel -> 0.75,
    CodeActions -> {
      CodeAction["Replace with " <> format[ToInputFormString[choice1]], ReplaceNode, <|
        "ReplacementNode" -> choice1,
        Source -> data[[Key[Source]]]
      |>],
      CodeAction["Replace with " <> format[ToInputFormString[choice2]], ReplaceNode, <|
        "ReplacementNode" -> choice2,
        Source -> data[[Key[Source]]]
      |>]
    } |> ]}
]]



Attributes[scanPatternTestFunctions] = {HoldRest}

(*
warn about a?b& which parses as (a?b)& and not a?(b&)
*)
scanPatternTestFunctions[pos_List, aggIn_] :=
Catch[
 Module[{agg, node, data, children, patternTest, patternTestChildren, patternTestArg1, patternTestArg2,
  choice1, choice2},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  patternTest = children[[1]];
  patternTestChildren = patternTest[[2]];
  patternTestArg1 = patternTestChildren[[1]];
  patternTestArg2 = patternTestChildren[[3]];

  choice1 = BinaryNode[PatternTest, {
    patternTestArg1,
    LeafNode[Token`Question, "?", <||>],
    GroupNode[GroupParen, {
      LeafNode[Token`OpenParen, "(", <||>],
      PostfixNode[Function, {
        patternTestArg2,
        LeafNode[Token`Amp, "&", <||>] }, <||>],
      LeafNode[Token`CloseParen, ")", <||>] }, <||>] }, <||>];

  choice2 = PostfixNode[Function, {
    GroupNode[GroupParen, {
      LeafNode[Token`OpenParen, "(", <||>],
      patternTest,
      LeafNode[Token`CloseParen, ")", <||>] }, <||>],
    LeafNode[Token`Amp, "&", <||>]}, <||>];

  {InspectionObject["SuspiciousPatternTestFunction", "Suspicious use of ``&``.\n\
The precedence of ``&`` is surprisingly low and the precedence of ``?`` is surprisingly high.\n\
``PatternTest`` is inside ``Function``.", "Warning", <|
    Source -> data[[Key[Source]]],
    ConfidenceLevel -> 0.75,
    CodeActions -> {
      CodeAction["Replace with " <> format[ToInputFormString[choice1]], ReplaceNode, <|
        "ReplacementNode" -> choice1,
        Source -> data[[Key[Source]]]
      |>],
      CodeAction["Replace with " <> format[ToInputFormString[choice2]], ReplaceNode, <|
        "ReplacementNode" -> choice2,
        Source -> data[[Key[Source]]]
      |>]
    }
  |>]}
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
 Module[{agg, node, data, children, call, patternTest, callChildren, patternTestChildren, patternTestArg1, patternTestArg2,
  choice1, choice2},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  call = children[[1]];
  patternTest = call[[1]];
  callChildren = call[[2]];
  patternTestChildren = patternTest[[2]];
  patternTestArg1 = patternTestChildren[[1]];
  patternTestArg2 = patternTestChildren[[3]];

  choice1 = BinaryNode[PatternTest, {
    patternTestArg1,
    LeafNode[Token`Question, "?", <||>],
    GroupNode[GroupParen, {
      LeafNode[Token`OpenParen, "(", <||>],
      PostfixNode[Function, {
        CallNode[patternTestArg2, callChildren, <||>],
        LeafNode[Token`Amp, "&", <||>] }, <||>],
      LeafNode[Token`CloseParen, ")", <||>] }, <||>] }, <||>];

  choice2 = PostfixNode[Function, {
    CallNode[GroupNode[GroupParen, {
      LeafNode[Token`OpenParen, "(", <||>],
      patternTest,
      LeafNode[Token`CloseParen, ")", <||>] }, <||>], callChildren, <||>],
    LeafNode[Token`Amp, "&", <||>] }, <||>];

  {InspectionObject["SuspiciousPatternTestCallFunction", "Suspicious use of ``&``.\n\
The precedence of ``&`` is surprisingly low and the precedence of ``?`` is surprisingly high.\n\
Call to ``PatternTest`` " <> format[ToInputFormString[call]] <> " is inside ``Function``.",
    "Warning", <|
    Source -> data[[Key[Source]]],
    ConfidenceLevel -> 0.75,
    CodeActions -> {
      CodeAction["Replace with " <> format[ToInputFormString[choice1]], ReplaceNode, <|
        "ReplacementNode" -> choice1,
        Source -> data[[Key[Source]]]
      |>],
      CodeAction["Replace with " <> format[ToInputFormString[choice2]], ReplaceNode, <|
        "ReplacementNode" -> choice2,
        Source -> data[[Key[Source]]]
      |>]
    }
  |>]}
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
warn about a_:b  which is Optional[Pattern[a, _], b]  and not the same as  a:b
*)
scanPatternBlankOptionals[pos_List, aggIn_] :=
Catch[
 Module[{agg, node, data, children, patternBlank, patternBlankChildren, pattern, opt, issues,
  choice},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  patternBlank = children[[1]];
  patternBlankChildren = patternBlank[[2]];
  pattern = patternBlankChildren[[1]];
  opt = children[[3]];

  Which[
    MatchQ[opt, InfixNode[Alternatives, _, _]],
      (*
      The FE changed how:
      i_:0|1
      is parsed in 12.1

      https://mathematica.stackexchange.com/questions/224987/i-01-varies-in-v12-1-incompatible-change-or-bug
      *)
      AppendTo[issues, InspectionObject["BackwardsCompatibility", "This syntax changed in ``WL 12.1``. Earlier versions treated this syntax incorrectly.", "Warning", <| data, ConfidenceLevel -> 1.0 |>]]
    ,
    (*
    bring in heuristics for when a_:b is valid
    If b has Patterns or Blanks, then b is NOT a valid optional and warn
    *)
    !FreeQ[opt,
      blankPat |
      BinaryNode[Pattern, _, _] |
      (* also check for Alternatives *)
      InfixNode[Alternatives, _, _]],

      choice = BinaryNode[Pattern, {
        pattern,
        LeafNode[Token`Colon, ":", <||>],
        opt}, <||>];

      AppendTo[issues,
        InspectionObject["SuspiciousPatternBlankOptional", "Suspicious use of ``:``.\n\
This may be ok if " <> format[ToInputFormString[pattern]] <> " is used as a pattern.", "Warning", <|
          Source -> data[[Key[Source]]],
          ConfidenceLevel -> 0.85,
          CodeActions -> {
            CodeAction["Replace with " <> format[ToInputFormString[choice]], ReplaceNode, <|
              "ReplacementNode" -> choice,
              Source -> data[[Key[Source]]]
            |>]
          }
        |>]
      ]
  ];

  issues
]]





Attributes[scanInformation] = {HoldRest}

scanInformation[pos_List, aggIn_] :=
Module[{agg, node, data, children, tok},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  tok = children[[1]];

  {InspectionObject["SuspiciousInformation", "Suspicious use of ``" <> tok["String"] <> "``.", "Error", <| data, ConfidenceLevel -> 0.55 |>]}
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
 Module[{agg, node, data, children, stringExpArgRest, alternatives, alternativesChildren, alternativesMost, alternativesLast,
  choice1, choice2},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  alternatives = children[[1]];
  alternativesChildren = alternatives[[2, ;;;;2]];
  alternativesMost = Most[alternativesChildren];
  alternativesLast = Last[alternativesChildren];

  stringExpArgRest = children[[3;;;;2]];

  choice1 = InfixNode[Alternatives,
    Flatten[Riffle[alternativesMost ~Join~ {GroupNode[GroupParen, {
      LeafNode[Token`OpenParen, "(", <||>],
      InfixNode[StringExpression,
        Flatten[Riffle[
          {alternativesLast} ~Join~ stringExpArgRest
          ,
          {{LeafNode[Token`Whitespace, " ", <||>], LeafNode[Token`TildeTilde, "~~", <||>], LeafNode[Token`Whitespace, " ", <||>]}}
        ]], <||>],
      LeafNode[Token`CloseParen, ")", <||>]}, <||>]}
      ,
      {{LeafNode[Token`Whitespace, " ", <||>], LeafNode[Token`Bar, "|", <||>], LeafNode[Token`Whitespace, " ", <||>]}}
    ]], <||>];

  choice2 = InfixNode[StringExpression,
    Flatten[Riffle[{GroupNode[GroupParen, {
      LeafNode[Token`OpenParen, "(", <||>],
      InfixNode[Alternatives,
        Flatten[Riffle[
          alternativesChildren
          ,
          {{LeafNode[Token`Whitespace, " ", <||>], LeafNode[Token`Bar, "|", <||>], LeafNode[Token`Whitespace, " ", <||>]}}
        ]], <||>],
      LeafNode[Token`CloseParen, ")", <||>]}, <||>]} ~Join~ stringExpArgRest
      ,
      {{LeafNode[Token`Whitespace, " ", <||>], LeafNode[Token`TildeTilde, "~~", <||>], LeafNode[Token`Whitespace, " ", <||>]}}
    ]], <||>];

  {InspectionObject["SuspiciousAlternativesStringExpression", "Suspicious use of ``|``.\n\
The precedence of ``|`` is higher than ``~~``.\n\
``Alternatives`` is inside ``StringExpression``.", "Remark", <|
    Source -> data[[Key[Source]]],
    ConfidenceLevel -> 0.75,
    CodeActions -> {
      CodeAction["Replace with " <> format[ToInputFormString[choice1]], ReplaceNode, <|
        "ReplacementNode" -> choice1,
        Source -> data[[Key[Source]]]
      |>],
      CodeAction["Replace with " <> format[ToInputFormString[choice2]], ReplaceNode, <|
        "ReplacementNode" -> choice2,
        Source -> data[[Key[Source]]]
      |>]
    }
  |>]}
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
  {InspectionObject["ExpectedSymbol", "Suspicious syntax.", "Warning", <| data, ConfidenceLevel -> 0.55 |>]}
]]






Attributes[scanPrefixPlus] = {HoldRest}

scanPrefixPlus[pos_List, aggIn_] :=
Module[{agg, node, data, issues, op},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];

  op = node[[2, 1]];

  data = op[[3]];

  issues = {};

  AppendTo[issues, InspectionObject["PrefixPlus", "Unexpected prefix ``Plus``.", "Remark", <|Source->data[Source], ConfidenceLevel->0.9|>]];

  issues
]







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






Attributes[scanUppercasePatternBlank] = {HoldRest}

scanUppercasePatternBlank[pos_List, aggIn_] :=
 Module[{agg, node, tag, data, children, src, sym, context, name, issues},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  tag = node[[1]];
  children = node[[2]];
  data = node[[3]];

  sym = children[[1]];

  name = sym["String"];

  (*
  Determine context of symbol
  *)
  With[{name = name},
  context =
    Quiet[
      Check[
        Context[name]
        ,
        StringJoin[Most[StringSplit[name, "`"]]]
        ,
        {Context::notfound}
      ]
      ,
      {Context::notfound}
    ];
  ];

  issues = {};

  If[context == "System`",

    src = sym[[3, Key[Source]]];

    AppendTo[issues, InspectionObject["SystemPatternBlank", "Unexpected **System`** symbol as pattern name.", "Error",
                      <|  Source->src,
                          ConfidenceLevel->0.95|>]];
    ,
    (* not in System`*)

    src = sym[[3, Key[Source]]];

    AppendTo[issues, InspectionObject["UppercasePatternBlank", "Suspicious uppercase symbol as pattern name.", "Remark",
                      <|  Source->src,
                          ConfidenceLevel->0.80|>]];
  ];

  issues
]


Attributes[scanBlankPredicate] = {HoldRest}

scanBlankPredicate[pos_List, aggIn_] :=
  Catch[
  Module[{agg, node, tag, data, children, src, predName, pred, issues},
    agg = aggIn;
    node = Extract[agg, {pos}][[1]];
    tag = node[[1]];
    children = node[[2]];
    data = node[[3]];

    pred = children[[2]];

    predName = pred["String"];

    issues = {};

    (*
    white-list
    *)
    If[MemberQ[{
      (*
      the special functions that end in Q, but are not functions that return Booleans
      *)
      "EllipticNomeQ", "HypergeometricPFQ", "InverseEllipticNomeQ", "LegendreQ", "MarcumQ", "PartitionsQ", "QHypergeometricPFQ",
      (*
      2-arg functions
      *)
      "MemberQ", "StringContainsQ"}, predName],
      Throw[issues]
    ];

    src = pred[[3, Key[Source]]];

    AppendTo[issues, InspectionObject["BlankPredicate", "Unexpected predicate after blank.", "Error",
      <| Source->src,
         ConfidenceLevel->0.95,
         CodeActions -> {
          CodeAction["Insert ``?``", InsertNode, <|Source->src, "InsertionNode"->LeafNode[Token`Question, "?", <||>] |>] } |>]];

    issues
  ]]


Attributes[scanSymbolPatternTest] = {HoldRest}

scanSymbolPatternTest[pos_List, aggIn_] :=
  Catch[
  Module[{agg, node, tag, data, children, qSrc, a, q, b, aSrc, aName, issues},
    agg = aggIn;
    node = Extract[agg, {pos}][[1]];
    tag = node[[1]];
    children = node[[2]];
    data = node[[3]];

    a = children[[1]];
    q = children[[2]];
    b = children[[3]];

    issues = {};

    aName = a["String"];

    (*
    Heuristic here:

    In something like:

    pat?test

    pat is being used as a pattern, so ok
    *)
    If[StringContainsQ[aName, "pat", IgnoreCase -> True],
      Throw[issues]
    ];

    aSrc = a[[3, Key[Source]]];
    qSrc = q[[3, Key[Source]]];

    AppendTo[issues, InspectionObject["SymbolPatternTest", "Unexpected ``PatternTest`` after symbol.", "Error",
      <| Source->qSrc,
         (*
         Lower from .95 to .85 because it is somewhat common to use the 1-arg operator forms of predicates as objects
         *)
         ConfidenceLevel->0.85,
         CodeActions -> {
          CodeAction["Insert ``_`` behind", InsertNode, <|Source->qSrc, "InsertionNode"->LeafNode[Token`Under, "_", <||>] |>],
          CodeAction["Insert ``_`` in front", InsertNode, <|Source->aSrc, "InsertionNode"->LeafNode[Token`Under, "_", <||>] |>] } |>]];

    issues
  ]]


Attributes[scanMessageName] = {HoldRest}

scanMessageName[pos_List, aggIn_] :=
Module[{agg, node, data, issues, children, rand},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];

  children = node[[2]];

  rand = children[[5]];

  data = rand[[3]];

  issues = {};

  AppendTo[issues, InspectionObject["MessageName", "Unexpected argument to ``MessageName``.", "Error", <|Source->data[Source], ConfidenceLevel->0.9|>]];

  issues
]




End[]


EndPackage[]
