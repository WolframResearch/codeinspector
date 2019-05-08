BeginPackage["Lint`Rules`"]

$DefaultConcreteRules

$DefaultAbstractRules


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]
Needs["Lint`"]
Needs["Lint`Format`"]



(*

Rules are of the form: pat -> func where pat is the node pattern to match on and func is the processing function for the node.

Functions are of the form: function[pos_, ast_] where pos is the position of the node in the AST, and ast is the AST itself.
  And function must return a list of Lints. 


A rule of thumb is to make patterns as specific as possible, to offload work of calling the function.

*)

$DefaultConcreteRules = <|



(*
Concrete lints
*)

(*
Tags: ImplicitTimesAcrossLines
*)
InfixNode[ImplicitTimes, _, KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]] -> scanImplicitTimes,
InfixNode[ImplicitTimes, { BlankNullSequenceNode[_, _, _], _, BlankNode[_, _, _] }, _] -> scanImplicitTimesBlanks,

(*
Tags: DotDifferentLine
*)
InfixNode[Dot, _, KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]] -> scanDots,

(*
Tags: SuspiciousSpan
*)
BinaryNode[Span, _, _] -> scanSpans,
TernaryNode[Span, _, _] -> scanSpans,


(*
Tags: StraySemicolon
*)
(*
InfixNode[CompoundExpression, _, _] -> scanCompoundExpressions,
*)

(*
Tags: SuspiciousOut
*)
OutNode[_, _, _] -> scanOuts,

(*

a_?b[x]
probably meant to have a_?(b[x])

will also scan for:
a_?b[x]&

*)
CallNode[BinaryNode[PatternTest, _, _], {_}, _] -> scanPatternTestCalls,

BinaryNode[PatternTest, {_, _, SymbolNode[Symbol, "Association"|"String"|"Integer"|"Real"|"Failure", _]}, _] -> scanBadSymbolPatternTests,


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



PrefixNode[Information, _, _] -> scanInformation,


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


Nothing
|>





$DefaultAbstractRules = <|


CallNode[SymbolNode[Symbol, "String", _], _, _] -> scanStringCalls,
CallNode[SymbolNode[Symbol, "Integer", _], _, _] -> scanIntegerCalls,
CallNode[SymbolNode[Symbol, "Real", _], _, _] -> scanRealCalls,
(*

not a good scan

CallNode[SymbolNode["Failure", _, _], _, _] -> scanFailureCalls,
*)

(*
Tags: Control
*)
SymbolNode[Symbol, "Return" | "Break" | "Continue", _] -> scanControls,


CallNode[SymbolNode[Symbol, "Pattern", _], _, _] -> scanPatterns,

(*
Tags: WhichArguments SwitchWhichConfusion
*)
CallNode[SymbolNode[Symbol, "Which", _], _, _] -> scanWhichs,

(*
Tags: SwitchArguments SwitchWhichConfusion OperatingSystemLinux
*)
CallNode[SymbolNode[Symbol, "Switch", _], _, _] -> scanSwitchs,

(*
Tags: DuplicateKeys
*)
CallNode[SymbolNode[Symbol, "Association", _], {CallNode[SymbolNode[Symbol, "Rule" | "RuleDelayed", _], _, _] ...}, _] -> scanAssocs,

(*
Tags: 
*)
CallNode[SymbolNode[Symbol, "Module", _], _, _] -> scanModules,

(*
Tags: 
*)
CallNode[SymbolNode[Symbol, "DynamicModule", _], _, _] -> scanDynamicModules,

(*
Tags: 
*)
CallNode[SymbolNode[Symbol, "With", _], _, _] -> scanWiths,

(*
Tags: 
*)
CallNode[SymbolNode[Symbol, "Block", _], _, _] -> scanBlocks,

(*
Tags: 
*)
(*
1-arg Optional[] is ok to have named patterns
Only scan 2-arg Optionals
*)
CallNode[SymbolNode[Symbol, "Optional", _], {_, _}, _] -> scanOptionals,


(*
Scan all symbols that are intuitive, yet do not exist
*)
SymbolNode[Symbol, "AnyFalse" | "AllFalse" | "Failed"(*|"Boolean"*), _] -> scanBadSymbols,

(*

If LoadJavaClass["java.lang.System"] is called, then these symbols are created in System`

It is therefore dangerous to use these symbols in production code where it is unknown whether JLink will be used.


too noisy


SymbolNode[Symbol, "arraycopy" | "clearProperty" | "console" | "currentTimeMillis" | "err" | "exit" | "gc" |
                      "getenv" | "getProperties" | "getProperty" | "getSecurityManager" | "identityHashCode" |
                      "in" | "inheritedChannel" | "lineSeparator" | "load" | "loadLibrary" | "mapLibraryName" |
                      "nanoTime" | "out" | "runFinalization" | "runFinalizersOnExit" | "setErr" | "setIn" |
                      "setOut" | "setProperties" | "setProperty" | "setSecurityManager", _] -> scanJavaSystemSymbols,
*)

CallNode[SymbolNode[Symbol, "LoadJavaClass" | "JLink`LoadJavaClass", _], { StringNode[String, "\"java.lang.System\"", _] }, _] -> scanLoadJavaClassSystem,





(*
scan for a := a  and  a = a
possible results from batch renaming symbols
*)
CallNode[SymbolNode[Symbol, "Set" | "SetDelayed", _], { SymbolNode[Symbol, token_, _], SymbolNode[Symbol, token_, _] }, _] -> scanSelfAssignments,


ContextNode[{StringNode[String, "\"Private`\"", _]}, _, _] -> scanPrivateContextNode,


(*

experimental

detect a?fooQ  when you meant a_?fooQ

currently too difficult to determine what is a pattern

CallNode[SymbolNode[Symbol, "PatternTest", _], {
                      lhs_ /; FreeQ[lhs, CallNode[SymbolNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _]],
                      _}, _] -> scanPatternTestMissingPattern,
*)



(*

experimental

detect calls like f[a_] := a_


TODO: A clever thing to do would be to detet when inside of Quiet[ ,{Rule::rhs, RuleDelayed::rhs}] and turn off this check

CallNode[SymbolNode[Symbol, "Set" | "SetDelayed", _], { lhs_, rhs_ } /;
            Intersection[Cases[lhs, CallNode[SymbolNode[Symbol, "Pattern", _], {SymbolNode[Symbol, name_, _], _}, _] :> name, {0, Infinity}],
                          Cases[rhs, CallNode[SymbolNode[Symbol, "Pattern", _], {SymbolNode[Symbol, name_, _], _}, _] :> name, {0, Infinity}]] != {}, _] -> scanRHSPatterns,
*)



(*
cst of [x] is fine
ast of [x] is an error
*)
AbstractSyntaxErrorNode[_, _, _] -> scanAbstractSyntaxErrorNodes,



(*
Tags: SyntaxError NotContiguous
*)
KeyValuePattern[AbstractSyntaxIssues -> _] -> scanAbstractSyntaxIssues,



Nothing
|>





Attributes[scanImplicitTimes] = {HoldRest}

scanImplicitTimes[pos_List, cstIn_] :=
Module[{cst, node, children, data, warnings, line, nextLine},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  warnings = {};

  line = children[[1]][[3]][Source][[2,1]];
  Do[
    nextLine = n[[3]][Source][[1,1]];
    If[line != nextLine,
      AppendTo[warnings,
        Lint["ImplicitTimesAcrossLines", "Implicit Times across lines.\n\
Did you mean ``;`` or ``,``?", "Warning", data]];
      Break[];
    ];
    line = n[[3]][Source][[2,1]];
    ,
    {n, children[[2;;]]}
  ];

  warnings
]

Attributes[scanImplicitTimesBlanks] = {HoldRest}

scanImplicitTimesBlanks[pos_List, cstIn_] :=
Module[{cst, node, data},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  data = node[[3]];

  {Lint["ImplicitTimesBlanks", "Implicit Times between ``___`` and ``_``.\n\
Did you mean ``___``?", "Error", data]}
]




Attributes[scanDots] = {HoldRest}

scanDots[pos_List, cstIn_] :=
Module[{cst, node, children, data, issues, line, nextLine},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  line = children[[1]][[3]][Source][[2,1]];
  Do[
    nextLine = n[[3]][Source][[1,1]];
    If[line != nextLine,
      AppendTo[issues,
        Lint["DotDifferentLine", "Operands for ``.`` are on different lines.\n\
Did you mean ``;`` or ``,``?", "Warning", data]];
      Break[];
    ];
    line = n[[3]][Source][[2,1]];
    ,
    {n, children[[3;;;;2]]}
  ];

  issues
]


Attributes[scanSpans] = {HoldRest}

scanSpans[pos_List, cstIn_] :=
Catch[
 Module[{cst, node, children, data, issues},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  (*
  The position {2, _} tests for top-level, because the cst looks like FileNode[File, { SpanNode }, <||>]
  *)
  If[MatchQ[pos, {2, _}],
    (* top node, no parent *)

    AppendTo[issues, Lint["SuspiciousSpan", "Suspicious use of ``;;`` at top-level.\n\
Did you mean ``;``?", "Warning", data]];
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

the parser returns a Warning

StraySemicolon is to find things like this:

(f[];
; Throw[$Failed, $tag])

*)

(*
Attributes[scanCompoundExpressions] = {HoldRest}

scanCompoundExpressions[pos_List, cstIn_] :=
Module[{cst, node, children, data, issues, internalNulls},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  (*
  say Most because we don't care about the last ;

  But if Most[children] does have InternalNullNodes, then we also want to include the last child, because
  that is the ; that is most likely to be incorrect.
  *)
  internalNulls = Cases[Most[children], InternalNullNode[Null, _, _]];
  If[!empty[internalNulls],
    internalNulls = Cases[children, InternalNullNode[Null, _, _]];
  ];

  (*
  a bit noisy. Remark for now
  *)

  Scan[(AppendTo[issues, Lint["StraySemicolon", "``;`` may not be needed.", "Warning", #[[3]]]])&, internalNulls];

  issues
]
*)



Attributes[scanOuts] = {HoldRest}

scanOuts[pos_List, cstIn_] :=
 Module[{cst, node, data, s},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  s = node["String"];
  data = node[[3]];
  {Lint["SuspiciousOut", "Suspicious use of ``" <> s <> "`` in file.", "Warning", data]}
]


Attributes[scanPatternTestCalls] = {HoldRest}

(*
warn about a_?b[x] which actually parses as (a_?b)[x] and not a_?(b[x])
*)
scanPatternTestCalls[pos_List, cstIn_] :=
Catch[
 Module[{cst, node, data, children, patternTest, args, patternTestChildren, patternTestArg1, patternTestArg2, issues,
  parent, parentPos},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  patternTest = node[[1]];
  children = node[[2]];

  patternTestChildren = patternTest[[2]];
  patternTestArg1 = patternTestChildren[[1]];
  patternTestArg2 = patternTestChildren[[3]];
  args = children[[1]];
  data = node[[3]];


  If[Length[pos] >= 2,
    parentPos = Drop[pos, -2];
    parent = Extract[cst, {parentPos}][[1]];

    If[MatchQ[parent, PostfixNode[Function, _, _]],
      Throw[scanPatternTestCallFunctions[parentPos, cst]]
     ];
  ];

  issues = {};

  (*

  this is noisy, so make Remark for now

  *)

  AppendTo[issues, Lint["SuspiciousPatternTestCall", "Suspicious use of ``?``.\n\
The precedence of ``?`` is surprisingly high.\n\
``PatternTest`` ``" <> ToInputFormString[patternTest] <> "`` is calling arguments ``" <> ToInputFormString[args] <> "``.\n\
Did you mean ``" <> ToInputFormString[BinaryNode[PatternTest, {
                                        patternTestArg1,
                                        TokenNode[Token`Question, "?", <||>],
                                        GroupNode[GroupParen, {
                                          TokenNode[Token`OpenParen, "(", <||>],
                                          CallNode[patternTestArg2, {args}, <||>],
                                          TokenNode[Token`CloseParen, ")", <||>] }, <||>]}, <||>]] <>
        "`` or ``" <> ToInputFormString[CallNode[GroupNode[GroupParen, {
                                                    TokenNode[Token`OpenParen, "(", <||>],
                                                    patternTest,
                                                    TokenNode[Token`CloseParen, ")", <||>] }, <||>], children, <||>]] <> "``?", "Remark", data]];

  issues

]]



Attributes[scanBadSymbolPatternTests] = {HoldRest}

scanBadSymbolPatternTests[pos_List, cstIn_] :=
Catch[
 Module[{cst, node, data, children, patternTestArg1, patternTestArg2, issues},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];

  patternTestArg1 = children[[1]];
  patternTestArg2 = children[[3]];
  data = node[[3]];

  issues = {};

  (*
  catch cases like a_?Association  when it should have been a_?AssociationQ
  *)
  Switch[patternTestArg2,
    SymbolNode[Symbol, "Association", _],
      AppendTo[issues, Lint["AssociationCall", "Calling ``Association`` as a function.\n\
Did you mean ``AssociationQ``?", "Error", patternTestArg2[[3]]]];
    ,
    SymbolNode[Symbol, "String", _],
      AppendTo[issues, Lint["StringCall", "Calling ``String`` as a function.\n\
Did you mean ``StringQ``?", "Error", patternTestArg2[[3]]]];
    ,
    SymbolNode[Symbol, "Integer", _],
      AppendTo[issues, Lint["IntegerCall", "Calling ``Integer`` as a function.\n\
Did you mean ``IntegerQ``?", "Error", patternTestArg2[[3]]]];
    ,
    SymbolNode[Symbol, "Real", _],
      AppendTo[issues, Lint["RealCall", "Calling ``Real`` as a function.\n\
Did you mean ``RealQ``?", "Error", patternTestArg2[[3]]]];
    ,
    SymbolNode[Symbol, "Failure", _],
      AppendTo[issues, Lint["FailureCall", "Calling ``Failure`` as a function.\n\
Did you mean ``FailureQ``?", "Error", patternTestArg2[[3]]]];
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
scanRuleFunctions[pos_List, cstIn_] :=
Catch[
 Module[{cst, node, data, children, rule, ruleHead, ruleChild1, ruleChild2, parentPos, parent},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  rule = children[[1]];
  ruleChild1 = rule[[2]][[1]];

  (*
  we want to reduce the number of false positives so here are some heuristics:
  *)

  (*
  heuristic
  if # or ## occur in LHS of Rule, then there is no problem, (a->b)& is the intended parse
  *)
  If[!FreeQ[ruleChild1, _SlotNode | _SlotSequenceNode],
    Throw[{}]
  ];

  If[pos != {},
    parentPos = Most[pos];
    parent = Extract[cst, {parentPos}][[1]];
    While[parentPos != {} && (ListQ[parent] || MatchQ[parent, GroupNode[GroupSquare, _, _]]),
     parentPos = Most[parentPos];
     parent = Extract[cst, {parentPos}][[1]];
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
     If[MatchQ[parent, CallNode[SymbolNode[Symbol, "Map" | "MapAt", _], {GroupNode[GroupSquare, {_, node, __}, _]}, _]],
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
``" <> SymbolName[ruleHead] <> "`` ``" <> ToInputFormString[rule] <> "`` is inside a ``Function``.\n\
Did you mean ``" <>
      ToInputFormString[BinaryNode[ruleHead, {
        ruleChild1,
        rule[[2]][[2]],
        GroupNode[GroupParen, {
          TokenNode[Token`OpenParen, "(", <||>],
          PostfixNode[Function, {
            ruleChild2,
            TokenNode[Token`Amp, "&", <||>] }, <||>],
          TokenNode[Token`CloseParen, ")", <||>] }, <||>] }, <||>]] <> "`` or ``" <>
      ToInputFormString[PostfixNode[Function, {
                          GroupNode[GroupParen, {
                            TokenNode[Token`OpenParen, "(", <||>],
                            rule,
                            TokenNode[Token`CloseParen, ")", <||>] }, <||>],
                          TokenNode[Token`Amp, "&", <||>] }, <||>]] <> "``?", "Warning", data]}
]]



Attributes[scanPatternTestFunctions] = {HoldRest}

(*
warn about a?b& which parses as (a?b)& and not a?(b&)
*)
scanPatternTestFunctions[pos_List, cstIn_] :=
Catch[
 Module[{cst, node, data, children, patternTest, patternTestChildren, patternTestArg1, patternTestArg2},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  patternTest = children[[1]];
  patternTestChildren = patternTest[[2]];
  patternTestArg1 = patternTestChildren[[1]];
  patternTestArg2 = patternTestChildren[[3]];
  {Lint["SuspiciousPatternTestFunction", "Suspicious use of ``&``.\n\
The precedence of ``&`` is surprisingly low and the precedence of ``?`` is surprisingly high.\n\
``?`` is inside a ``Function``.\n\
Did you mean ``" <>
  ToInputFormString[BinaryNode[PatternTest, {
                        patternTestArg1,
                        TokenNode[Token`Question, "?", <||>],
                        GroupNode[GroupParen, {
                          TokenNode[Token`OpenParen, "(", <||>],
                          PostfixNode[Function, {
                            patternTestArg2,
                            TokenNode[Token`Amp, "&", <||>] }, <||>],
                          TokenNode[Token`CloseParen, ")", <||>] }, <||>] }, <||>]] <> 
          "`` or ``" <> ToInputFormString[PostfixNode[Function, {
                                            GroupNode[GroupParen, {
                                              TokenNode[Token`OpenParen, "(", <||>],
                                              patternTest,
                                              TokenNode[Token`CloseParen, ")", <||>] }, <||>]}, <||>]] <> "``?", "Warning", data]}
]]





(*

this is called from scanPatternTestCalls and not the dispatcher

*)

Attributes[scanPatternTestCallFunctions] = {HoldRest}

(*
warn about a?b[#]& which parses as (a?b[#])& and not a?(b[#]&)
*)
scanPatternTestCallFunctions[pos_List, cstIn_] :=
Catch[
 Module[{cst, node, data, children, call, patternTest, callChildren, patternTestChildren, patternTestArg1, patternTestArg2},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
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
Call to ``PatternTest`` ``" <> ToInputFormString[call] <> "`` is inside a ``Function``.\n\
Did you mean ``" <> ToInputFormString[BinaryNode[PatternTest, {
                                        patternTestArg1,
                                        TokenNode[Token`Question, "?", <||>],
                                        GroupNode[GroupParen, {
                                          TokenNode[Token`OpenParen, "(", <||>],
                                          PostfixNode[Function, {
                                            CallNode[patternTestArg2, callChildren, <||>],
                                            TokenNode[Token`Amp, "&", <||>] }, <||>],
                                          TokenNode[Token`CloseParen, ")", <||>] }, <||>] }, <||>]] <>
  "`` or ``" <> ToInputFormString[PostfixNode[Function, {
                                    CallNode[GroupNode[GroupParen, {
                                      TokenNode[Token`OpenParen, "(", <||>],
                                      patternTest,
                                      TokenNode[Token`CloseParen, ")", <||>] }, <||>], callChildren, <||>],
                                    TokenNode[Token`Amp, "&", <||>] }, <||>]] <> "``?",
    "Warning", data]}
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
scanPatternBlankOptionals[pos_List, cstIn_] :=
Catch[
 Module[{cst, node, data, children, patternBlank, patternBlankChildren, pattern, opt},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
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
  If[FreeQ[opt, BlankNode[Blank, _, _] |
                BlankSequenceNode[BlankSequence, _, _] |
                BlankNullSequenceNode[BlankNullSequence, _, _] |
                PatternBlankNode[PatternBlank, _, _] |
                PatternBlankSequenceNode[PatternBlankSequence, _, _] |
                PatternBlankNullSequenceNode[PatternBlankNullSequence, _, _] |
                BinaryNode[Pattern, _, _] |
                (* also check for Alternatives *)
                InfixNode[Alternatives, _, _]
                ],
    Throw[{}]
  ];

  {Lint["SuspiciousPatternBlankOptional", "Suspicious use of ``:``.\n\
Did you mean ``" <> ToInputFormString[BinaryNode[Pattern, {
                        pattern,
                        TokenNode[Token`Fake`PatternColon, ":", <||>],
                        opt}, <||>]] <> "``?\n\
This may be ok if ``" <> ToInputFormString[pattern] <> "`` is used as a pattern.", "Warning", data]}
]]





Attributes[scanInformation] = {HoldRest}

scanInformation[pos_List, cstIn_] :=
Module[{cst, node, data, children, tok},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  tok = children[[1]];

  {Lint["SuspiciousInformation", "Suspicious use of ``" <> tok["String"] <> "`` syntax in file.", "Error", data]}
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





Attributes[scanSyntaxErrorNodes] = {HoldRest}

scanSyntaxErrorNodes[pos_List, cstIn_] :=
 Module[{cst, node, tag, data, tagString},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  tag = node[[1]];
  data = node[[3]];

  tagString = Block[{$ContextPath = {"AST`", "System`"}, $Context = "Lint`Scratch`"}, ToString[tag]];

  {Lint["SyntaxError", "``" <> tagString <> "``", "Fatal", data]}
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







(*

Abstract rules

*)


Attributes[scanStringCalls] = {HoldRest}

scanStringCalls[pos_List, astIn_] :=
 Module[{ast, node, children, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  {Lint["StringCall", "Calling ``String`` as a function.\n\
Did you mean ``StringQ``?\n\
This may be ok if ``String`` is handled programmatically.", "Error", data]}
  ]

Attributes[scanIntegerCalls] = {HoldRest}

scanIntegerCalls[pos_List, astIn_] :=
 Module[{ast, node, children, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  {Lint["IntegerCall", "Calling ``Integer`` as a function.\n\
Did you mean ``IntegerQ``?\n\
This may be ok if ``Integer`` is handled programmatically.", "Error", data]}
  ]

Attributes[scanRealCalls] = {HoldRest}

scanRealCalls[pos_List, astIn_] :=
 Module[{ast, node, children, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  {Lint["RealCall", "Calling ``Real`` as a function.\n\
Did you mean ``RealQ``?\n\
This may be ok if ``Real`` is handled programmatically.", "Error", data]}
  ]

(*

not a good scan

Attributes[scanFailureCalls] = {HoldRest}

scanFailureCalls[pos_List, astIn_] :=
 Module[{ast, node, children, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  If[Length[children] == 1,
  	{Lint["FailureCall", {"Calling ", LintBold["Failure"], " as a function. Did you mean ", LintBold["FailureQ"],
      "? This may be ok if ", LintBold["Failure"], " is handled programmatically."}, "Error", data]}
  	,
  	{}
  ]
  ]
*)





Attributes[scanAssocs] = {HoldRest}

scanAssocs[pos_List, astIn_] :=
 Module[{ast, node, children, data, opLocation, duplicates, selected},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  opLocation = data[Source];
  
    duplicates = Keys[Select[CountsBy[children[[All, 2, 1]], ToFullFormString], # > 1&]];
   selected = Flatten[Select[children[[All, 2, 1]], Function[{key}, ToFullFormString[key] === #]]& /@ duplicates, 1];

   {Lint["DuplicateKeys", "Duplicate keys in ``Association``.", "Error", #[[3]]]}& /@ selected

  ]



Attributes[scanWhichs] = {HoldRest}

scanWhichs[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, warnings, span, duplicates, selected},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  
  warnings = {};

  If[empty[children],
    AppendTo[warnings, 
     Lint["WhichArguments", "``Which`` does not have any arguments.\n\
This may be ok if ``Which`` has pattern arguments.", "Error", data]];
    Throw[warnings]
  ];

  If[!EvenQ[Length[children]],
    AppendTo[warnings, 
     Lint["WhichArguments", "``Which`` does not have even number of arguments.\n\
This may be ok if ``Which`` has pattern arguments.", "Error", data]];
    Throw[warnings]
  ];


  If[MatchQ[children[[1]], SymbolNode[Symbol, "$OperatingSystem", _]],
    span = children[[1]][[3]];
   AppendTo[warnings, 
    Lint["SwitchWhichConfusion", "``Which`` has ``$OperatingSystem`` in first place.\n\
Did you mean ``Switch``?", "Error", span]];
  ];

  If[MatchQ[children[[-2]], CallNode[SymbolNode[Symbol, "Blank", _], _, _]],
    span = children[[-2]][[3]];
   AppendTo[warnings, 
    Lint["SwitchWhichConfusion", "``Which`` has ``_`` in last place.\n\
Did you mean ``True``?", "Error", span]];
  ];

    duplicates = Keys[Select[CountsBy[children[[;;;;2]], ToFullFormString], # > 1&]];
   selected = Flatten[Select[children[[;;;;2]], Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
   Scan[
    AppendTo[warnings,
      Lint["DuplicateClauses", "Duplicate clauses in ``Which``.", "Error", #[[3]]]
    ]&
    ,
    selected
   ];


  warnings
  ]]




Attributes[scanSwitchs] = {HoldRest}

scanSwitchs[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, span, cases, duplicates, issues, pairs, form, value,
    formPatternNames, selected, valuePatterns},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  
  issues = {};

  If[Length[children] == 1,
   AppendTo[issues, Lint["SwitchArguments", "``Switch`` only has one argument.\n\
This may be ok if ``Switch`` has pattern arguments.", "Error", data]];
   Throw[issues];
  ];


  If[!OddQ[Length[children]],
   AppendTo[issues, Lint["SwitchArguments", "``Switch`` does not have odd number of arguments.\n\
This may be ok if ``Switch`` has pattern arguments.", "Error", data]];
   Throw[issues];
  ];

  If[MatchQ[children[[1]], SymbolNode[Symbol, "$OperatingSystem", _]],
   cases = Cases[children[[2;;-1;;2]], StringNode[String, "\"Linux\"", _], {0, Infinity}];
   If[cases =!= {},
    span = cases[[1]][[3]];
    AppendTo[issues, Lint["OperatingSystemLinux", "``\"Linux\"`` is not a value of ``$OperatingSystem``.\n\
Did you mean ``\"Unix\"``?", "Error", span]];
   ]
  ];

  (*
   Switch has True in last place like this: Switch[a,1,b,True,c]
   *)
  If[MatchQ[children[[-2]], SymbolNode[Symbol, "True", _]],
   (* presence of False makes it less likely that True is unintended *)
   If[FreeQ[children[[2;;-4;;2]], SymbolNode[Symbol, "False", _]],
    span = children[[-2]][[3]];
    AppendTo[issues, Lint["SwitchWhichConfusion", "``Switch`` has ``True`` in last place.\n\
Did you mean ``_``?", "Warning", span]];
   ]
  ];

  duplicates = Keys[Select[CountsBy[children[[2;;;;2]], ToFullFormString], # > 1&]];
  selected = Flatten[Select[children[[2;;;;2]], Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
  Scan[(AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``Switch``.", "Error", #[[3]]]])&, selected];


  pairs = Partition[children[[2;;]], 2];

  Scan[(
    form = #[[1]];
    value = #[[2]];
    formPatternNames = Cases[form, CallNode[SymbolNode[Symbol, "Pattern", _], {SymbolNode[Symbol, n_, _], _}, _] :> n, {0, Infinity}];

    Scan[(
      
      valuePatterns = Cases[value, SymbolNode[Symbol, #, _], {0, Infinity}];
      If[empty[valuePatterns],
        (*
        too noisy
        add a Remark about unused named pattern in Switch? *)
        Null
        ,
        (*
        too many false positives, so make this a Remark for now
        
        experimental

        Scan[(AppendTo[issues, Lint["NamedPatternInSwitch", "Named pattern in ``Switch``: ``" <> ToFullFormString[#] <> "``.\n\
The pattern ``" <> ToFullFormString[#] <> "`` occurs in the matching form, but ``Switch`` does not support pattern replacement.\n\
This may be ok if ``" <> ToFullFormString[#] <> "`` is set before being used.\n\
Consider removing the named pattern ``" <> ToFullFormString[#] <> "``.", "Remark", #[[3]]]])&, valuePatterns]*)
        Null
      ]

      )&, formPatternNames];

    )&, pairs];

  issues
]]


Attributes[scanPatterns] = {HoldRest}

scanPatterns[pos_List, astIn_] :=
 Module[{ast, node, patSymbol, name, rhs, children, patterns, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  
  children = node[[2]];
  
  patSymbol = children[[1]];
  name = patSymbol["String"];
  rhs = children[[2]];

  issues = {};

  patterns = Cases[rhs, CallNode[SymbolNode[Symbol, "Pattern", _], _, _], {0, Infinity}];
  Scan[(
    If[#[[2]][[1]]["String"] == name,
      (*
      This is never correct code, but make a Warning for now because it is noisy
      *)
      AppendTo[issues, Lint["DuplicateNamedPattern", "Duplicate named pattern ``" <> name <> "`` in RHS of ``Pattern``.", "Warning", #[[3]]]];
    ];
  )&, patterns];

  issues
]



Attributes[scanControls] = {HoldRest}

scanControls[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data, parentPos, parent, s},
  If[pos == {},
    (* top node, no parent *)
    Throw[{}]
  ];
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  s = node["String"];
  data = node[[3]];
  
  parentPos = Most[pos];
  parent = Extract[ast, {parentPos}][[1]];
  While[ListQ[parent],
   parentPos = Most[parentPos];
   parent = Extract[ast, {parentPos}][[1]];
   ];

   If[MatchQ[parent, CallNode[node, _, _]],
    Throw[{}]
   ];

  {Lint["Control", "``" <> s <> "`` appears but is not called.\n\
Did you mean ``" <> s<>"[]``?\n\
This may be ok if ``" <> s <> "`` is used as a symbol.", "Warning", data]}
  ]]




Attributes[scanModules] = {HoldRest}

scanModules[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, params, warnings, vars, used, unusedParams},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  warnings = {};

  If[Length[children] != 2,
    AppendTo[warnings, Lint["ModuleArguments", "``Module`` does not have 2 arguments.\n\
This may be ok if ``Module`` is handled programmatically.", "Error", data]];
    Throw[warnings]
  ];

  If[!MatchQ[children[[1]], CallNode[SymbolNode[Symbol, "List", _], _, _]],
    AppendTo[warnings, Lint["ModuleArguments", "``Module`` does not have a ``List`` for argument 1.\n\
This may be ok if ``Module`` is handled programmatically.", "Error", data]];
    Throw[warnings]
  ];


  params = children[[1,2]];
   vars = # /. {CallNode[SymbolNode[Symbol, "Set"|"SetDelayed", _], {sym:SymbolNode[_, _, _], _}, _] :> sym,
            sym:SymbolNode[_, _, _] :> sym,
            (*

            Compiler syntax includes:
            Module[{ Typed[x, "Integer64"] }, x]

            TODO: support this

            CallNode[SymbolNode["Typed", {}, _], { sym:SymbolNode[_, _, _], _ }, _] :> sym
            *)
            err_ :> (AppendTo[warnings, Lint["ModuleArguments", "Variable ``" <> ToFullFormString[err] <>
              "`` does not have proper form.\n\
This may be ok if ``Module`` is handled programmatically.", "Error", #[[3]]]]; Nothing)}& /@ params;
    duplicates = Keys[Select[CountsBy[vars, ToFullFormString], # > 1&]];
    selected = Flatten[Select[vars, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
    Scan[AppendTo[warnings, Lint["DuplicateVariables", "Duplicate variables in ``Module``: ``" <> ToFullFormString[#] <> "``.", "Error", #[[3]]]]&, selected];

  used = ToFullFormString /@ Cases[children[[2]], _SymbolNode, {0, Infinity}];
  unusedParams = Select[vars, Function[{c}, !MemberQ[used, ToFullFormString[c]]]];

  Scan[AppendTo[warnings, Lint["UnusedVariables", "Unused variables in ``Module``: ``" <> ToFullFormString[#] <> "``.", "Warning", #[[3]]]]&, unusedParams];

  warnings
]]


Attributes[scanDynamicModules] = {HoldRest}

scanDynamicModules[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, params, warnings, vars, used, unusedParams},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  warnings = {};

  If[!(Length[children] >= 2),
    AppendTo[warnings, Lint["DynamicModuleArguments", "``DynamicModule`` does not have 2 arguments.\n\
This may be ok if ``DynamicModule`` is handled programmatically.", "Error", data]];
    Throw[warnings]
  ];

  (*

  DynamicModule takes options

  If[Length[children] != 2,
    AppendTo[warnings, Lint["DynamicModuleArguments", {LintBold["DynamicModule"], " does not have 2 arguments. This may be ok if ",
                              LintBold["DynamicModule"], " is handled programmatically."}, "Error", data]];
    Throw[warnings]
  ];
  *)

  If[!MatchQ[children[[1]], CallNode[SymbolNode[Symbol, "List", _], _, _]],
    AppendTo[warnings, Lint["DynamicModuleArguments", "``DynamicModule`` does not have a ``List`` for argument 1.\n\
This may be ok if ``DynamicModule`` is handled programmatically.", "Error", data]];
    Throw[warnings]
  ];


  params = children[[1,2]];
   vars = # /. {CallNode[SymbolNode[Symbol, "Set"|"SetDelayed", _], {sym:SymbolNode[_, _, _], _}, _] :> sym,
            sym:SymbolNode[_, _, _] :> sym,
            err_ :> (AppendTo[warnings, Lint["DynamicModuleArguments", "Variable ``" <> ToFullFormString[err] <>
              "`` does not have proper form.\n\
This may be ok if ``DynamicModule`` is handled programmatically.", "Error", #[[3]]]]; Nothing)}& /@ params;
    duplicates = Keys[Select[CountsBy[vars, ToFullFormString], # > 1&]];
    selected = Flatten[Select[vars, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
    Scan[AppendTo[warnings, Lint["DuplicateVariables", "Duplicate variables in ``DynamicModule``: ``" <> ToFullFormString[#] <> "``.", "Error", #[[3]]]]&, selected];

  used = ToFullFormString /@ Cases[children[[2]], _SymbolNode, {0, Infinity}];
  unusedParams = Select[vars, Function[{c}, !MemberQ[used, ToFullFormString[c]]]];

  Scan[AppendTo[warnings, Lint["UnusedVariables", "Unused variables in ``DynamicModule``: ``" <> ToFullFormString[#] <> "``.", "Warning", #[[3]]]]&, unusedParams];

  warnings
]]



(*

With has an undocumented syntax for allowing variables to refer to previously With'd variables

In[32]:= With[{a=1}, {b=Hold[a]}, b+1]
Out[32]= 1+Hold[1]
*)
Attributes[scanWiths] = {HoldRest}

scanWiths[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, paramLists, warnings, varsAndVals, vars, vals, usedBody, unusedParams},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  warnings = {};

  If[Length[children] < 2,
    AppendTo[warnings, Lint["WithArguments", "``With`` does not have 2 or more arguments.\n\
This may be ok if ``With`` is handled programmatically.", "Error", data]];
    Throw[warnings];
  ];

  If[!MatchQ[Most[children], {CallNode[SymbolNode[Symbol, "List", _], _, _]...}],
    AppendTo[warnings, Lint["WithArguments", "``With`` does not have a ``List`` for most arguments.\n\
This may be ok if ``With`` is handled programmatically.", "Error", data]];
    Throw[warnings];
  ];

  paramLists = Most[children][[All, 2]];
   
   varsAndVals = Function[{list}, # /. {CallNode[SymbolNode[Symbol, "Set"|"SetDelayed", _], {sym:SymbolNode[_, _, _], val_}, _] :> {sym, val},
            err_ :> (AppendTo[warnings, Lint["WithArguments", "Variable ``" <> ToFullFormString[err] <> "`` does not have proper form.\n\
This may be ok if ``With`` is handled programmatically.", "Error", #[[3]]]]; Nothing)}& /@ list] /@ paramLists;

  If[varsAndVals == {{}},
    Throw[warnings];
  ];

   {vars, vals} = Transpose[Transpose /@ varsAndVals];

    duplicates = Keys[Select[CountsBy[#, ToFullFormString], # > 1 &]]& /@ vars;
      selected = Flatten[Function[{duplicates, vars}, (Select[vars, Function[{c}, ToFullFormString[c] === #]])& /@ duplicates] @@@ Transpose[{duplicates, vars}]];
  Scan[AppendTo[warnings, Lint["DuplicateVariables", "Duplicate variables in ``With``: ``" <> ToFullFormString[#] <> "``.", "Error", #[[3]]]]&, selected];

  usedBody = ToFullFormString /@ Cases[Last[children], _SymbolNode, {0, Infinity}];

  usedAtVariousScopes = FoldList[Join[#1, ToFullFormString /@ Cases[#2, _SymbolNode, {0, Infinity}]]&, usedBody, vals // Reverse] // Reverse;

  unusedParams = Function[{vars, useds}, Select[vars, Function[{c}, !MemberQ[useds, ToFullFormString[c]]]]] @@@ Transpose[{vars, Most[usedAtVariousScopes]}];

  unusedParams = Flatten[unusedParams];

  Scan[AppendTo[warnings, Lint["UnusedVariables", "Unused variables in ``With``: ``" <> ToFullFormString[#] <> "``.", "Warning", #[[3]]]]&, unusedParams];

  warnings
]]



Attributes[scanBlocks] = {HoldRest}

scanBlocks[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, params, warnings, varsWithSet, varsWithoutSet, toDelete},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  warnings = {};

  If[Length[children] != 2,
    AppendTo[warnings, Lint["BlockArguments", "``Block`` does not have 2 arguments.\n\
This may be ok if ``Block`` is handled programmatically.", "Error", data]];
    Throw[warnings]
  ];

  If[!MatchQ[children[[1]], CallNode[SymbolNode[Symbol, "List", _], _, _]],
    AppendTo[warnings, Lint["BlockArguments", "``Block`` does not have a ``List`` for argument 1.\n\
This may be ok if ``Block`` is handled programmatically.", "Error", data]];
    Throw[warnings]
  ];

  params = children[[1,2]];

  varsWithSet = {};
  varsWithoutSet = {};

  Scan[# /. {
    CallNode[SymbolNode[Symbol, "Set"|"SetDelayed", _], {sym:SymbolNode[_, _, _], _}, _] :> (AppendTo[varsWithSet, sym]),
    sym:SymbolNode[_, _, _] :> (AppendTo[varsWithoutSet, sym]),
    err_ :> (AppendTo[warnings, Lint["BlockArguments", "Variable ``" <> ToFullFormString[err] <> "`` does not have proper form.\n\
This may be ok if ``Block`` is handled programmatically.", "Error", #[[3]]]])}&, params];

  vars = varsWithSet ~Join~ varsWithoutSet;

  duplicates = Keys[Select[CountsBy[vars, ToFullFormString], # > 1&]];
  selected = Flatten[Select[vars, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
  Scan[AppendTo[warnings, Lint["DuplicateVariables", "Duplicate variables in ``Block``: ``" <> ToFullFormString[#] <> "``.", "Error", #[[3]]]]&, selected];

  (*
  Give unused Block variables its own tag
  *)

  used = ToFullFormString /@ Cases[children[[2]], _SymbolNode, {0, Infinity}];
  unusedParams = Select[vars, Function[{c}, !MemberQ[used, ToFullFormString[c]]]];

  (*
  Now we will use heuristics to pare down the list of unused variables in Block
  *)

  (*
  if you have Block[{x = 1}, b]  then it is probably on purpose
  i.e., setting x to a value shows intention
  *)
  toDelete = varsWithSet;
  unusedParams = Complement[unusedParams, toDelete];

  (*
  Blocking fully-qualified symbol is probably on purpose
  *)
  toDelete = Select[unusedParams, fullQualifiedSymbolQ];
  unusedParams = Complement[unusedParams, toDelete];

  (*
  after removing fully-qualified symbols, now scan for lowercase symbols and only let those through

  on the assumption that lowercase symbols will be treated as "local" variables
  *)
  unusedParams = Select[unusedParams, lowercaseSymbolQ];
  
  Scan[AppendTo[warnings, Lint["UnusedBlockVariables", "Unused variables in ``Block``: ``" <> ToFullFormString[#] <> "``.", "Warning", #[[3]]]]&, unusedParams];

  warnings
]]

(*
if there is a ` anywhere in the symbol, then assume it is fully-qualified
*)
fullQualifiedSymbolQ[SymbolNode[Symbol, s_, _]] :=
  StringContainsQ[s, "`"]

lowercaseSymbolQ[SymbolNode[Symbol, s_, _]] :=
  StringMatchQ[s, RegularExpression["[a-z].*"]]





Attributes[scanOptionals] = {HoldRest}

scanOptionals[pos_List, astIn_] :=
 Module[{ast, node, children, data, issues, opt, pats},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  issues = {};

  (*
  scan for e.g., a_:b:c
  a named pattern in 2nd arg of Optional
  *)
  opt = children[[2]];
  pats = Cases[opt, CallNode[SymbolNode[Symbol, "Pattern", _], _, _], {0, Infinity}];
  Scan[(
    AppendTo[issues, Lint["NamedPatternInOptional", "Named pattern ``" <> ToFullFormString[#[[2]][[1]]] <> "`` in ``Optional``.", "Error", #[[3]]]]
  )&, pats];

  issues
]


Attributes[scanBadSymbols] = {HoldRest}

scanBadSymbols[pos_List, astIn_] :=
 Module[{ast, node, name, data, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  name = node["String"];
  data = node[[3]];

  issues = {};

  Switch[name,
    "Failed",
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``Failed``.\n\
Did you mean ``$Failed``?", "Error", data]]
    ,
    "AnyFalse",
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``AnyFalse``.\n\
Did you mean ``AllTrue`` (and also inverting the logic) ?", "Error", data]]
    ,
    "AllFalse",
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``AllFalse``.\n\
Did you mean ``AnyTrue`` (and also inverting the logic) ?", "Error", data]]
    ,
    _,
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``" <> name <> "``.", "Error", data]]
  ];

  issues
]



(*

too noisy

Attributes[scanJavaSystemSymbols] = {HoldRest}

scanJavaSystemSymbols[pos_List, astIn_] :=
 Module[{ast, node, name, data, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  name = node["Name"];
  data = node[[3]];

  issues = {};

  AppendTo[issues, Lint["BadJavaSymbol", "Bad Java symbol: ``" <> name <> "``.\n\
It is possible that JLink can create this symbol in System` and interfere with the symbol's definition.", "Remark", data]];

  issues
]

*)










Attributes[scanSelfAssignments] = {HoldRest}

scanSelfAssignments[pos_List, astIn_] :=
Catch[
 Module[{ast, node, var, data, parentPos, parent},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  var = node[[2]][[1]];
  data = node[[3]];

  (*
  It is a common idiom to do With[{a = a}, foo], so do not warn about that

  And there are enough occurrences of Block and Module, so add those too
  *)
  If[Length[pos] >= 4,
    parentPos = Drop[pos, -4];
    parent = Extract[ast, {parentPos}][[1]];
    If[MatchQ[parent, CallNode[SymbolNode[Symbol, "Block" | "DynamicModule" | "Module" | "With", _], _, _]],

      (* and make sure to only skip  With[{a = a}, foo]  and still report   With[{}, a=a] *)
      If[pos[[-3]] == 1,
        Throw[{}]
      ]
    ]
  ];

  {Lint["SelfAssignment", "Self assignment: ``" <> ToFullFormString[var] <> "``.", "Warning", data]}
]]





Attributes[scanLoadJavaClassSystem] = {HoldRest}

scanLoadJavaClassSystem[pos_List, astIn_] :=
Catch[
 Module[{ast, node, var, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  var = node[[2]][[1]];
  data = node[[3]];

  {Lint["LoadJavaClassSystem", "``LoadJavaClass[\"java.lang.System\"]`` redefines symbols in **System`** context.\n\
This can interfere with system functionality.\n\
Did you mean ``LoadJavaCLass[\"java.lang.System\", AllowShortContext->False]``?", "Warning", data]}
]]



Attributes[scanPrivateContextNode] = {HoldRest}

scanPrivateContextNode[pos_List, astIn_] :=
Catch[
 Module[{ast, node, str, strData},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];

  str = node[[1]][[1]];
  strData = str[[3]];

  {Lint["SuspiciousPrivateContext", "Suspicious context: ``\"Private`\"``.\n\
Did you mean ``\"`Private`\"``?", "Error", strData]}
]]



(*

too noisy

experimental

Attributes[scanPatternTestMissingPattern] = {HoldRest}

scanPatternTestMissingPattern[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  data = node[[3]];

  {Lint["PatternTestMissingPattern", "``PatternTest`` is missing a pattern on the LHS.", "Error", data]}
]]
*)




(*

too noisy

experimental

Attributes[scanRHSPatterns] = {HoldRest}

scanRHSPatterns[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data, children, lhs, rhs, lhsPatNames, rhsPatSyms, badSyms, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  data = node[[3]];

  children = node[[2]];
  lhs = children[[1]];
  rhs = children[[2]];

  lhsPatNames = Cases[lhs, CallNode[SymbolNode[Symbol, "Pattern", _], {SymbolNode[Symbol, name_, _], _}, _] :> name, {0, Infinity}];
  rhsPatSyms = Cases[rhs, CallNode[SymbolNode[Symbol, "Pattern", _], {sym:SymbolNode[Symbol, _, _], _}, _] :> sym, {0, Infinity}];

  badSyms = Select[rhsPatSyms, MemberQ[lhsPatNames, #["Name"]]&];

  issues = {};

  Scan[(
    AppendTo[issues, Lint["RHSPattern", "Pattern ``" <> #["Name"] <> "`` appears on the RHS.", "Error", #[[3]]]]
  )&, badSyms];

  issues
]]

*)








Attributes[scanAbstractSyntaxErrorNodes] = {HoldRest}

scanAbstractSyntaxErrorNodes[pos_List, astIn_] :=
 Module[{ast, node, token, data, tokString},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  token = node[[1]];
  data = node[[3]];

  tokString = Block[{$ContextPath = {"AST`", "System`"}, $Context = "Lint`Scratch`"}, ToString[token]];

  {Lint["AbstractSyntaxError", "Abstract syntax error with token: ``" <> tokString <> "``.", "Fatal", data]}
]






Attributes[scanAbstractSyntaxIssues] = {HoldRest}

(*
Just directly convert AbstractSyntaxIssues to Lints
*)
scanAbstractSyntaxIssues[pos_List, astIn_] :=
Module[{ast, data, issues},
  ast = astIn;
  data = Extract[ast, {pos}][[1]];
  issues = data[AbstractSyntaxIssues];

  Lint @@@ issues
]



(*

too noisy

scanAlts[pos_, actual_] :=
 
 Module[{node, parentPos, parent, span, opts},
  node = Extract[actual, pos];
  opts = node[[-1]];
  parentPos = Most[pos];
  parent = Extract[actual, parentPos];
  Switch[parent,
   _,
   span = opts[Source];
   {Lint["Weird Alternatives", "Warning", <|Source -> span|>]}
   ]
  ]
*)




(*

too noisy

scanStrings[StringNode[s_, {}, opts_]] :=
Module[{span, origLen},
  span = opts[Source];
  origLen = span[[2, 2]] - span[[1, 2]] + 1;
  If[StringLength[s] != origLen,
    Lint["Unrecognized character", "Error", <|Source -> span|>]
    ,
    {}
  ]
]
*)



(*

too noisy

scanSetDelayeds[
  BinaryNode[SetDelayed, {left_, right_}, opts_?AssociationQ]] :=
 Module[{warnings, opLocation, duplicates, selected, name1, 
   name2, span1, span2},

   warnings = {};


(*
too noisy


  name1 = DeclarationName[left];
  If[name1 === $Failed,
   AppendTo[warnings, 
    Lint["Internal failure", "Fatal", <|Source -> opts[Source]|>]]
   ];
  If[MatchQ[right, BinaryNode[Set, _, _]],
   name2 = DeclarationName[right[[2, 1]]];
   If[name2 === $Failed,
    AppendTo[warnings, 
     Lint["Internal failure", 
      "Fatal", <|Source -> opts[Source]|>]]
    ];
   If[name1 =!= name2,
    span1 = left[[-1]][Source];
    span2 = right[[2, 1, -1]][Source];
    AppendTo[warnings, 
     Lint["Memoization of different symbols", 
      "Warning", <|Source -> {span1[[1]], span2[[2]]}|>]]
    ];
   ];
   XPrint["scanSetDelayeds returning: ", warnings];

   *)
   warnings
  ]

scanSetDelayeds[args___] := (
  Message[scanSetDelayeds::unhandled, {args}];
  $Failed
)
*)



End[]


EndPackage[]