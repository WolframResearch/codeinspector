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
InfixNode[CompoundExpression, _, _] -> scanCompoundExpressions,

(*
Tags: SuspiciousOut
*)
OutNode[_, _, _] -> scanOuts,

(*

a_?b[x]
probably meant to have a_?(b[x])
*)
CallNode[BinaryNode[PatternTest, _, _], {_}, _] -> scanPatternTestCalls,


(*
a->b&
probably meant to have a->(b&)
*)
PostfixNode[Function, {BinaryNode[Rule|RuleDelayed, _, _]}, _] -> scanRuleFunctions,

(*
a?b&
probably meant to have a?(b&)
*)
PostfixNode[Function, {BinaryNode[PatternTest, _, _]}, _] -> scanPatternTestFunctions,

(*

a_?b[x]& is handled here
*)
PostfixNode[Function, {CallNode[BinaryNode[PatternTest, _, _], {_}, _]}, _] -> scanPatternTestCallFunctions,

BinaryNode[Pattern, {_, InfixNode[Alternatives, _, _]}, _] -> scanAlternativesPatterns,

BinaryNode[Optional, {PatternBlankNode[PatternBlank, {_}, _], _}, _] -> scanPatternBlankOptionals,
BinaryNode[Optional, {PatternBlankSequenceNode[PatternBlankSequence, {_}, _], _}, _] -> scanPatternBlankOptionals,
BinaryNode[Optional, {PatternBlankNullSequenceNode[PatternBlankNullSequence, {_}, _], _}, _] -> scanPatternBlankOptionals,




BinaryNode[Optional, {BlankNode[Blank, _, _], _}, _] -> scanBlankOptionals,
BinaryNode[Optional, {BlankSequenceNode[BlankSequence, _, _], _}, _] -> scanBlankOptionals,
BinaryNode[Optional, {BlankNullSequenceNode[BlankNullSequence, _, _], _}, _] -> scanBlankOptionals,




(*
Tags: SyntaxError
*)
SyntaxErrorNode[_, _, _] -> scanSyntaxErrorNodes,
CallMissingCloserNode[_, _, _] -> scanMissingCloserNodes,

(*
Tags: SyntaxError MaxExpressionDepth etc.
*)
KeyValuePattern[SyntaxIssues -> _] -> scanSyntaxIssues,



Nothing
|>





$DefaultAbstractRules = <|

CallNode[SymbolNode["String", _, _], _, _] -> scanStringCalls,
CallNode[SymbolNode["Integer", _, _], _, _] -> scanIntegerCalls,
CallNode[SymbolNode["Real", _, _], _, _] -> scanRealCalls,
CallNode[SymbolNode["Failure", _, _], _, _] -> scanFailureCalls,

(*
Tags: Control
*)
SymbolNode["Return" | "Break" | "Continue", _, _] -> scanControls,


CallNode[SymbolNode["Pattern", _, _], _, _] -> scanPatterns,

(*
Tags: WhichArguments SwitchWhichConfusion
*)
CallNode[SymbolNode["Which", _, _], _, _] -> scanWhichs,

(*
Tags: SwitchArguments SwitchWhichConfusion OperatingSystemLinux
*)
CallNode[SymbolNode["Switch", _, _], _, _] -> scanSwitchs,

(*
Tags: DuplicateKeys
*)
CallNode[SymbolNode["Association", _, _], {CallNode[SymbolNode["Rule" | "RuleDelayed", _, _], _, _] ...}, _] -> scanAssocs,

(*
Tags: 
*)
CallNode[SymbolNode["Module", _, _], _, _] -> scanModules,

(*
Tags: 
*)
CallNode[SymbolNode["With", _, _], _, _] -> scanWiths,

(*
Tags: 
*)
CallNode[SymbolNode["Block", _, _], _, _] -> scanBlocks,

(*
Tags: 
*)
(*
1-arg Optional[] is ok to have named patterns
Only scan 2-arg Optionals
*)
CallNode[SymbolNode["Optional", _, _], {_, _}, _] -> scanOptionals,


(*
Scan all symbols that are intuitive, yet do not exist
*)
SymbolNode["AnyFalse" | "Failed"(*|"Boolean"*), _, _] -> scanBadSymbols,



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
        Lint["ImplicitTimesAcrossLines", {"Implicit times across lines. Did you mean ", LintBold[";"], " or ",
                                            LintBold[","], "?"}, "Warning", data]];
      Break[];
    ];
    line = n[[3]][Source][[2,1]];
    ,
    {n, children[[2;;]]}
  ];

  warnings
]


Attributes[scanDots] = {HoldRest}

scanDots[pos_List, cstIn_] :=
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
        Lint["DotDifferentLine", {"Operands for ", LintBold["."], " are on different lines. Did you mean ",
                                    LintBold[";"], " or ", LintBold[","], "?"}, "Warning", data]];
      Break[];
    ];
    line = n[[3]][Source][[2,1]];
    ,
    {n, children[[2;;]]}
  ];

  warnings
]


Attributes[scanSpans] = {HoldRest}

scanSpans[pos_List, cstIn_] :=
Catch[
 Module[{cst, node, children, data, parentPos, parent, issues, line, nextLine},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  If[pos == {},
    (* top node, no parent *)
    Throw[{Lint["SuspiciousSpan", {"Suspicious use of ", LintBold[";;"], ". Did you mean ", LintBold[";"], "?"}, "Warning", data]}]
  ];

  issues = {};

  parentPos = Most[pos];
  parent = Extract[cst, {parentPos}][[1]];
  While[ListQ[parent] || MatchQ[parent, GroupNode[GroupParen, _, _]],
   parentPos = Most[parentPos];
   parent = Extract[cst, {parentPos}][[1]];
   ];

   If[MatchQ[parent, _PartNode | CallNode[SymbolNode["Part"|"Internal`BagPart", {}, _], _, _]],
    (* any Span inside a Part is ok *)
    Throw[issues]
   ];

   If[MatchQ[parent, _CallNode | _GroupNode],
    (* just ignore anything inside f[] or {} for now *)
    Throw[issues]
   ];

   Switch[node,
      BinaryNode[Span, {_, InternalAllNode[All, {}, _]}, _],
        AppendTo[issues, Lint["SuspiciousSpan", {"Suspicious use of ", LintBold[";;"], ". Did you mean ", LintBold[";"], "?"}, "Warning", data]];
      ,
      _,
        line = children[[1]][[3]][Source][[2,1]];
        Do[
          nextLine = n[[3]][Source][[1,1]];
          If[line != nextLine,
            AppendTo[issues, Lint["SuspiciousSpan", {"Suspicious use of ", LintBold[";;"], ". Did you mean ", LintBold[";"], "?"}, "Warning", data]];
          ];
          line = n[[3]][Source][[2,1]];
          ,
          {n, children[[2;;]]}
        ];
    ];

  issues
]]



(*

StraySemicolon is to find things like this:

(f[];
; Throw[$Failed, $tag])

*)

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

  Scan[(AppendTo[issues, Lint["StraySemicolon", {LintBold[";"], " may not be needed."}, "Remark", #[[3]]]])&, internalNulls];

  issues
]

Attributes[scanOuts] = {HoldRest}

scanOuts[pos_List, cstIn_] :=
 Module[{cst, node, data, s},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  s = node[[1]];
  data = node[[3]];
  {Lint["SuspiciousOut", {"Suspicious use of ", LintBold[s], " in file."}, "Warning", data]}
]


Attributes[scanPatternTestCalls] = {HoldRest}

(*
warn about a_?b[x] which actually parses as (a_?b)[x] and not a_?(b[x])
*)
scanPatternTestCalls[pos_List, cstIn_] :=
Catch[
 Module[{cst, node, data, children, patternTest, args, patternTestArg1, patternTestArg2},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  patternTest = node[[1]];
  children = node[[2]];

  patternTestArg1 = patternTest[[2]][[1]];
  patternTestArg2 = patternTest[[2]][[2]];
  args = children[[1]];
  data = node[[3]];

  (*

  this is noisy, so make Remark for now

  *)

  {Lint["SuggestParentheses", {"Suspicious use of ", LintBold["?"], ". The precedence of ", LintBold["?"], " is surprisingly high. PatternTest ", LintBold[ToInputFormString[patternTest]],
    " is calling arguments ", LintBold[ToInputFormString[args]],
    ". Did you mean ", LintBold[ToInputFormString[BinaryNode[PatternTest,
        {patternTestArg1, GroupNode[GroupParen, {CallNode[patternTestArg2, {args}, <||>]}, <||>]}, <||>]]],
        " or ", LintBold[ToInputFormString[CallNode[GroupNode[GroupParen, {patternTest}, <||>], children, <||>]]], " ?"}, "Remark", data]}
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
  ruleHead = rule[[1]];
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
     If[MatchQ[parent, BinaryNode[BinaryAt | Map | Composition | RightComposition | Apply, {node, _}, _]],
      Throw[{}]
     ];

     If[$Debug,
      Print[parent]
     ];

     (*
     heuristic
      if inside Map[], then assume it is intentional, i.e., Map[a->b&, x] is intentional
     *)
     If[MatchQ[parent, CallNode[SymbolNode["Map" | "MapAt", _, _], {GroupNode[GroupSquare, {node, _, _}, _]}, _]],
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

  ruleChild2 = rule[[2]][[2]];

  {Lint["SuspiciousRuleFunction", {"Suspicious use of ", LintBold["&"], ". The precedence of ", LintBold["&"], " is surprisingly low. ", SymbolName[ruleHead], " ", LintBold[ToInputFormString[rule]],
    " is inside a ", LintBold["Function"], ". Did you mean ", LintBold[ToInputFormString[BinaryNode[ruleHead, {ruleChild1,
      GroupNode[GroupParen, {PostfixNode[Function, {ruleChild2}, <||>]}, <||>]}, <||>]]], " or ",
      LintBold[ToInputFormString[PostfixNode[Function, {GroupNode[GroupParen, {rule}, <||>]}, <||>]]], " ?"}, "Warning", data]}
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
  patternTestArg2 = patternTestChildren[[2]];
  {Lint["SuspiciousPatternTestFunction", {"Suspicious use of ", LintBold["&"], ". The precedence of ", LintBold["&"], " is surprisingly low and the precedence of ", LintBold["?"], " is surprisingly high. ", LintBold["?"], " is inside a ", LintBold["Function"], ". Did you mean ",
          LintBold[ToInputFormString[BinaryNode[PatternTest, {patternTestArg1, GroupNode[GroupParen, {PostfixNode[Function, {patternTestArg2}, <||>]}, <||>]}, <||>]]],
          " or ", LintBold[ToInputFormString[PostfixNode[Function, {GroupNode[GroupParen, {patternTest}, <||>]}, <||>]]], " ?"}, "Warning", data]}
]]



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

  {Lint["SuspiciousPatternTestCallFunction", {"Suspicious use of ", LintBold["&"], ". The precedence of ", LintBold["&"],
    " is surprisingly low and the precedence of ", LintBold["?"], " is surprisingly high. Call to PatternTest ",
    LintBold[ToInputFormString[call]], " is inside a ", LintBold["Function"], ". Did you mean ",
    LintBold[ToInputFormString[BinaryNode[PatternTest, {patternTestArg1, GroupNode[GroupParen, {PostfixNode[Function, {CallNode[patternTestArg2, callChildren, <||>]}, <||>]}, <||>]}, <||>]]],
          " or ", LintBold[ToInputFormString[PostfixNode[Function, {CallNode[GroupNode[GroupParen, {patternTest}, <||>], callChildren, <||>]}, <||>]]], " ?"},
    "Warning", data]}
]]




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
  opt = children[[2]];

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
                BinaryNode[Pattern, _, _]
                ],
    Throw[{}]
  ];

  {Lint["SuspiciousPatternBlankOptional", {"Suspicious use of ", LintBold[":"], ". Did you mean ",
          LintBold[ToInputFormString[BinaryNode[Pattern, {pattern, opt}, <||>]]], " ? This may be ok if ", LintBold[ToInputFormString[pattern]], " is used as a pattern."}, "Warning", data]}
]]



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






Attributes[scanSyntaxErrorNodes] = {HoldRest}

scanSyntaxErrorNodes[pos_List, cstIn_] :=
 Module[{cst, node, token, data},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  token = node[[1]];
  data = node[[3]];
  {Lint["SyntaxError", ToString[token], "Fatal", data]}
]



Attributes[scanMissingCloserNodes] = {HoldRest}

scanMissingCloserNodes[pos_List, cstIn_] :=
 Module[{cst, node, token, data},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  token = node[[1]];
  data = node[[3]];
  {Lint["SyntaxError", "Missing closer", "Fatal", data]}
]



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
  {Lint["StringCall", {"Calling ", LintBold["String"], " as a function. Did you mean ", LintBold["StringQ"],
    " ? This may be ok if ", LintBold["String"], " is handled programmatically."}, "Error", data]}
  ]

Attributes[scanIntegerCalls] = {HoldRest}

scanIntegerCalls[pos_List, astIn_] :=
 Module[{ast, node, children, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  {Lint["IntegerCall", {"Calling ", LintBold["Integer"], " as a function. Did you mean ", LintBold["IntegerQ"],
      " ? This may be ok if ", LintBold["Integer"], " is handled programmatically."}, "Error", data]}
  ]

Attributes[scanRealCalls] = {HoldRest}

scanRealCalls[pos_List, astIn_] :=
 Module[{ast, node, children, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  {Lint["RealCall", {"Calling ", LintBold["Real"], " as a function. This may be ok if ", LintBold["Real"], " is handled programmatically."}, "Error", data]}
  ]

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
  ];
  ]






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

   {Lint["DuplicateKeys", {"Duplicate keys in ", LintBold["Association"]}, "Error", #[[3]]]}& /@ selected

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
     Lint["WhichArguments", {LintBold["Which"], " does not have any arguments. This may be ok if ",
                              LintBold["Which"], " has pattern arguments"}, "Warning", data]];
    Throw[warnings]
  ];

  If[!EvenQ[Length[children]],
    AppendTo[warnings, 
     Lint["WhichArguments", {LintBold["Which"], " does not have even number of arguments. This may be ok if ",
                              LintBold["Which"], " has pattern arguments"}, "Warning", data]];
    Throw[warnings]
  ];


  If[MatchQ[children[[1]], SymbolNode["$OperatingSystem", _, _]],
    span = children[[1]][[3]];
   AppendTo[warnings, 
    Lint["SwitchWhichConfusion", {LintBold["Which"], " has ", LintBold["$OperatingSystem"] , " in first place. Did you mean ",
                                  LintBold["."], "?"}, "Warning", span]];
  ];

  If[MatchQ[children[[-2]], CallNode[SymbolNode["Blank", _, _], _, _]],
    span = children[[-2]][[3]];
   AppendTo[warnings, 
    Lint["SwitchWhichConfusion", {LintBold["Which"], " has ", LintBold["_"], " in last place. Did you mean ",
                                    LintBold["True"], "?"}, "Error", span]];
  ];

    duplicates = Keys[Select[CountsBy[children[[;;;;2]], ToFullFormString], # > 1&]];
   selected = Flatten[Select[children[[;;;;2]], Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
   Scan[
    AppendTo[warnings,
      Lint["DuplicateClauses", {"Duplicate clauses in ", LintBold["Which"]}, "Error", #[[3]]]
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
   AppendTo[issues, Lint["SwitchArguments", {LintBold["Switch"], " only has one argument. This may be ok if ",
                              LintBold["Switch"], " has pattern arguments"}, "Warning", data]];
   Throw[issues];
  ];


  If[!OddQ[Length[children]],
   AppendTo[issues, Lint["SwitchArguments", {LintBold["Switch"], " does not have odd number of arguments. This may be ok if ",
                              LintBold["Switch"], " has pattern arguments"}, "Warning", data]];
   Throw[issues];
  ];

  If[MatchQ[children[[1]], SymbolNode["$OperatingSystem", _, _]],
   cases = Cases[children[[2;;-1;;2]], StringNode["\"Linux\"", _, _], {0, Infinity}];
   If[cases =!= {},
    span = cases[[1]][[3]];
    AppendTo[issues, Lint["OperatingSystemLinux", {LintBold["\"Linux\""], " is not a value of ",
                                    LintBold["$OperatingSystem"], ". Did you mean ", LintBold["\"Unix\""], "?"}, "Warning", span]];
   ]
  ];

  (*
   Switch has True in last place like this: Switch[a,1,b,True,c]
   *)
  If[MatchQ[children[[-2]], SymbolNode["True", _, _]],
   (* presence of False makes it less likely that True is unintended *)
   If[FreeQ[children[[2;;-4;;2]], SymbolNode["False", _, _]],
    span = children[[-2]][[3]];
    AppendTo[issues, Lint["SwitchWhichConfusion", {LintBold["Switch"], " has ", LintBold["True"], " in last place. Did you mean ",
                                    LintBold["_"], "?"}, "Warning", span]];
   ]
  ];

  duplicates = Keys[Select[CountsBy[children[[2;;;;2]], ToFullFormString], # > 1&]];
  selected = Flatten[Select[children[[2;;;;2]], Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
  Scan[(AppendTo[issues, Lint["DuplicateClauses", {"Duplicate clauses in ", LintBold["Switch"]}, "Error", #[[3]]]])&, selected];


  pairs = Partition[children[[2;;]], 2];

  Scan[(
    form = #[[1]];
    value = #[[2]];
    formPatternNames = Cases[form, CallNode[SymbolNode["Pattern", _, _], {SymbolNode[n_, _, _], _}, _] :> n, {0, Infinity}];

    Scan[(
      
      valuePatterns = Cases[value, SymbolNode[#, _, _], {0, Infinity}];
      If[empty[valuePatterns],
        (*
        too noisy
        add a Remark about unused named pattern in Switch? *)
        Null
        ,
        Scan[(AppendTo[issues, Lint["NamedPattern", {"Named pattern in ", LintBold["Switch"], ": ", LintBold[ToFullFormString[#]],
          ". The pattern ", LintBold[ToFullFormString[#]], " occurs in the matching form, but ", LintBold["Switch"],
          " does not support pattern replacement. Consider removing the named pattern ", LintBold[ToFullFormString[#]]}, "Warning", #[[3]]]])&, valuePatterns]
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
  name = patSymbol[[1]];
  rhs = children[[2]];

  issues = {};

  patterns = Cases[rhs, CallNode[SymbolNode["Pattern", _, _], _, _], {0, Infinity}];
  Scan[(
    If[#[[2]][[1]][[1]] == name,
      (*
      This is never correct code, but make a Warning for now because it is noisy
      *)
      AppendTo[issues, Lint["DuplicateNamedPattern", {"Duplicate named pattern ", LintBold[name], " in RHS of ", LintBold["Pattern"]}, "Warning", #[[3]]]];
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
  s = node[[1]];
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

  {Lint["Control", {LintBold[s], " appears but is not called. Did you mean ",
                      LintBold[s<>"[]"], "? This may be ok if ", LintBold[s], " is used as a symbol."}, "Warning", data]}
  ]]

Attributes[scanModules] = {HoldRest}

scanModules[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, params, warnings, vars},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  warnings = {};

  If[Length[children] != 2,
    AppendTo[warnings, Lint["ModuleArguments", {LintBold["Module"], " does not have 2 arguments. This may be ok if ",
                              LintBold["Module"], " is handled programmatically."}, "Error", data]];
    Throw[warnings]
  ];

  If[!MatchQ[children[[1]], CallNode[SymbolNode["List", _, _], _, _]],
    AppendTo[warnings, Lint["ModuleArguments", {LintBold["Module"], " does not have a List for argument 1. This may be ok if ",
                              LintBold["Module"], " is handled programmatically."}, "Error", data]];
    Throw[warnings]
  ];


  params = children[[1,2]];
   vars = # /. {CallNode[SymbolNode["Set"|"SetDelayed", _, _], {sym:SymbolNode[_, _, _], _}, _] :> sym,
            sym:SymbolNode[_, _, _] :> sym,
            err_ :> (AppendTo[warnings, Lint["ModuleArguments", {"Variable ", ToFullFormString[err],
                        " does not have proper form. This may be ok if ", LintBold["Module"], " is handled programmatically."}, "Error", #[[3]]]]; Nothing)}& /@ params;
    duplicates = Keys[Select[CountsBy[vars, ToFullFormString], # > 1&]];
    selected = Flatten[Select[vars, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
    Scan[AppendTo[warnings, Lint["DuplicateVariables", {"Duplicate variables in ", LintBold["Module"], ": ", ToFullFormString[#]}, "Error", #[[3]]]]&, selected];

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
 Module[{ast, node, children, data, duplicates, selected, paramLists, warnings, vars},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  warnings = {};

  If[Length[children] < 2,
    AppendTo[warnings, Lint["WithArguments", {LintBold["With"], " does not have 2 or more arguments. This may be ok if ",
                              LintBold["With"], " is handled programmatically."}, "Error", data]];
    Throw[warnings];
  ];

  If[!MatchQ[Most[children], {CallNode[SymbolNode["List", _, _], _, _]...}],
    AppendTo[warnings, Lint["WithArguments", {LintBold["With"], " does not have a List for most arguments. This may be ok if ",
                              LintBold["With"], " is handled programmatically."}, "Error", data]];
    Throw[warnings];
  ];

  paramLists = Most[children][[All, 2]];
   vars = Function[{list}, # /. {CallNode[SymbolNode["Set"|"SetDelayed", _, _], {sym:SymbolNode[_, _, _], _}, _] :> sym,
            err_ :> (AppendTo[warnings, Lint["WithArguments", {"Variable ", ToFullFormString[err], " does not have proper form. This may be ok if ",
                              LintBold["With"], " is handled programmatically."}, "Error", #[[3]]]]; Nothing)}& /@ list] /@ paramLists;
    duplicates = Keys[Select[CountsBy[#, ToFullFormString], # > 1 &]]& /@ vars;
      selected = Flatten[Function[{duplicates, vars}, (Select[vars, Function[{c}, ToFullFormString[c] === #]])& /@ duplicates] @@@ Transpose[{duplicates, vars}]];
  Scan[AppendTo[warnings, Lint["DuplicateVariables", {"Duplicate variables in ", LintBold["With"], ": ", ToFullFormString[#]}, "Error", #[[3]]]]&, selected];

  warnings
]]



Attributes[scanBlocks] = {HoldRest}

scanBlocks[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, params, warnings, vars},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  warnings = {};

  If[Length[children] != 2,
    AppendTo[warnings, Lint["BlockArguments", {LintBold["Block"], " does not have 2 arguments. This may be ok if ",
                              LintBold["Block"], " is handled programmatically."}, "Error", data]];
    Throw[warnings]
  ];

  If[!MatchQ[children[[1]], CallNode[SymbolNode["List", _, _], _, _]],
    AppendTo[warnings, Lint["BlockArguments", {LintBold["Block"], " does not have a List for argument 1. This may be ok if ",
                              LintBold["Block"], " is handled programmatically."}, "Error", data]];
    Throw[warnings]
  ];

  params = children[[1,2]];
   vars = # /. {CallNode[SymbolNode["Set"|"SetDelayed", _, _], {sym:SymbolNode[_, _, _], _}, _] :> sym,
            sym:SymbolNode[_, _, _] :> sym,
            err_ :> (AppendTo[warnings, Lint["BlockArguments", {"Variable ", ToFullFormString[err], " does not have proper form. This may be ok if ",
                              LintBold["Block"], " is handled programmatically."}, "Error", #[[3]]]]; Nothing)}& /@ params;
    duplicates = Keys[Select[CountsBy[vars, ToFullFormString], # > 1&]];
    selected = Flatten[Select[vars, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
    Scan[AppendTo[warnings, Lint["DuplicateVariables", {"Duplicate variables in ", LintBold["Block"], ": ", ToFullFormString[#]}, "Error", #[[3]]]]&, selected];

  warnings
]]


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
  pats = Cases[opt, CallNode[SymbolNode["Pattern", _, _], _, _], {0, Infinity}];
  Scan[(
    AppendTo[issues, Lint["NamedPattern", {"Named pattern ", LintBold[ToFullFormString[#[[2]][[1]]]], " in ", LintBold["Optional"]}, "Error", #[[3]]]]
  )&, pats];

  issues
]


Attributes[scanBadSymbols] = {HoldRest}

scanBadSymbols[pos_List, astIn_] :=
 Module[{ast, node, token, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  token = node[[1]];
  data = node[[3]];
  {Lint["BadSymbol", ToString[token], "Error", data]}
]




Attributes[scanAbstractSyntaxIssues] = {HoldRest}

(*
Just directly convert SyntaxIssues to Lints
*)
scanAbstractSyntaxIssues[pos_List, cstIn_] :=
Module[{cst, data, issues},
  cst = cstIn;
  data = Extract[cst, {pos}][[1]];
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