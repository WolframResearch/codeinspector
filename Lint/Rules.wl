BeginPackage["Lint`Rules`"]

$DefaultRules

Begin["`Private`"]

Needs["AST`"]
Needs["Lint`"]
Needs["Lint`Format`"]

(*

Rules are of the form: pat -> func where pat is the node pattern to match on and func is the processing function for the node.

Functions are of the form: function[pos_, ast_] where pos is the position of the node in the AST, and ast is the AST itself.
  And function must return a list of Lints. 


A rule of thumb is to make patterns as specific as possible, to offload work of calling the function.

*)

$DefaultRules = <|

(*
Tags: SyntaxError
*)
GroupNode[GroupSquare, _, _] -> scanSquares,

(*
Tags: SyntaxError
*)
GroupNode[GroupParen, {}, _] -> scanParens,
GroupNode[GroupParen, {_, _, ___}, _] -> scanParens,

(*
Tags: SyntaxError
*)
BinaryNode[PatternTest, {BinaryNode[PatternTest, _, _], _}, _] -> scanPatternTests,

(*
Tags: SyntaxError
*)
PrefixNode[LinearSyntaxBang, _, _] -> scanLinearSyntaxBangs,

(*
Tags: SyntaxError
*)
SyntaxErrorNode[_, _, _] -> scanSyntaxErrorNodes,

(*
Tags: SyntaxError NotContiguous MaxExpressionDepth etc.
*)
KeyValuePattern[SyntaxIssues -> _] -> scanSyntaxIssues,






(*
Tags: Control
*)
SymbolNode["Return" | "Break" | "Continue", _, _] -> scanControls,

(*
Tags: SuspiciousSpan
*)
BinaryNode[Span, {_, _}, _] -> scanSpans,

(*
Tags: ImplicitTimesAcrossLines
*)
InfixNode[InfixImplicitTimes, _, KeyValuePattern[Source -> {{line1_, _}, {line2_, _}} /; line1 != line2]] -> scanImplicitTimes,

(*
Tags: SuspiciousOut
*)
OutNode[_, _, _] -> scanOuts,

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
GroupNode[Association, _, _] -> scanAssocs,

(*
Tags: DotDifferentLine
*)
InfixNode[Dot, _, _] -> scanDots,


Nothing
|>



Attributes[scanSquares] = {HoldRest}

scanSquares[pos_List, astIn_] := 
 Module[{ast, node, data, opLocation},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  data = node[[3]];
  opLocation = data[Source];
  {Lint["SyntaxError", "Naked Square expression", "Fatal", <|Source -> opLocation|>]}
  ]



Attributes[scanParens] = {HoldRest}

scanParens[pos_List, astIn_] :=
 Module[{ast, node, children, data, opLocation},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  opLocation = data[Source];
  Which[
   Length[children] == 0,
   {Lint["SyntaxError", "Empty Paren expression", "Fatal", <|Source -> opLocation|>]}
   ,
   Length[children] > 1,
   {Lint["SyntaxError", "Incorrect Paren expression", "Fatal", <|Source -> opLocation|>]}
   ]
  ]



Attributes[scanPatternTests] = {HoldRest}

scanPatternTests[pos_List, astIn_] :=
Module[{ast, node, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  data = node[[3]];
  {Lint["SyntaxError", "Nested ? is illegal", "Fatal", data]}
]



Attributes[scanLinearSyntaxBangs] = {HoldRest}

scanLinearSyntaxBangs[pos_List, astIn_] :=
Module[{ast, node, children, span},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  If[!MatchQ[children, {GroupNode[GroupLinearSyntaxParen, _, _]}],
    span = data[Source];
    {Lint["SyntaxError", "\\! must be followed by \\(", "Fatal", <|Source -> span|>]}
    ,
    {}
  ]
]



Attributes[scanSyntaxErrorNodes] = {HoldRest}

scanSyntaxErrorNodes[pos_List, astIn_] :=
 Module[{ast, node, token, data, span},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  token = node[[1]];
  data = node[[3]];
  span = data[Source];
  {Lint["SyntaxError", ToString[token], "Fatal", <|Source -> span|>]}
  ]




Attributes[scanSyntaxIssues] = {HoldRest}

(*
Just directly convert SyntaxIssues to Lints
*)
scanSyntaxIssues[pos_List, astIn_] :=
Module[{ast, data, issues},
  ast = astIn;
  data = Extract[ast, {pos}][[1]];
  issues = data[SyntaxIssues];

  Lint @@@ issues
]












Attributes[scanAssocs] = {HoldRest}

scanAssocs[pos_List, astIn_] :=
 Module[{ast, node, children, data, opLocation, duplicates, selected},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  opLocation = data[Source];
  Which[
   !MatchQ[children, {BinaryNode[Rule | RuleDelayed, {_, _}, _] ...}],
   (*{Warning["Malformed Association","Warning",<|Source->
   opLocation|>]}*)
   {}
   ,
   duplicates = 
    Keys[Select[
      CountsBy[children[[All, 2, 1]], ToInputFormString], # > 1&]];
   duplicates =!= {},
   selected = 
    Flatten[Select[children[[All, 2, 1]], 
        Function[{key}, ToInputFormString[key] === #]]& /@ 
      duplicates, 1];
   {Lint["DuplicateKeys", "Duplicate keys in Association: " <> ToInputFormString[#], "Error", <|Source -> (#[[3]][Source])|>]}& /@ selected
   ,
   True,
   {}
   ]
  ]




Attributes[scanDots] = {HoldRest}

scanDots[pos_List, astIn_] :=
 Module[{ast, node, children, data, warnings, line, nextLine},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

   warnings = {};

   line = children[[1]][[3]][Source][[2,1]];
   Do[
    nextLine = n[[3]][Source][[1,1]];
    If[line != nextLine,
      AppendTo[warnings,
        Lint["DotDifferentLine", {"Operands for ", LintMarkup[".", FontWeight->Bold], " are on different lines. Did you mean ", LintMarkup[";", FontWeight->Bold], " or ", LintMarkup[",", FontWeight->Bold], "?"}, "Warning", data]];
      Break[];
    ];
    line = n[[3]][Source][[2,1]];
    ,
    {n, children[[2;;]]}
  ];

   warnings
  ]



Attributes[scanImplicitTimes] = {HoldRest}

scanImplicitTimes[pos_List, astIn_] :=
 Module[{ast, node, children, nodeSpans, pars},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  nodeSpans = #[[3]][Source]& /@ children;
  pars = Partition[nodeSpans, 2, 1];
  Map[If[#[[1, 2, 1]] =!= #[[2, 1, 1]], {
    Lint["ImplicitTimesAcrossLines", {"Implicit times across lines. Did you mean ", LintMarkup[";", FontWeight->Bold], " or ", LintMarkup[",", FontWeight->Bold], "?"}, "Warning", <|Source -> {#[[1, 1]], #[[2, 2]]}|>]}, {}]&, pars]
  ]



Attributes[scanOuts] = {HoldRest}

scanOuts[pos_List, astIn_] :=
 Module[{ast, node, data, opSpan, s},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  s = node[[1]];
  data = node[[3]];
  opSpan = data[Source];
  {Lint["SuspiciousOut", "Suspicious use of " <> s <> " in file.", "Warning", <|Source -> opSpan|>]}
]



Attributes[scanWhichs] = {HoldRest}

scanWhichs[pos_List, astIn_] :=
 Module[{ast, node, children, data, opSpan, warnings = {}, span},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  opSpan = data[Source];
  Which[
   Length[children] == 0,
   AppendTo[warnings, 
     Lint["WhichArguments", {LintMarkup["Which", FontWeight->Bold], " does not have any arguments. This may be ok if ", LintMarkup["Which", FontWeight->Bold], " has pattern arguments"}, "Warning", <|Source -> opSpan|>]];
   ,
   !EvenQ[Length[children]],
   AppendTo[warnings, 
     Lint["WhichArguments", {LintMarkup["Which", FontWeight->Bold], " does not have even number of arguments. This may be ok if ", LintMarkup["Which", FontWeight->Bold], " has pattern arguments"}, "Warning", <|Source -> opSpan|>]];
   ,
   MatchQ[children[[1]], SymbolNode["$OperatingSystem", _, _]],
   span = children[[1]][[3]];
   AppendTo[warnings, 
    Lint["SwitchWhichConfusion", {LintMarkup["Which", FontWeight->Bold], " has ", LintMarkup["$OperatingSystem", FontWeight->Bold] , " in first place. Did you mean ", LintMarkup[".", FontWeight->Bold], "?"}, "Warning", span]];
   ,
   MatchQ[children[[-2]], BlankNode[_, _, _]],
   span = children[[-2]][[3]];
   AppendTo[warnings, 
    Lint["SwitchWhichConfusion", {LintMarkup["Which", FontWeight->Bold], " has ", LintMarkup["_", FontWeight->Bold], " in last place. Did you mean ", LintMarkup["True", FontWeight->Bold], "?"}, "Error", span]];
   ];
  warnings
  ]




Attributes[scanSwitchs] = {HoldRest}

scanSwitchs[pos_List, astIn_] :=
 Module[{ast, node, children, data, opSpan, warnings = {}, span, cases},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  opSpan = data[Source];
  Which[
   Length[children] == 1,
   AppendTo[warnings, 
     Lint["SwitchArguments", {LintMarkup["Switch", FontWeight->Bold], " only has one argument. This may be ok if ", LintMarkup["Switch", FontWeight->Bold], " has pattern arguments"}, "Warning", <|Source -> opSpan|>]];
   ,
   !OddQ[Length[children]],
   AppendTo[warnings, 
     Lint["SwitchArguments", {LintMarkup["Switch", FontWeight->Bold], " does not have odd number of arguments. This may be ok if ", LintMarkup["Switch", FontWeight->Bold], " has pattern arguments"}, "Warning", <|Source -> opSpan|>]];
   ,
   MatchQ[children[[1]], SymbolNode["$OperatingSystem", _, _]],
   cases = Cases[children[[2;;-1;;2]], StringNode["\"Linux\"", _, _], {0, Infinity}];
   If[cases =!= {},
   span = cases[[1]][[3]];
   AppendTo[warnings, 
    Lint["OperatingSystemLinux", {LintMarkup["\"Linux\"", FontWeight->Bold], " is not a value of ", LintMarkup["$OperatingSystem", FontWeight->Bold], ". Did you mean ", LintMarkup["\"Unix\"", FontWeight->Bold], "?"}, "Warning", span]];
   ]
   ,
   MatchQ[children[[-2]], SymbolNode["True", _, _]],
   span = children[[-2]][[3]];
   AppendTo[warnings, 
    Lint["SwitchWhichConfusion", {LintMarkup["Switch", FontWeight->Bold], " has ", LintMarkup["True", FontWeight->Bold], " in last place. Did you mean ", LintMarkup["_", FontWeight->Bold], "?"}, "Warning", span]];
   ];
  warnings
  ]



Attributes[scanControls] = {HoldRest}

scanControls[pos_List, astIn_] :=
 Module[{ast, node, data, parentPos, parent, span, opts, s},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  parentPos = Most[pos];
  parent = Extract[ast, parentPos];
  If[ListQ[parent],
   parentPos = Most[parentPos];
   parent = Extract[ast, parentPos];
   ];
  Switch[parent,
   CallNode[node, _, _],
   {}
   ,
   BinaryNode[BinaryAt, {node, _}, _],
   {}
   ,
   BinaryNode[BinarySlashSlash, {_, node}, _],
   {}
   ,
   _,
   s = node[[1]];
    data = node[[3]];
   span = data[Source];
   {Lint["Control", {LintMarkup[s, FontWeight->Bold], " appears but is not called. Did you mean ", LintMarkup[s<>"[]", FontWeight->Bold], "? This may be ok if ", LintMarkup[s, FontWeight->Bold], " is used as a symbol."}, "Warning", <|Source -> span|>]}
   ]
  ]



Attributes[scanSpans] = {HoldRest}

scanSpans[pos_List, astIn_] :=
 Module[{ast, node, data, parentPos, parent, span, opts},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  data = node[[3]];
  parentPos = Most[pos];
  parent = Extract[ast, parentPos];
  If[ListQ[parent],
   parentPos = Most[parentPos];
   parent = Extract[ast, parentPos];
   ];
  Switch[parent,
   PartNode[_, _, _],
   {}
   ,
   GroupNode[_, _, _],
   {}
   ,
   CallNode[_, _, _],
   {}
   ,
   _,
   If[!MatchQ[node[[2, 1]], _InternalEmptyNode] &&
        (MatchQ[node[[2, 2]], _InternalEmptyNode] || (node[[2, 1, 3]][Source][[2, 1]] =!= node[[2, 2, 3]][Source][[1, 1]]))
    ,
    span = data[Source];
    {Lint["SuspiciousSpan", {"Suspicious use of Span. Consider using ", LintMarkup[";", FontWeight->Bold]}, "Warning", <|Source -> span|>]}
    ,
    {}
    ]
   ]
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