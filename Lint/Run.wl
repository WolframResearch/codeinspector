BeginPackage["Lint`Run`"]




Begin["`Private`"]

Needs["AST`"]
Needs["Lint`"]


LintFile[file_String] :=
Module[{ast},

  If[FileType[file] =!= File,
   Return[Failure["NotAFile", <|"FileName"->file|>]]
   ];

  If[FileByteCount[file] > 1*^6,
   Return[Failure["FileTooLarge", <|"FileName"->file, "FileSize"->FileSize[file]|>]]
   ];
   
  ast = ParseFile[file];
  lintAST[ast]
]


LintString[input_String] :=
Module[{ast},
  ast = ParseString[input];
  lintAST[ast]
]




lintAST[f_?FailureQ] :=
  f

lintAST[ast_] :=
Module[{warnings, pos},

  If[$Debug,
    Print["ast: ", ast]
  ];

  warnings = {};

  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];

	warnings = Flatten @ Join[warnings, Cases[ast, n:InfixNode[BinarySpaceTimes, _, _] :> scanSpaceTimes[n], {0, Infinity}, Heads->True] ];

  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];

  warnings = Flatten @ Join[warnings, Cases[ast, n:GroupNode[GroupSquare, _, _] :> scanSquares[n], {0, Infinity}, Heads->True] ];

  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];

  warnings = Flatten @ Join[warnings, Cases[ast, n:GroupNode[GroupParen, _, _] :> scanParens[n], {0, Infinity}, Heads->True] ];

  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];

  warnings = Flatten @ Join[warnings, Cases[ast, n:OutNode[_, _, _] :> scanOuts[n], {0, Infinity}, Heads->True] ];
  
  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];

  warnings = Flatten @ Join[warnings, Cases[ast, n:CallNode[SymbolNode["Which", _, _], _, _] :> scanWhichs[n], {0, Infinity}, Heads->True] ];

  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];

  warnings = Flatten @ Join[warnings, Cases[ast, n:CallNode[SymbolNode["Switch", _, _], _, _] :> scanSwitchs[n], {0, Infinity}, Heads->True] ];

  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];

  pos = Position[ast, SymbolNode["Return" | "Break" | "Continue", _, _]];
  warnings = Flatten @ Join[warnings, Map[scanControls[#, ast]&, pos]];

  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];

  pos = Position[ast, BinaryNode[Span, {_, _}, _]];
  warnings = Flatten @ Join[warnings, Map[scanSpans[#, ast] &, pos]];

  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];
  
  warnings = Flatten @ Join[warnings, Cases[ast, n:GroupNode[Association, _, _] :> scanAssocs[n], {0, Infinity}, Heads->True] ];

  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];

  warnings = Flatten @ Join[warnings, Cases[ast, n:BinaryNode[SetDelayed, _, _] :> scanSetDelayeds[n], {0, Infinity}, Heads->True] ];

  warnings = Flatten @ Join[warnings, Cases[ast, n:InfixNode[Dot, _, _] :> scanDots[n], {0, Infinity}, Heads->True] ];

  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];

  warnings = Flatten @ Join[warnings, Cases[ast, n:BinaryNode[PatternTest, _, _] :> scanPatternTests[n], {0, Infinity}, Heads->True] ];

  warnings = Flatten @ Join[warnings, Cases[ast, n:PrefixNode[LinearSyntaxBang, _, _] :> scanLinearSyntaxBangs[n], {0, Infinity}, Heads->True] ];

  warnings = Flatten @ Join[warnings, Cases[ast, n:SyntaxErrorNode[_, _, _] :> scanSyntaxErrorNodes[n], {0, Infinity}, Heads->True] ];

  warnings = Flatten @ Join[warnings, Cases[ast, KeyValuePattern[SyntaxIssues -> issues_] :> scanSyntaxIssues[issues], {0, Infinity}, Heads->True] ];

  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];

  (*
  warnings = Flatten @ Join[warnings, Cases[ast, n:StringNode[_, _, _] :> scanStrings[n], {0, Infinity}, Heads->True] ];

  If[$Debug,
    Print["warnings: ", Length[warnings]]
  ];
  *)
  
  Flatten[warnings]
]


(*
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


scanSquares[GroupNode[GroupSquare, nodes_, opts_?AssociationQ]] :=
 
 Module[{opLocation},
  opLocation = opts[Source];
  {Lint["Naked Square expression", "Fatal", <|Source -> opLocation|>]}
  ]





scanParens[GroupNode[GroupParen, nodes_, opts_?AssociationQ]] :=
 Module[{opLocation},
  opLocation = opts[Source];
  Which[
   Length[nodes] === 0,
   {Lint["Empty Paren expression", "Fatal", <|Source -> opLocation|>]}
   ,
   Length[nodes] > 1,
   {Lint["Incorrect Paren expression", 
     "Fatal", <|Source -> opLocation|>]}
   ,
   True,
   {}
   ]
  ]

scanAssocs[GroupNode[Association, nodes_, opts_?AssociationQ]] :=
 Module[{opLocation, duplicates, selected},
  opLocation = opts[Source];
  Which[
   ! MatchQ[nodes, {BinaryNode[Rule | RuleDelayed, {_, _}, _] ...}],
   (*{Warning["Malformed Association","Warning",<|Source->
   opLocation|>]}*)
   {}
   ,
   duplicates = 
    Keys[Select[
      CountsBy[nodes[[All, 2, 1]], ToInputFormString], # > 1 &]];
   duplicates =!= {},
   selected = 
    Flatten[Select[nodes[[All, 2, 1]], 
        Function[{key}, ToInputFormString[key] === #]] & /@ 
      duplicates, 1];
   {Lint["Duplicate keys in Association: " <> ToInputFormString[#],
        "Error", <|Source -> (#[[-1]][Source])|>]} & /@ selected
   ,
   True,
   {}
   ]
  ]

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


scanDots[
  InfixNode[Dot, nodes_, opts_?AssociationQ]] :=
 Module[{warnings, line, nextLine},

   warnings = {};

   line = nodes[[1]][[3]][Source][[2,1]];
   Do[
    nextLine = n[[3]][Source][[1,1]];
    If[line != nextLine,
      AppendTo[warnings, Lint["Operand for " <> ToString[Style[".", Bold], StandardForm] <> " is on different line\nDid you mean " <> ToString[Style[";", Bold], StandardForm] <>
          " or " <> ToString[Style[",", Bold], StandardForm] <> "?", "Warning", opts]];
      Break[];
    ];
    line = n[[3]][Source][[2,1]];
    ,
    {n, nodes[[2;;]]}
  ];

   warnings
  ]






scanSpaceTimes[
  InfixNode[BinarySpaceTimes, nodes_, opts_?AssociationQ]] :=
 Module[{nodeSpans, pars},
  nodeSpans = #[[-1]][Source] & /@ nodes;
  pars = Partition[nodeSpans, 2, 1];
  Map[If[#[[1, 2, 1]] =!= #[[2, 1, 1]], {Lint[
       "Implicit multiplication across lines\nDid you mean " <> ToString[Style[";", Bold], StandardForm] <>
          " or " <> ToString[Style[",", Bold], StandardForm] <> "?", 
       "Warning", <|
        Source -> {#[[1, 2]] + {0, 1}, #[[2, 1]] + {0, -1}}|>]}, {}] &, pars]
  ]

scanOuts[OutNode[s_, _, opts_?AssociationQ]] :=
 Module[{opSpan},
  opSpan = opts[Source];
  {Lint["Using " <> s <> " in file.", 
    "Warning", <|Source -> opSpan|>]}
  ]

scanWhichs[CallNode[SymbolNode["Which", _, _], nodes_, opts_]] :=
 Module[{opSpan, warnings = {}, span},
  opSpan = opts[Source];
  Which[
   Length[nodes] == 0,
   AppendTo[warnings, 
     Lint["Which does not have any arguments\nThis may be ok if Which has pattern arguments", 
      "Warning", <|Source -> opSpan|>]];
   ,
   !EvenQ[Length[nodes]],
   AppendTo[warnings, 
     Lint["Which does not have even number of arguments\nThis may be ok if Which has pattern arguments", 
      "Warning", <|Source -> opSpan|>]];
   ,
   MatchQ[nodes[[1]], SymbolNode["$OperatingSystem", _, _]],
   span = nodes[[1]][[3]];
   AppendTo[warnings, 
    Lint["Which has $OperatingSystem in first place\nDid you mean " <> ToString[Style["Switch", Bold], StandardForm] <> "?", "Warning", span]];
   ,
   MatchQ[nodes[[-2]], BlankNode[_, _, _]],
   span = nodes[[-2]][[3]];
   XXXPrint[span];
   AppendTo[warnings, 
    Lint["Which has a Blank in last place\nDid you mean " <> ToString[Style["True", Bold], StandardForm] <> "?", 
     "Error", span]];
   ];
  warnings
  ]

scanWhichs[args___] := (
  Message[scanWhichs::unhandled, {args}];
  $Failed
)


scanSwitchs[CallNode[SymbolNode["Switch", _, _], nodes_, opts_]] :=
 Module[{opSpan, warnings = {}, span, cases},
  opSpan = opts[Source];
  Which[
   Length[nodes] == 1,
   AppendTo[warnings, 
     Lint["Switch only has one argument\nThis may be ok if Switch has pattern arguments", 
      "Warning", <|Source -> opSpan|>]];
   ,
   !OddQ[Length[nodes]],
   AppendTo[warnings, 
     Lint["Switch does not have odd number of arguments\nThis may be ok if Switch has pattern arguments", 
      "Warning", <|Source -> opSpan|>]];
   ,
   MatchQ[nodes[[1]], SymbolNode["$OperatingSystem", _, _]],
   cases = Cases[nodes[[2;;-1;;2]], StringNode["\"Linux\"", _, _], {0, Infinity}];
   If[cases =!= {},
   span = cases[[1]][[3]];
   AppendTo[warnings, 
    Lint["\"Linux\" is not a value of $OperatingSystem\nDid you mean " <> ToString[Style["\"Unix\"", Bold], StandardForm] <> "?", "Warning", span]];
   ]
   ,
   MatchQ[nodes[[-2]], SymbolNode["True", _, _]],
   span = nodes[[-2]][[3]];
   AppendTo[warnings, 
    Lint["Switch has True in last place\nDid you mean " <> ToString[Style["_", Bold], StandardForm] <> "?", "Warning", span]];
   ];
  warnings
  ]

scanControls[pos_, actual_] :=
 Module[{node, parentPos, parent, span, opts},
  node = Extract[actual, pos];
  opts = node[[3]];
  parentPos = Most[pos];
  parent = Extract[actual, parentPos];
  If[ListQ[parent],
   parentPos = Most[parentPos];
   parent = Extract[actual, parentPos];
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
   span = opts[Source];
   {Lint[node[[1]] <> " appears but is not called\nDid you mean " <> ToString[Style[node[[1]]<>"[]", Bold], StandardForm] <> "?\nThis may be ok if " <> node[[1]] <> " is used as a symbol", "Warning", <|Source -> span|>]}
   ]
  ]

scanSpans[pos_, actual_] :=
 Module[{node, parentPos, parent, span, opts},
  node = Extract[actual, pos];
  opts = node[[-1]];
  parentPos = Most[pos];
  parent = Extract[actual, parentPos];
  If[ListQ[parent],
   parentPos = Most[parentPos];
   parent = Extract[actual, parentPos];
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
   If[! MatchQ[
       node[[2, 1]], _InternalEmptyNode] && (MatchQ[
        node[[2, 
         2]], _InternalEmptyNode] || (node[[2, 1, -1]][Source][[2, 
          1]] =!= node[[2, 2, -1]][Source][[1, 1]]))
       ,
    span = opts[Source];
    {Lint["Weird Span", "Warning", <|Source -> span|>]}
    ,
    {}
    ]
   ]
  ]

(*
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

scanPatternTests[BinaryNode[PatternTest, nodes_, opts_]] :=
Module[{span},
  If[MatchQ[nodes, {BinaryNode[PatternTest, _, _], _}],
    span = opts[Source];
    {Lint["Nested ? is illegal", "Fatal", <|Source -> span|>]}
    ,
    {}
  ]
]

scanLinearSyntaxBangs[PrefixNode[LinearSyntaxBang, nodes_, opts_]] :=
Module[{span},
  If[!MatchQ[nodes, {GroupNode[GroupLinearSyntaxParen, _, _]}],
    span = opts[Source];
    {Lint["\\! must be followed by \\(", "Fatal", <|Source -> span|>]}
    ,
    {}
  ]
]

scanSyntaxErrorNodes[SyntaxErrorNode[msg_, nodes_, opts_]] :=
 Module[{span},
  span = opts[Source];
  {Lint[msg, "Fatal", <|Source -> span|>]}
  ]


scanSyntaxIssues[issues_List] :=
Module[{},
  Lint @@ #& /@ issues
]




End[]


EndPackage[]





