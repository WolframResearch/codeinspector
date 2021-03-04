BeginPackage["CodeInspector`AbstractRules`"]

$DefaultAbstractRules


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

$DefaultAbstractRules = <|


CallNode[LeafNode[Symbol, "String" | "Integer" | "Real" | "True", _], _, _] -> scanBadCalls,

(*

not a good scan

CallNode[SymbolNode["Failure", _, _], _, _] -> scanFailureCalls,
*)

(*
Tags: Control
*)
LeafNode[Symbol, "Return" | "Break" | "Continue", _] -> scanControls,


CallNode[LeafNode[Symbol, "Pattern", _], _, _] -> scanPatterns,

(*
Tags: WhichArguments SwitchWhichConfusion
*)
CallNode[LeafNode[Symbol, "Which", _], _, _] -> scanWhichs,

(*
Tags: SwitchArguments SwitchWhichConfusion OperatingSystemLinux
*)
CallNode[LeafNode[Symbol, "Switch", _], _, _] -> scanSwitchs,

(*
Tags: IfArguments
*)
CallNode[LeafNode[Symbol, "If", _], _, _] -> scanIfs,

(*
Tags: DuplicateKeys
*)
CallNode[LeafNode[Symbol, "Association", _], _, _] -> scanAssocs,

CallNode[LeafNode[Symbol, "List", _], { CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], _, _]... }, _] -> scanListsOfRules,

(*
Tags: 
*)
CallNode[LeafNode[Symbol, "Module", _], _, _] -> scanModules,

(*
Tags: 
*)
CallNode[LeafNode[Symbol, "DynamicModule", _], _, _] -> scanDynamicModules,

(*
Tags: 
*)
CallNode[LeafNode[Symbol, "With", _], _, _] -> scanWiths,

(*
Tags: 
*)
CallNode[LeafNode[Symbol, "Block" | "Internal`InheritedBlock", _], _, _] -> scanBlocks,

(*
Tags: 
*)
(*
1-arg Optional[] is ok to have named patterns
Only scan 2-arg Optionals
*)
CallNode[LeafNode[Symbol, "Optional", _], {_, _}, _] -> scanOptionals,

(*
Tags: 
*)
(*

experimental

must handle Condition

CallNode[LeafNode[Symbol, "Replace" | "ReplaceAll" | "ReplaceRepeated", _], _, _] -> scanReplaces,
*)

(*
Scan some symbols that are intuitive, yet do not exist
*)
LeafNode[Symbol,
  "AnyFalse" | "AllFalse" | "Failed" | "Boolean" | "RealQ" | "FalseQ" | "RationalQ" |
  "ComplexQ" | "SymbolQ" | "Match" | "UnSameQ" | "StringMatch" |
  "OptionsQ" | "OptionPattern", _] -> scanBadSymbols,

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

CallNode[LeafNode[Symbol, "LoadJavaClass" | "JLink`LoadJavaClass", _], {
  LeafNode[String, "\"java.lang.System\"", _] }, _] -> scanLoadJavaClassSystem,





(*
scan for a := a  and  a = a
possible results from batch renaming symbols
*)
CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {
  LeafNode[Symbol, token_, _], LeafNode[Symbol, token_, _] }, _] -> scanSelfAssignments,


ContextNode[{LeafNode[String, "\"Private`\"", _]}, _, _] -> scanPrivateContextNode,



LeafNode[Symbol, "$HistoryLength" | "$Line", _] -> scanSessionSymbols,

CallNode[LeafNode[Symbol, "In" | "Out" | "InString", _], _, _] -> scanSessionCalls,


(*

too noisy

CallNode[LeafNode[Symbol, "Print" | "Echo", _], _, _] -> scanDebugCalls,
*)




(*

experimental

FileNode[_, _, _] -> scanFiles,
*)


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



CallNode[LeafNode[Symbol, "And", _], _, _] -> scanAnds,

CallNode[LeafNode[Symbol, "Or", _], _, _] -> scanOrs,

CallNode[LeafNode[Symbol, "Alternatives", _], _, _] -> scanAlternatives,



CallNode[LeafNode[Symbol, "Refine" | "Reduce" | "Solve" | "FindInstance" | "Assuming", _], _, _] -> scanSolverCalls,



CallNode[LeafNode[Symbol, "Rule", _], {lhs_ /; !FreeQ[lhs, CallNode[LeafNode[Symbol, "Pattern", _], _, _]], _}, _] -> scanPatternRules,


CallNode[LeafNode[Symbol, "Rule", _], {LeafNode[Symbol, "ImageSize", _], rhs_ /; !FreeQ[rhs, CallNode[LeafNode[Symbol, "ImageDimensions", _], _, _]]}, _] -> scanImageSizes,



CallNode[LeafNode[Symbol, "OptionsPattern", _], {}, _] -> scanOptionsPattern,


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








Attributes[scanBadCalls] = {HoldRest}

scanBadCalls[pos_List, astIn_] :=
Module[{ast, node, data, head, name, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  head = node[[1]];
  name = head["String"];
  data = head[[3]];

  issues = {};

    Switch[name,
    "String",
      AppendTo[issues,
        InspectionObject["BadCall", "``String`` is not a function.", "Error",
          <|
            Source -> data[Source],
            CodeActions -> {
              CodeAction["Replace ``String`` with ``StringQ``", ReplaceNode, <|
                "ReplacementNode" -> ToNode[StringQ], Source -> data[Source] |>]
            },
            ConfidenceLevel -> 0.90,
            "Argument" -> name
          |>
        ]
      ]
    ,
    "Integer",
      AppendTo[issues,
        InspectionObject["BadCall", "``Integer`` is not a function.", "Error",
          <|
            Source -> data[Source],
            CodeActions -> {
              CodeAction["Replace ``Integer`` with ``IntegerQ``", ReplaceNode, <|
                "ReplacementNode" -> ToNode[IntegerQ], Source -> data[Source] |>]
            },
            ConfidenceLevel -> 0.90,
            "Argument" -> name
          |>
        ]
      ]
    ,
    "Real",
      AppendTo[issues,
        InspectionObject["BadCall", "``Real`` is not a function.", "Error",
          <|
            Source -> data[Source],
            CodeActions -> {
              CodeAction["Replace ``Real`` with ``Developer`RealQ``", ReplaceNode, <|
                "ReplacementNode" -> ToNode[Developer`RealQ], Source -> data[Source] |>]
            },
            ConfidenceLevel -> 0.90,
            "Argument" -> name
          |>
        ]
      ]
    ,
    "True",
      AppendTo[issues,
        InspectionObject["BadCall", "``True`` is not a function.", "Error",
          <|
            Source -> data[Source],
            CodeActions -> {
              CodeAction["Replace ``True`` with ``TrueQ``", ReplaceNode, <|
                "ReplacementNode" -> ToNode[TrueQ], Source -> data[Source] |>]
            },
            ConfidenceLevel -> 0.95,
            "Argument" -> name
          |>
        ]
      ]
    ,
    _,
      AppendTo[issues,
        InspectionObject["BadCall", format[name] <> " is not a function.", "Error",
          <|
            data,
            ConfidenceLevel -> 0.90,
            "Argument" -> name
          |>
        ]
      ]
  ];

  issues
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
Catch[
Module[{ast, node, children, data, issues, actions, counts, selecteds, srcs, dupKeys, expensiveChildren, filtered},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[!MatchQ[children, { CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], _, _]... }],
    (*
    Association does not have all Rule arguments.
    *)
    Throw[{}]
  ];

  (*
  Skip  <| _ -> a, _ -> b |>
  *)
  filtered = DeleteCases[children, CallNode[_, { CallNode[LeafNode[Symbol, "Blank", _], _, _], _ }, _]];

  expensiveChildren = ToFullFormString[#[[2, 1]]]& /@ filtered;

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[expensiveChildren, FailureQ],
    Throw[issues]
  ];

  counts = CountsBy[filtered, ToFullFormString[#[[2, 1]]]&];

  dupKeys = Keys[Select[counts, # > 1&]];

  selecteds = Function[key, Pick[filtered, (# == key)& /@ expensiveChildren]] /@ dupKeys;

  Do[

    If[empty[selected],
      Continue[]
    ];

    srcs = #[[2, 1, 3, Key[Source]]]& /@ selected;

    actions = MapIndexed[CodeAction["Delete key " <> ToString[#2[[1]]], DeleteNode, <|Source->#|>]&, srcs];

    AppendTo[issues, InspectionObject["DuplicateKeys", "``Association`` has duplicated keys.", "Error", <|
      Source -> First[srcs],
      "AdditionalSources" -> Rest[srcs],
      CodeActions -> actions, ConfidenceLevel -> 1.0 |> ]
    ];
    ,
    {selected, selecteds}
  ];

  issues
]]





Attributes[scanListsOfRules] = {HoldRest}

scanListsOfRules[pos_List, astIn_] :=
Catch[
Module[{ast, node, children, data, selecteds, issues, srcs, counts, keys, dupKeys, actions, expensiveChildren, parentPos, parent,
  confidence},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};
  confidence = 0.95;

  parentPos = pos;
  If[parentPos != {},
    parentPos = Most[parentPos];
    parent = Extract[ast, {parentPos}][[1]];
    If[ListQ[parent],
      parentPos = Most[parentPos];
      parent = Extract[ast, {parentPos}][[1]];
    ];

    If[MatchQ[parent, CallNode[LeafNode[Symbol, "Graph" | "NetGraph" | "WordCloud", _], _, _]],
      (*
      Graph[{1->2, 1->3}] is ok for list of rules, so give low confidence of copy/paste error
      *)
      confidence = 0.1;
    ]
  ];

  keys = children[[All, 2, 1]];

  (*
  heuristic

  if something like {_ -> {}, _ -> {}} then do not warn, it is just a pattern
  *)
  If[AnyTrue[keys, MatchQ[#, CallNode[LeafNode[Symbol, "Blank", _], _, _]]&],
    Throw[issues]
  ];

  expensiveChildren = ToFullFormString[#[[2, 1]]]& /@ children;

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[expensiveChildren, FailureQ],
    Throw[issues]
  ];

  counts = CountsBy[children, ToFullFormString[#[[2, 1]]]&];

  dupKeys = Keys[Select[counts, # > 1&]];

  (*
  selecteds is { {Rule[], Rule[]}, {Rule[], Rule[]} }
  *)
  selecteds = Function[key, Pick[children, (# == key)& /@ expensiveChildren]] /@ dupKeys;

  Do[

    If[empty[selected],
      Continue[]
    ];

    srcs = #[[2, 1, 3, Key[Source]]]& /@ selected;

    actions = MapIndexed[CodeAction["Delete key " <> ToString[#2[[1]]], DeleteNode, <|Source->#|>]&, srcs];

    AppendTo[issues, InspectionObject["DuplicateKeys", "Duplicate keys in list of rules.", "Warning", <|
      Source -> First[srcs],
      "AdditionalSources" -> Rest[srcs],
      CodeActions -> actions, ConfidenceLevel -> confidence |> ]
    ];
    ,
    {selected, selecteds}
  ];

  issues
]]




Attributes[scanWhichs] = {HoldRest}

scanWhichs[pos_List, astIn_] :=
Catch[
Module[{ast, node, head, children, data, issues, srcs, counts, selecteds,
  first,
  dupKeys, expensiveChildren, firsts,
  choice1, choice2},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  head = node[[1]];
  children = node[[2]];
  data = node[[3]];
  
  issues = {};

  If[empty[children],
    AppendTo[issues, 
     InspectionObject["WhichArguments", "``Which`` does not have any arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  If[!EvenQ[Length[children]],
    AppendTo[issues, 
      InspectionObject["WhichArguments", "``Which`` does not have even number of arguments.", "Error", <|
        data,
        ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  first = children[[1]];

  If[MatchQ[first, LeafNode[Symbol, "$OperatingSystem", _]],
    AppendTo[issues,
      InspectionObject["SwitchWhichConfusion", "``Which`` has ``$OperatingSystem`` in first place.", "Error", <|
        Source -> head[[3, Key[Source]]],
        "AdditionalSources" -> {first[[3, Key[Source]]]},
        CodeActions -> {
          CodeAction["Replace ``Which`` with ``Switch``", ReplaceNode, <|
            "ReplacementNode" -> ToNode[Switch], Source -> head[[3, Key[Source]]] |>]}, ConfidenceLevel -> 0.75|>
      ]
    ]
  ];

  If[MatchQ[children[[-2]], CallNode[LeafNode[Symbol, "Blank", _], _, _]],
    AppendTo[issues, 
      InspectionObject["SwitchWhichConfusion", "``_`` is not a test.", "Error", <|
        Source -> children[[-2, 3, Key[Source]]],
        CodeActions -> {
          CodeAction["Replace ``_`` with ``True``", ReplaceNode, <|
            "ReplacementNode" -> ToNode[True], Source -> children[[-2, 3, Key[Source]]] |>]}, ConfidenceLevel -> 1.0 |>
      ]
    ]
  ];

  Scan[
    Function[{child},
      If[MatchQ[child, CallNode[LeafNode[Symbol, "Set", _], {_, _}, _]],

        (*
        TODO: need to convert from abstract Set[a, b] to concrete a == b
        also need to convert from abstract Set[a, b] to concrete (a = b)
        *)
        (* choice1 = BinaryNode[Equal, {
          child[[2, 1]],
          LeafNode[Token`Whitespace, " ", <||>],
          LeafNode[Token`EqualEqual, "==", <||>],
          LeafNode[Token`Whitespace, " ", <||>],
          child[[2, 2]]}, <||>]; *)

        (* choice2 = GroupNode[GroupParen, {
          LeafNode[Token`OpenParen, "(", <||>],
          child,
          LeafNode[Token`CloseParen, ")", <||>]
          }, <||>]; *)

        AppendTo[issues,
          InspectionObject["WhichSet", "``Which`` has ``=`` as a clause.", "Error", <|
            Source -> child[[3, Key[Source]]],
            ConfidenceLevel -> 0.85,
            CodeActions -> {
              (* CodeAction["Replace = with ==", ReplaceNode, <|
                "ReplacementNode" -> choice1,
                Source -> child[[3, Key[Source]]]
              |>], *)
              (* CodeAction["Wrap with ()", ReplaceNode, <|
                "ReplacementNode" -> choice2,
                Source -> child[[3, Key[Source]]]
              |>] *)
            }
          |>]
        ];
      ];
    ]
    ,
    children[[;;;;2]]
  ];

  expensiveChildren = ToFullFormString /@ children[[;;;;2]];

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[expensiveChildren, FailureQ],
    Throw[issues]
  ];

  counts = CountsBy[children[[;;;;2]], ToFullFormString];

  dupKeys = Keys[Select[counts, # > 1&]];

  selecteds = Function[key, Pick[children[[;;;;2]], (# == key)& /@ expensiveChildren]] /@ dupKeys;

  Do[

    If[empty[selected],
      Continue[]
    ];

    firsts = firstTokenWithSource /@ selected;

    srcs = #[[3, Key[Source]]]& /@ firsts;

    AppendTo[issues, InspectionObject["DuplicateClauses", "Duplicate clauses in ``Which``.", "Error", <|
      Source -> First[srcs],
      "AdditionalSources" -> Rest[srcs],
      ConfidenceLevel -> 0.95,
      "Argument" -> "Which" |> ]
    ];
    ,
    {selected, selecteds}
  ];

  issues
]]


Attributes[scanSwitchs] = {HoldRest}

scanSwitchs[pos_List, astIn_] :=
Catch[
Module[{ast, node, children, data, src, cases, issues, selecteds, srcs, counts,
  dupKeys, expensiveChildren, firsts},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  
  issues = {};

  If[Length[children] == 1,
    AppendTo[issues, InspectionObject["SwitchArguments", "``Switch`` only has one argument.", "Error", <|
      data,
      ConfidenceLevel -> 0.55|>]];
    Throw[issues];
  ];


  If[!OddQ[Length[children]],
    AppendTo[issues, InspectionObject["SwitchArguments", "``Switch`` does not have odd number of arguments.", "Error", <|
      data,
      ConfidenceLevel -> 0.55|>]];
    Throw[issues];
  ];

  If[MatchQ[children[[1]], LeafNode[Symbol, "$OperatingSystem", _]],
    cases = Cases[children[[2;;-1;;2]], LeafNode[String, "\"Linux\"", _], {0, Infinity}];
    If[cases =!= {},
      src = cases[[1, 3, Key[Source]]];
      AppendTo[issues, InspectionObject["OperatingSystemLinux", "``\"Linux\"`` is not a value of ``$OperatingSystem``.", "Error", <|
        Source -> src,
        ConfidenceLevel -> 0.95, CodeActions -> {
          CodeAction["Replace Linux with Unix", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode["Unix"]|>]}|>]];
    ]
  ];

  (*
  Something like:
  Switch[a,
  1->2,
  3->4,
  _,5]
  *)
  If[Length[children] >= 5,
    If[MatchQ[children[[2;;-3]], { CallNode[LeafNode[Symbol, "Rule", _], _, _].. }],
      AppendTo[issues, InspectionObject["SwitchArguments", "``Switch`` does not take ``Rules`` for arguments.", "Error", <|
        data,
        ConfidenceLevel -> 0.95|>]];
    ]
  ];

  (*
   Switch has True in last place like this: Switch[a,1,b,True,c]
   *)
  If[MatchQ[children[[-2]], LeafNode[Symbol, "True", _]],
   (*
    
    heuristic 
  
   presence of False makes it less likely that True is unintended
   *)
    If[FreeQ[children[[2;;-4;;2]], LeafNode[Symbol, "False", _]],
      AppendTo[issues,
        InspectionObject["SwitchWhichConfusion", "``Switch`` has ``True`` in last place.", "Warning", <|
          Source -> children[[-2, 3, Key[Source]]],
          ConfidenceLevel -> 0.75,
          CodeActions -> {
            CodeAction["Replace True with _", ReplaceNode, <|
              Source -> children[[-2, 3, Key[Source]]],
              "ReplacementNode" -> LeafNode[Token`Under, "_", <||>] |>] } |>
        ]
      ]
    ]
  ];

  expensiveChildren = ToFullFormString /@ children[[2;;;;2]];

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[expensiveChildren, FailureQ],
    Throw[issues]
  ];

  counts = CountsBy[children[[2;;;;2]], ToFullFormString];

  dupKeys = Keys[Select[counts, # > 1&]];

  selecteds = Function[key, Pick[children[[2;;;;2]], (# == key)& /@ expensiveChildren]] /@ dupKeys;

  Do[

    If[empty[selected],
      Continue[]
    ];

    firsts = firstTokenWithSource /@ selected;

    srcs = #[[3, Key[Source]]]& /@ firsts;

    AppendTo[issues, InspectionObject["DuplicateClauses", "Duplicate clauses in ``Switch``.", "Error", <|
      Source -> First[srcs],
      "AdditionalSources" -> Rest[srcs],
      ConfidenceLevel -> 0.95,
      "Argument" -> "Switch" |> ]
    ];
    ,
    {selected, selecteds}
  ];


  (*
  pairs = Partition[children[[2;;]], 2];

  Scan[(
    form = #[[1]];
    value = #[[2]];
    formPatternNames = Cases[form, CallNode[LeafNode[Symbol, "Pattern", _], {LeafNode[Symbol, n_, _], _}, _] :> n, {0, Infinity}];

    Scan[(
      
      valuePatterns = Cases[value, LeafNode[Symbol, #, _], {0, Infinity}];
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

    )&, pairs];*)

  issues
]]



Attributes[scanIfs] = {HoldRest}

scanIfs[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, issues, selected, srcs, counts, firsts,
  child, choice1, choice2},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  
  issues = {};

  Which[
    Length[children] == 0,
      AppendTo[issues, InspectionObject["IfArguments", "``If`` has zero arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
      Throw[issues];
      ,
    Length[children] == 1,
      AppendTo[issues, InspectionObject["IfArguments", "``If`` only has one argument.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
      Throw[issues];
  ];

  child = children[[1]];

  If[MatchQ[child, CallNode[LeafNode[Symbol, "Set", _], {_, _}, _]],

    (*
    TODO: need to convert from abstract Set[a, b] to concrete a == b
    also need to convert from abstract Set[a, b] to concrete (a = b)
    *)
    (* choice1 = BinaryNode[Equal, {
      child[[2, 1]],
      LeafNode[Token`Whitespace, " ", <||>],
      LeafNode[Token`EqualEqual, "==", <||>],
      LeafNode[Token`Whitespace, " ", <||>],
      child[[2, 2]]}, <||>]; *)

    (* choice2 = GroupNode[GroupParen, {
      LeafNode[Token`OpenParen, "(", <||>],
      child,
      LeafNode[Token`CloseParen, ")", <||>]
      }, <||>]; *)

    AppendTo[issues,
      InspectionObject["IfSet", "``If`` has ``=`` as first argument.", "Warning", <|
        Source -> child[[3, Key[Source]]],
        ConfidenceLevel -> 0.85,
        CodeActions -> {
          (* CodeAction["Replace = with ==", ReplaceNode, <|
            "ReplacementNode" -> choice1,
            Source -> child[[3, Key[Source]]]
          |>], *)
          (* CodeAction["Wrap with ()", ReplaceNode, <|
            "ReplacementNode" -> choice2,
            Source -> child[[3, Key[Source]]]
          |>] *)
        }
      |>]
    ];
  ];

  srcs = {};
  If[Length[children] >= 3,

    counts = CountsBy[children[[2;;3]], ToFullFormString];

    (*
    bail out if there are errors and ToFullFormString has failed
    *)
    If[AnyTrue[Keys[counts], FailureQ],
      Throw[issues]
    ];

    (*
    Do not warn about  If[a, _, _]
    *)
    counts = KeyDrop[counts, "Blank[]"];

    selected = Select[children[[2;;3]], counts[ToFullFormString[#]] > 1&];

    If[!empty[selected],
      firsts = firstTokenWithSource /@ selected;
      srcs = #[[3, Key[Source]]]& /@ firsts;
      AppendTo[issues, InspectionObject["DuplicateClauses", "Both branches of ``If`` are the same.", "Error", <|
        Source -> First[srcs],
        "AdditionalSources" -> Rest[srcs],
        ConfidenceLevel -> 0.95,
        "Argument" -> "If"|>]]
    ];
  ];

  issues
]]



(*

experimental

must handle Condition


Attributes[scanReplaces] = {HoldRest}

scanReplaces[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, issues, selected, rules, lhss},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  
  If[Length[children] == 2,
    rules = children[[2]];
    ,
    (*
    operator form
    *)
    rules = children[[1]];
  ];

  If[!MatchQ[rules, CallNode[LeafNode[Symbol, "List", _], _, _]],
    Throw[{}];
  ];

  Scan[(If[!MatchQ[#, CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], { _, _ }, _]],
    Throw[{}];
    ];)&, rules[[2]]];

  issues = {};

  (*
  get the lhss of all of the rules
  *)
  lhss = rules[[2, All, 2, 1]];

  duplicates = Keys[Select[CountsBy[lhss, ToFullFormString], # > 1&]];
  selected = Flatten[Select[lhss, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
  Scan[(AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``Replace``.", "Error", #[[3]]]])&, selected];

  issues
]]

*)



Attributes[scanPatterns] = {HoldRest}

scanPatterns[pos_List, astIn_] :=
Catch[
Module[{ast, node, patSymbol, name, rhs, children, data, patterns, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[Length[children] != 2,
    AppendTo[issues, InspectionObject["PatternArguments", "``Pattern`` takes 2 arguments.", "Error", <|
      data,
      ConfidenceLevel -> 0.85|>]];
    Throw[issues];
  ];

  patSymbol = children[[1]];
  name = patSymbol["String"];
  rhs = children[[2]];

  patterns = Cases[rhs, CallNode[LeafNode[Symbol, "Pattern", _], _, _], {0, Infinity}];
  Scan[(
    If[#[[2, 1]]["String"] == name,
      AppendTo[issues, InspectionObject["DuplicatePatternName", "Pattern name " <> format[name] <> " occurs inside pattern with same name.", "Error", <|
        Source -> #[[2, 1, 3, Key[Source]]],
        "AdditionalSources" -> { patSymbol[[3, Key[Source]]] }, ConfidenceLevel -> 0.95 |> ]];
    ];
  )&, patterns];

  issues
]]



Attributes[scanControls] = {HoldRest}

scanControls[pos_List, astIn_] :=
Catch[
 Module[{ast, node, parentPos, parent, s},
  If[pos == {},
    (* top node, no parent *)
    Throw[{}]
  ];
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  s = node["String"];
  
  parentPos = Most[pos];
  parent = Extract[ast, {parentPos}][[1]];
  While[ListQ[parent],
    parentPos = Most[parentPos];
    parent = Extract[ast, {parentPos}][[1]];
  ];

  If[MatchQ[parent, CallNode[node, _, _]],
    Throw[{}]
  ];

  {InspectionObject["Control", format[s] <> " appears but is not called.", "Warning", <|
    Source -> node[[3, Key[Source]]],
    CodeActions -> {
      CodeAction["Add []", ReplaceNode, <|
        Source -> node[[3, Key[Source]]],
        (* node is a Symbol, so it is ok to insert here *)
        "ReplacementNode" -> CallNode[{node}, {LeafNode[Token`OpenSquare, "[", <||>], LeafNode[Token`CloseSquare, "]", <||>]}, <||>] |>] },
    ConfidenceLevel -> 0.85|>]}
]]




Attributes[scanModules] = {HoldRest}

scanModules[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, selected, params, issues, vars, counts,
  ruleDelayedRHSs, ruleDelayedRHSSymbols, ruleDelayedRHSParams, stringFunctions, paramUses, paramString, errs, srcs},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[empty[children],
    AppendTo[issues, InspectionObject["ModuleArguments", "``Module`` does not have 2 arguments.", "Error", <|
      data,
      ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  (*
  Used as a pattern or GroupMissingCloserNode, so no issues
  *)
  If[MatchQ[children[[1]],
    CallNode[LeafNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _] |
      GroupMissingCloserNode[_, _, _]]
    ,
    Throw[issues]
  ];

  If[Length[children] != 2,
    AppendTo[issues, InspectionObject["ModuleArguments", "``Module`` does not have 2 arguments.", "Error", <|
      data,
      ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  (*
  Module[Evaluate[]] denotes meta-programming
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Evaluate", _], _, _]],
    Throw[issues]
  ];

  If[!MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
    AppendTo[issues, InspectionObject["ModuleArguments", "``Module`` does not have a ``List`` for argument 1.", "Error", <|
      children[[1, 3]],
      ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], {}, _]],
    AppendTo[issues, InspectionObject["ModuleArgumentsEmpty", "``Module`` has an empty ``List`` for argument 1.", "Remark", <|
      children[[1, 3]],
      ConfidenceLevel -> 0.90|>]];
  ];


  params = children[[1, 2]];


  errs = {};

  vars = # /. {
    CallNode[LeafNode[Symbol, "Set"|"SetDelayed", _], {
      sym:LeafNode[Symbol, _, _], _}, _] :> sym,
    sym:LeafNode[Symbol, _, _] :> sym,
    (*

    Compiler syntax includes:
    Module[{ Typed[x, "Integer64"] }, x]

    TODO: support this

    CallNode[SymbolNode["Typed", {}, _], { sym:SymbolNode[_, _, _], _ }, _] :> sym
    *)
    err_ :> (AppendTo[errs, err]; Nothing)
  }& /@ params;

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[errs, FailureQ[ToFullFormString[#]]&],
    Throw[issues]
  ];

  Scan[Function[{err},
      Switch[err,
        (*
        Likely missing comma
        *)
        CallNode[LeafNode[Symbol, "Times", _], _, _],
          AppendTo[issues, InspectionObject["ModuleArguments", "Variable " <> format[ToFullFormString[err]] <>
            " does not have proper form.", "Error", <| Source -> err[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]]
        ,
        _,
          AppendTo[issues, InspectionObject["ModuleArguments", "Variable " <> format[ToFullFormString[err]] <>
            " does not have proper form.", "Error", <| Source -> err[[3, Key[Source]]], ConfidenceLevel -> 0.85|>]]
      ]
    ]
    ,
    errs
  ];

  counts = CountsBy[vars, ToFullFormString];

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[Keys[counts], FailureQ],
    Throw[issues]
  ];

  selected = Select[vars, counts[ToFullFormString[#]] > 1&];

  If[!empty[selected],
    srcs = #[[3, Key[Source]]]& /@ selected;

    AppendTo[issues, InspectionObject["DuplicateVariables", "Duplicate variables in ``Module``.", "Error",
      <| Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 1.0 |> ]];
  ];


  (*
  Now scan for leaked variables from String functions

  Related threads:
  https://mail-archive.wolfram.com/archive/l-kernel/2019/Apr00/0105.html
  https://mail-archive.wolfram.com/archive/t-alpha-scannerframeworks/2019/Dec00/0000.html
  *)
  stringFunctions =
    Cases[children[[2]],
      CallNode[LeafNode[Symbol, "StringReplace" | "StringReplaceList" | "StringSplit" | "StringCases" | "StringTrim", _], _, _], {0, Infinity}];

  ruleDelayedRHSs =
    Flatten[
      Cases[#[[2]], CallNode[LeafNode[Symbol, "RuleDelayed", _], {_, rhs_}, _] :> rhs, {0, Infinity}]& /@ stringFunctions];

  ruleDelayedRHSSymbols = Cases[ruleDelayedRHSs, LeafNode[Symbol, _, _], {0, Infinity}];

  ruleDelayedRHSParams = Select[vars, Function[{c}, MemberQ[ToFullFormString /@ ruleDelayedRHSSymbols, ToFullFormString[c]]]];

  Scan[
    Function[{ruleDelayedRHSParam},
      paramString = ToFullFormString[ruleDelayedRHSParam];

      paramUses = Select[ruleDelayedRHSSymbols, (ToFullFormString[#] == paramString)&];

      AppendTo[issues,
        InspectionObject["LeakedVariable", "Leaked variable in ``Module``: " <> format[paramString] <> ".", "Warning", <|
          Source -> ruleDelayedRHSParam[[3, Key[Source]]],
          "AdditionalSources" -> ( #[[3, Key[Source]]]& /@ paramUses ),
          ConfidenceLevel -> 0.75 |>
        ]
      ]
    ]
    ,
    ruleDelayedRHSParams
  ];

  issues
]]


Attributes[scanDynamicModules] = {HoldRest}

scanDynamicModules[pos_List, astIn_] :=
Catch[
Module[{ast, node, children, data, selected, params, issues, vars, counts, errs, srcs},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[empty[children],
    AppendTo[issues, InspectionObject["DynamicModuleArguments", "``DynamicModule`` does not have 2 arguments.", "Error", <|
      data,
      ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  (*
  Being used as a pattern or GroupMissingCloserNode, so no issues
  *)
  If[MatchQ[children[[1]],
    CallNode[LeafNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _] |
      GroupMissingCloserNode[_, _, _]]
    ,
    Throw[issues]
  ];

  If[!(Length[children] >= 2),
    AppendTo[issues, InspectionObject["DynamicModuleArguments", "``DynamicModule`` does not have 2 arguments.", "Error", <|
      data,
      ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  (*

  DynamicModule takes options

  If[Length[children] != 2,
    AppendTo[issues, Lint["DynamicModuleArguments", {LintBold["DynamicModule"], " does not have 2 arguments. This may be ok if ",
                              LintBold["DynamicModule"], " is handled programmatically."}, "Error", data]];
    Throw[issues]
  ];
  *)

  (*
      DynamicModule[Evaluate[]] denotes meta-programming
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Evaluate", _], _, _]],
    Throw[issues]
  ];

  If[!MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
    AppendTo[issues, InspectionObject["DynamicModuleArguments", "``DynamicModule`` does not have a ``List`` for argument 1.", "Error", <|
      children[[1, 3]], ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], {}, _]],
    AppendTo[issues, InspectionObject["DynamicModuleArgumentsEmpty", "``DynamicModule`` has an empty ``List`` for argument 1.", "Remark", <|
      children[[1, 3]], ConfidenceLevel -> 0.90|>]];
  ];

  params = children[[1, 2]];


  errs = {};

  vars = # /. {
    CallNode[LeafNode[Symbol, "Set"|"SetDelayed", _], {
      sym:LeafNode[Symbol, _, _], _}, _] :> sym,
    sym:LeafNode[Symbol, _, _] :> sym,
    err_ :> (AppendTo[errs, err]; Nothing)
  }& /@ params;

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[errs, FailureQ[ToFullFormString[#]]&],
    Throw[issues]
  ];

  Scan[Function[{err},
      Switch[err,
        (*
        Likely missing comma
        *)
        CallNode[LeafNode[Symbol, "Times", _], _, _],
          AppendTo[issues, InspectionObject["DynamicModuleArguments", "Variable " <> format[ToFullFormString[err]] <>
            " does not have proper form.", "Error", <| Source -> err[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]]
        ,
        _,
          AppendTo[issues, InspectionObject["DynamicModuleArguments", "Variable " <> format[ToFullFormString[err]] <>
            " does not have proper form.", "Error", <| Source -> err[[3, Key[Source]]], ConfidenceLevel -> 0.85|>]]
      ]
    ]
    ,
    errs
  ];

  counts = CountsBy[vars, ToFullFormString];

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[Keys[counts], FailureQ],
    Throw[issues]
  ];

  selected = Select[vars, (counts[ToFullFormString[#]] > 1)&];

  If[!empty[selected],
    srcs = #[[3, Key[Source]]]& /@ selected;

    AppendTo[issues, InspectionObject["DuplicateVariables", "Duplicate variables in ``DynamicModule``.", "Error", <|
      Source -> First[srcs],
      "AdditionalSources" -> Rest[srcs],
      ConfidenceLevel -> 1.0 |> ]
    ];
  ];

  issues
]]



(*

With has an undocumented syntax for allowing variables to refer to previously With'd variables

In[32]:= With[{a=1}, {b=Hold[a]}, b+1]
Out[32]= 1+Hold[1]
*)
Attributes[scanWiths] = {HoldRest}

scanWiths[pos_List, astIn_] :=
Catch[
Module[{ast, node, children, data, selected, paramLists, issues, varsAndVals, vars, vals,
  counts, errs, srcs},
  
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[empty[children],
    
    AppendTo[issues, InspectionObject["WithArguments", "``With`` does not have 2 or more arguments.", "Error", <|
      data,
      ConfidenceLevel -> 0.55|>]];

    Throw[issues];
  ];

  (*
  Being used as a pattern or GroupMissingCloserNode, so no issues
  *)
  If[MatchQ[children[[1]],
    CallNode[LeafNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _] |
      GroupMissingCloserNode[_, _, _]]
    ,
    Throw[issues]
  ];

  If[Length[children] < 2,
    AppendTo[issues, InspectionObject["WithArguments", "``With`` does not have 2 or more arguments.", "Error", <|
      data,
      ConfidenceLevel -> 0.55|>]];

    Throw[issues];
  ];

  If[!MatchQ[Most[children], {CallNode[LeafNode[Symbol, "List", _], _, _]...}],
    AppendTo[issues, InspectionObject["WithArguments", "``With`` does not have a ``List`` for most arguments.", "Error", <|
      Source -> {#[[1, 3, Key[Source], 1]], #[[-1, 3, Key[Source], 2]]}&[Most[children]],
      ConfidenceLevel -> 0.55|>]];

    Throw[issues];
  ];


  (*
      With[Evaluate[]] denotes meta-programming
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Evaluate", _], _, _]],
    Throw[issues]
  ];

  (*
      Use {_, ___} to match 1 or more children instead of using { __ }
      because of bug 382974

      MatchQ[f[], f[__]...] returns True

      Related bugs: 382974
  *)
  (* Having empty {} as With variable argument is not critical, but a warning may be issued *)
  If[!MatchQ[Most[children], {CallNode[LeafNode[Symbol, "List", _], { _, ___ }, _]...}],
    AppendTo[issues, InspectionObject["WithArgumentsEmpty", "``With`` does not have a ``List`` with arguments for most arguments.", "Remark", <|
      Source -> {#[[1, 3, Key[Source], 1]], #[[-1, 3, Key[Source], 2]]}&[Most[children]],
      ConfidenceLevel -> 0.90|>]];
  ];

  paramLists = Most[children][[All, 2]];


  errs = {};

  varsAndVals = Function[{list}, # /. {
    CallNode[LeafNode[Symbol, "Set"|"SetDelayed", _], {
      sym:LeafNode[Symbol, _, _], val_}, _] :> {sym, val},
    err_ :> (AppendTo[errs, err]; Nothing)
  }& /@ list] /@ paramLists;

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[errs, FailureQ[ToFullFormString[#]]&],
    Throw[issues]
  ];

  Scan[Function[{err},
      Switch[err,
        (*
        Likely missing comma
        *)
        CallNode[LeafNode[Symbol, "Times", _], _, _],
          AppendTo[issues, InspectionObject["WithArguments", "Variable " <> format[ToFullFormString[err]] <>
            " does not have proper form.", "Error", <| Source -> err[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]]
        ,
        _,
          AppendTo[issues, InspectionObject["WithArguments", "Variable " <> format[ToFullFormString[err]] <>
            " does not have proper form.", "Error", <| Source -> err[[3, Key[Source]]], ConfidenceLevel -> 0.85|>]]
      ]
    ]
    ,
    errs
  ];

  varsAndVals = DeleteCases[varsAndVals, {}];

  If[empty[varsAndVals],
    Throw[issues]
  ];

  {vars, vals} = Transpose[Transpose /@ varsAndVals];

  Scan[
    Function[varsList,
    
      counts = CountsBy[varsList, ToFullFormString];

      (*
      bail out if there are errors and ToFullFormString has failed
      *)
      If[AnyTrue[Keys[counts], FailureQ],
        Throw[issues]
      ];

      selected = Select[varsList, counts[ToFullFormString[#]] > 1&];

      If[!empty[selected],
        srcs = #[[3, Key[Source]]]& /@ selected;

        AppendTo[issues, InspectionObject["DuplicateVariables", "Duplicate variables in ``With``.", "Error", <|
          Source -> First[srcs],
          "AdditionalSources" -> Rest[srcs],
          ConfidenceLevel -> 1.0 |> ]];
      ];
    ]
    ,
    vars
  ];

  issues
]]



Attributes[scanBlocks] = {HoldRest}

scanBlocks[pos_List, astIn_] :=
Catch[
Module[{ast, node, head, children, data, selected, params, issues, varsWithSet, varsWithoutSet,
  vars, counts, errs, srcs},

  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  head = node[[1]];
  children = node[[2]];
  data = node[[3]];
  issues = {};

  If[empty[children],
    AppendTo[issues, InspectionObject["BlockArguments", format[head["String"]] <> " does not have 2 arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  (*
  Being used as a pattern or GroupMissingCloserNode, so no issues
  *)
  If[MatchQ[children[[1]],
    CallNode[LeafNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _] |
      GroupMissingCloserNode[_, _, _]]
    ,
    Throw[issues]
  ];

  If[Length[children] != 2,
    AppendTo[issues, InspectionObject["BlockArguments", format[head["String"]] <> " does not have 2 arguments.", "Error", <|data, ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  (*
      Block[Evaluate[]] denotes meta-programming
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Evaluate", _], _, _]],
    Throw[issues]
  ];

  If[!MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
    AppendTo[issues, InspectionObject["BlockArguments", format[head["String"]] <> " does not have a ``List`` for argument 1.", "Error", <|
      children[[1, 3]], ConfidenceLevel -> 0.55|>]];
    Throw[issues]
  ];

  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], {}, _]],
    AppendTo[issues, InspectionObject["BlockArgumentsEmpty", "``Block`` has an empty ``List`` for argument 1.", "Remark", <|
      children[[1, 3]], ConfidenceLevel -> 0.90|>]];
  ];

  params = children[[1, 2]];

  varsWithSet = {};
  varsWithoutSet = {};


  errs = {};

  Scan[# /. {
    CallNode[LeafNode[Symbol, "Set"|"SetDelayed", _], {
      sym:LeafNode[_, _, _], _}, _] :> (AppendTo[varsWithSet, sym]),
    sym:LeafNode[Symbol, _, _] :> (AppendTo[varsWithoutSet, sym]),
    err_ :> (AppendTo[errs, err]; Nothing)
  }&, params];

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[errs, FailureQ[ToFullFormString[#]]&],
    Throw[issues]
  ];

  Scan[Function[{err},
      Switch[err,
        (*
        Likely missing comma
        *)
        CallNode[LeafNode[Symbol, "Times", _], _, _],
          AppendTo[issues, InspectionObject["BlockArguments", "Variable " <> format[ToFullFormString[err]] <>
            " does not have proper form.", "Error", <| Source -> err[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]]
        ,
        _,
          AppendTo[issues, InspectionObject["BlockArguments", "Variable " <> format[ToFullFormString[err]] <>
            " does not have proper form.", "Error", <| Source -> err[[3, Key[Source]]], ConfidenceLevel -> 0.85|>]]
      ]
    ]
    ,
    errs
  ];

  vars = varsWithSet ~Join~ varsWithoutSet;

  counts = CountsBy[vars, ToFullFormString];

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[Keys[counts], FailureQ],
    Throw[issues]
  ];

  selected = Select[vars, counts[ToFullFormString[#]] > 1&];

  If[!empty[selected],
    srcs = #[[3, Key[Source]]]& /@ selected;

    AppendTo[issues, InspectionObject["DuplicateVariables", "Duplicate variables in ``Block``.", "Error",
      <| Source->First[srcs], "AdditionalSources"->Rest[srcs], ConfidenceLevel -> 1.0 |> ]];
  ];

  issues
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
  pats = Cases[opt, CallNode[LeafNode[Symbol, "Pattern", _], _, _], {0, Infinity}];
  Scan[(
    AppendTo[issues, InspectionObject["NamedPatternInOptional", "Named pattern " <> format[ToFullFormString[#[[2, 1]]]] <> " in ``Optional``.", "Error", <|
      #[[3]],
      ConfidenceLevel -> 0.95|>]]
  )&, pats];

  issues
]


Attributes[scanBadSymbols] = {HoldRest}

scanBadSymbols[pos_List, astIn_] :=
Module[{ast, node, name, data, issues, src},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  name = node["String"];
  data = node[[3]];

  issues = {};

  src = data[Source];

  Switch[name,
    "Failed",
      AppendTo[issues,
        InspectionObject["BadSymbol", "``Failed`` does not exist in **System`** context.", "Error",
          <|
            Source -> src,
            ConfidenceLevel -> 0.75,
            CodeActions -> {
              CodeAction["Replace with ``$Failed``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[$Failed]|>]
            },
            "Argument" -> name
          |>
        ]
      ]
    ,
    "Boolean",
      AppendTo[issues,
        InspectionObject["BadSymbol", "``Boolean`` does not exist in **System`** context.", "Error",
          <|
            Source -> src,
            ConfidenceLevel -> 0.75,
            CodeActions -> {
              CodeAction["Replace with ``True|False``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[True|False]|>]
            },
            "Argument" -> name
          |>
        ]
      ]
    ,
    "Match",
      AppendTo[issues,
        InspectionObject["BadSymbol", "``Match`` does not exist in **System`** context.", "Error",
          <|
            Source -> src,
            ConfidenceLevel -> 0.75,
            CodeActions -> {
              CodeAction["Replace with ``MatchQ``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[MatchQ]|>]
            },
            "Argument" -> name
          |>
        ]
      ]
    ,
    "UnSameQ",
      AppendTo[issues,
        InspectionObject["BadSymbol", "``UnSameQ`` does not exist in **System`** context.", "Error",
          <|
            Source -> src,
            ConfidenceLevel -> 0.95,
            CodeActions -> {
              CodeAction["Replace with ``UnsameQ``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[UnsameQ]|>]
            },
            "Argument" -> name
          |>
        ]
      ]
    ,
    "StringMatch",
      AppendTo[issues,
        InspectionObject["BadSymbol", "``StringMatch`` does not exist in **System`** context.", "Error",
          <|
            Source -> src,
            ConfidenceLevel -> 0.95,
            CodeActions -> {
              CodeAction["Replace with ``StringMatchQ``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[StringMatchQ]|>]
            },
            "Argument" -> name
          |>
        ]
      ]
    ,
    "OptionsQ",
      AppendTo[issues,
        InspectionObject["BadSymbol", "``OptionsQ`` does not exist in **System`** context.", "Error",
          <|
            Source -> src,
            ConfidenceLevel -> 0.95,
            CodeActions -> {
              CodeAction["Replace with ``OptionQ``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[OptionQ]|>]
            },
            "Argument" -> name
          |>
        ]
      ]
    ,
    "OptionPattern",
      AppendTo[issues,
        InspectionObject["BadSymbol", "``OptionPattern`` does not exist in **System`** context.", "Error",
          <|
            Source -> src,
            ConfidenceLevel -> 0.95,
            CodeActions -> {
              CodeAction["Replace with ``OptionsPattern``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[OptionsPattern]|>]
            },
            "Argument" -> name
          |>
        ]
      ]
    ,
    _,
      (* everything else *)
      AppendTo[issues,
        InspectionObject["BadSymbol", "``" <> name <> "`` does not exist in **System`** context.", "Error",
          <|
            Source -> src,
            ConfidenceLevel -> 0.75,
            "Argument" -> name
          |>
        ]
      ]
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
Module[{ast, node, var, data, parentPos, parent, withChildren},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  var = node[[2, 1]];
  data = node[[3]];

  If[Length[pos] >= 4,
    parentPos = Drop[pos, -4];
    parent = Extract[ast, {parentPos}][[1]];
    Switch[parent,
      CallNode[LeafNode[Symbol, "With", _], _, _],
        (*
        It is a common idiom to do With[{a = a}, foo], so do not warn about that
        *)
        withChildren = Extract[ast, { Drop[pos, -3] }][[1]];

        (* and make sure to only skip  With[{a = a}, foo]  and  With[{a=1}, {b=b}, a+b]  and still report  With[{}, a=a] *)
        If[pos[[-3]] != Length[withChildren],
          Throw[{}]
        ]
      ,
      CallNode[LeafNode[Symbol, "Block", _], _, _],
        (*
        It is a (somewhat) common idiom to do Block[{$ContextPath = $ContextPath}, foo], so do not warn about that
        *)
        (* and make sure to only skip  Block[{$ContextPath = $ContextPath}, foo]  and still report  Block[{}, $ContextPath = $ContextPath] *)
        If[pos[[-3]] == 1,
          Throw[{}]
        ]
    ]
  ];

  {InspectionObject["SelfAssignment", "Self assignment: " <> format[ToFullFormString[var]] <> ".", "Warning", <|
    data,
    ConfidenceLevel -> 0.95|>]}
]]





Attributes[scanLoadJavaClassSystem] = {HoldRest}

scanLoadJavaClassSystem[pos_List, astIn_] :=
Catch[
Module[{ast, node, var, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  var = node[[2, 1]];
  data = node[[3]];

  {InspectionObject["LoadJavaClassSystem", "``LoadJavaClass[\"java.lang.System\"]`` redefines symbols in **System`** context.\n\
This can interfere with system functionality.", "Warning", <|
    Source -> data[[Key[Source]]],
    ConfidenceLevel -> 0.95,
    CodeActions -> {
      CodeAction["Replace with LoadJavaClass[\"java.lang.System\", AllowShortContext->False]", ReplaceNode, <|
        "ReplacementNode" ->
          CallNode[{LeafNode[Symbol, "LoadJavaClass", <||>]}, {
            GroupNode[GroupSquare, {
              LeafNode[Token`OpenSquare, "[", <||>],
              InfixNode[Comma, {
                LeafNode[String, "\"java.lang.System\"", <||>],
                LeafNode[Token`Comma, ",", <||>],
                LeafNode[Whitespace, " ", <||>],
                BinaryNode[Rule, {
                  LeafNode[Symbol, "AllowShortContext", <||>],
                  LeafNode[Token`MinusGreater, "->", <||>],
                  LeafNode[Symbol, "False", <||>]}, <||>]}, <||>],
                LeafNode[Token`CloseSquare, "]", <||>]}, <||>]}, <||>],
        Source -> data[[Key[Source]]]
      |>]
    }|>]}
]]



Attributes[scanPrivateContextNode] = {HoldRest}

scanPrivateContextNode[pos_List, astIn_] :=
Catch[
Module[{ast, node, str, strData},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];

  str = node[[1, 1]];
  strData = str[[3]];

  {InspectionObject["SuspiciousPrivateContext", "Suspicious context: ``\"Private`\"``.", "Error", <|
    Source -> strData[[Key[Source]]],
    ConfidenceLevel -> 0.95,
    CodeActions -> {
      CodeAction["Replace with \"`Private`\"", ReplaceNode, <|
        "ReplacementNode" -> ToNode["`Private`"],
        Source -> strData[[Key[Source]]]
      |>] } |>]}
]]



Attributes[scanSessionSymbols] = {HoldRest}

scanSessionSymbols[pos_List, astIn_] :=
Catch[
Module[{ast, node, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  data = node[[3]];

  {InspectionObject["SuspiciousSessionSymbol", "Suspicious use of session symbol " <> format[node["String"]] <> ".", "Warning", <|
    data,
    ConfidenceLevel -> 0.55|>]}
]]



Attributes[scanSessionCalls] = {HoldRest}

scanSessionCalls[pos_List, astIn_] :=
Catch[
Module[{ast, node, data, head},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  head = node[[1]];
  data = node[[3]];

  {InspectionObject["SuspiciousSessionSymbol", "Suspicious use of session function " <> format[head["String"]] <> ".", "Warning", <|
    data,
    ConfidenceLevel -> 0.55|>]}
]]



(*

too noisy

Attributes[scanDebugCalls] = {HoldRest}

scanDebugCalls[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data, head},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  head = node[[1]];
  data = node[[3]];

  {Lint["DebugSymbol", "Suspicious use of debug function ``" <> head["String"] <> "``.", "Warning", data]}
]]
*)


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



(*

experimental

must handle Conditions:

f[] := g[] /; cond

f[] := Module[{}, g[] /; cond]

f[] := Module[{}, a[];b[];g[] /; cond]



Attributes[scanFiles] = {HoldRest}

scanFiles[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data, defs, issues, children, lhss, duplicates, selected},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  defs = Cases[children, CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], _, _]];

  lhss = defs[[All, 2, 1]];

  duplicates = Keys[Select[CountsBy[lhss, ToFullFormString], # > 1&]];
  selected = Flatten[Select[lhss, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
  Scan[AppendTo[issues, Lint["DuplicateDefinitions", "Duplicate definition", "Error", #[[3]]]]&, selected];

  issues
]]
*)





Attributes[scanAnds] = {HoldRest}

scanAnds[pos_List, astIn_] :=
Catch[
Module[{ast, node, children, data, selected, issues, consts, counts, firsts, srcs},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};
  
  consts = Cases[children, LeafNode[Symbol, "True"|"False", _]];
  Scan[(AppendTo[issues, InspectionObject["LogicalConstant", "Logical constant in ``And``.", "Warning", <|
    #[[3]],
    ConfidenceLevel -> 0.95|>]])&
    ,
    consts
  ];

  counts = CountsBy[children, ToFullFormString];

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[Keys[counts], FailureQ],
    Throw[issues]
  ];

  selected = Select[children, counts[ToFullFormString[#]] > 1&];

  If[!empty[selected],

    firsts = firstTokenWithSource /@ selected;

    srcs = #[[3, Key[Source]]]& /@ firsts;

    AppendTo[issues, InspectionObject["DuplicateClauses", "Duplicate clauses in ``And``.", "Error", <|
      Source -> First[srcs],
      "AdditionalSources" -> Rest[srcs],
      ConfidenceLevel -> 0.95,
      "Argument" -> "And"|> ]];
  ];

  issues
]]


Attributes[scanOrs] = {HoldRest}

scanOrs[pos_List, astIn_] :=
Catch[
Module[{ast, node, children, data, selected, issues, consts, counts, firsts, srcs},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};
  
  consts = Cases[children, LeafNode[Symbol, "True"|"False", _]];
  Scan[(AppendTo[issues, InspectionObject["LogicalConstant", "Logical constant in ``Or``.", "Warning", <|
    #[[3]],
    ConfidenceLevel -> 0.95|>]])&, consts];

  counts = CountsBy[children, ToFullFormString];

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[Keys[counts], FailureQ],
    Throw[issues]
  ];

  selected = Select[children, counts[ToFullFormString[#]] > 1&];

  If[!empty[selected],

    firsts = firstTokenWithSource /@ selected;

    srcs = #[[3, Key[Source]]]& /@ firsts;

    AppendTo[issues, InspectionObject["DuplicateClauses", "Duplicate clauses in ``Or``.", "Error",
      <| Source->First[srcs],
      "AdditionalSources"->Rest[srcs],
      ConfidenceLevel -> 0.95,
      "Argument" -> "Or"
      |> ]];
  ];

  issues
]]


Attributes[scanAlternatives] = {HoldRest}

scanAlternatives[pos_List, astIn_] :=
Catch[
Module[{ast, node, children, data, selected, issues, blanks, counts, firsts, srcs},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};
  
  (*
  if this is _ | PatternSequence[] then this is ok
  *)
  If[MatchQ[children, {CallNode[LeafNode[Symbol, "Blank", _], {}, _],
                        CallNode[LeafNode[Symbol, "PatternSequence", _], {}, _]} |
                      {CallNode[LeafNode[Symbol, "PatternSequence", _], {}, _],
                        CallNode[LeafNode[Symbol, "Blank", _], {}, _]}],
      Throw[issues]
  ];

  (*
  only test for _
  patterns like a_ may occur in Alternatives
  *)
  blanks = Cases[children, CallNode[LeafNode[Symbol, "Blank", _], {}, _]];

  Scan[(AppendTo[issues, InspectionObject["Blank", "Blank in ``Alternatives``.", "Error", <|
    #[[3]],
    ConfidenceLevel -> 0.95|>]])&, blanks];

  counts = CountsBy[children, ToFullFormString];

  (*
  bail out if there are errors and ToFullFormString has failed
  *)
  If[AnyTrue[Keys[counts], FailureQ],
    Throw[issues]
  ];

  selected = Select[children, counts[ToFullFormString[#]] > 1&];

  If[!empty[selected],

    firsts = firstTokenWithSource /@ selected;

    srcs = #[[3, Key[Source]]]& /@ firsts;

    AppendTo[issues, InspectionObject["DuplicateClauses", "Duplicate clauses in ``Alternatives``.", "Error",
      <|
        Source->First[srcs],
        "AdditionalSources"->Rest[srcs],
        ConfidenceLevel -> 0.95,
        "Argument" -> "Alternatives"
      |> ]];
  ];

  issues
]]



Attributes[scanSolverCalls] = {HoldRest}

scanSolverCalls[pos_List, astIn_] :=
Catch[
Module[{ast, node, children, data, issues, cases},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  cases = Cases[children, CallNode[LeafNode[Symbol, "EvenQ" | "OddQ" | "PrimeQ", _], _, _], Infinity];

  Scan[(AppendTo[issues, InspectionObject["BadSolverCall", "*Q function in symbolic solver. Did you mean to do this?", "Error", <|
    Source -> #[[3, Key[Source]]],
    ConfidenceLevel -> 0.90|>]])&
    ,
    cases
  ];

  issues
]]









Attributes[scanPatternRules] = {HoldRest}

scanPatternRules[pos_List, astIn_] :=
Catch[
Module[{ast, node, children, data, lhsPatterns, lhs, rhs, lhsPatternNames,
  rhsOccurringSymbols, rhsSymbols, fullForm, issues},

  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  lhs = node[[2, 1]];
  rhs = node[[2, 2]];
  lhsPatterns = Cases[lhs, CallNode[LeafNode[Symbol, "Pattern", _], _, _], {0, Infinity}];

  lhsPatternNames = #[[2, 1]]& /@ lhsPatterns;
  rhsSymbols = Cases[rhs, LeafNode[Symbol, _, _], {0, Infinity}];

  Do[
    
    fullForm = ToFullFormString[lhsPatternName];

    rhsOccurringSymbols = Select[rhsSymbols, (ToFullFormString[#] == fullForm)&];

    If[!empty[rhsOccurringSymbols],
      
      (*
      TODO: need to go from abstract Rule[a, b] to concrete a :> b
      *)
      (* choice = BinaryNode[RuleDelayed, {
        lhs,
        LeafNode[Token`ColonGreater, ":>", <||>],
        rhs
        }, <||>]; *)

      AppendTo[issues,
        InspectionObject["PatternRule", "The same symbol occurs on lhs and rhs of ``Rule``.", "Error", <|
          Source -> lhsPatternName[[3, Key[Source]]],
          "AdditionalSources" -> rhsOccurringSymbols[[All, 3, Key[Source]]],
          ConfidenceLevel -> 0.8,
          CodeActions -> {
            (* CodeAction["Replace with -> with :>", ReplaceNode, <|
              "ReplacementNode" -> choice1,
              Source -> data[[Key[Source]]]
            |>] *)
          }
        |>]
      ]
    ];
    ,
    {lhsPatternName, lhsPatternNames}
  ];

  issues
]]







Attributes[scanImageSizes] = {HoldRest}

scanImageSizes[pos_List, astIn_] :=
Catch[
Module[{ast, node, children, data, issues},

  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  AppendTo[issues, InspectionObject["ImageSize", "The option ``ImageSize`` is in points, but ``ImageDimensions`` is in pixels.", "Error", <|
    Source -> data[Source],
    ConfidenceLevel -> 0.8 |>]];

  issues
]]








(*

Explained here:
https://mathematica.stackexchange.com/questions/124199/functions-with-both-optional-arguments-and-options/124208#124208

Related issues:
https://github.com/WolframResearch/codeinspector/issues/2
https://github.com/WolframResearch/codeinspector/issues/3
*)

Attributes[scanOptionsPattern] = {HoldRest}

scanOptionsPattern[pos_List, astIn_] :=
Catch[
Module[{ast, node, data, parent, parentPos, previousPos, previous, optional, optionalChildren, optionalPattern, insideSet,
  definition, definitions, issues, actualPos},

  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  data = node[[3]];

  issues = {};

  If[pos == {},
    Throw[issues]
  ];

  actualPos = pos;

  parentPos = Most[pos];
  parent = Extract[ast, {parentPos}][[1]];
  While[ListQ[parent],
    parentPos = Most[parentPos];
    parent = Extract[ast, {parentPos}][[1]]
  ];
  
  (*
  could be something like  opts:OptionsPattern[], so go include the Pattern before proceeding
  *)
  If[MatchQ[parent, CallNode[LeafNode[Symbol, "Pattern", _], _, _]],

      If[parentPos == {},
        Throw[issues]
      ];

      actualPos = parentPos;

      parentPos = Most[parentPos];
      parent = Extract[ast, {parentPos}][[1]];
      While[ListQ[parent],
        parentPos = Most[parentPos];
        parent = Extract[ast, {parentPos}][[1]]
      ];
  ];

  previousPos = parentPos~Join~{2}~Join~{Last[actualPos] - 1};

  previous = Extract[ast, {previousPos}][[1]];

  If[!MatchQ[previous, CallNode[LeafNode[Symbol, "Optional", _], _, _]],
    Throw[issues]
  ];

  (*
  Now scan all the way up to determine whether inside a Set[] or SetDelayed[]
  *)
  insideSet = False;
  While[True,
    If[parentPos == {},
      Break[]
    ];
    parentPos = Drop[parentPos, -1];
    parent = Extract[ast, parentPos];
    If[ListQ[parent],
      parentPos = Drop[parentPos, -1];
      parent = Extract[ast, parentPos];
    ];
    If[MatchQ[parent, CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], _, _]],
      insideSet = True;
      Break[]
    ];
  ];

  If[insideSet,
    definitions = parent[[3, Key["Definitions"]]];
    Which[
      (*
      may not be at top-level and "Definitions" may be not present

      Ex:
      InitializePublicHelp[]:=
      (
        SyntaxInformation[SendMessage] = {"ArgumentsPattern" -> {_, _., OptionsPattern[]}};
      )
      *)
      MissingQ[definitions],
        Throw[issues]
      ,
      AnyTrue[definitions, (#[[2]] == "SyntaxInformation")&],
        (*
        Allow code like this to pass:
        SyntaxInformation[f] = {"ArgumentsPattern" -> {_., OptionsPattern[]}}
        *)
        Throw[issues]
    ]
  ];

  optional = previous;
  optionalChildren = optional[[2]];
  optionalPattern = optionalChildren[[1]];

  (*
  dig down into name
  *)
  If[MatchQ[optionalPattern, CallNode[LeafNode[Symbol, "Pattern", _], _, _]],
    optionalPattern = optionalPattern[[2, 2]]
  ];

  If[MatchQ[optionalPattern,
      CallNode[LeafNode[Symbol, "Blank" | "BlankSequence" | "BlankNullSequence", _], {}, _] |
      CallNode[LeafNode[Symbol, "Blank", _], {LeafNode[Symbol, "List", _]}, _]
    ]
    ,
    AppendTo[issues, InspectionObject["OptionsPattern", "``Optional`` occurs before ``OptionsPattern`` and may bind arguments intended for ``OptionsPattern``.", "Error", <|
      Source -> optionalPattern[[3, Key[Source]]],
      "AdditionalSources" -> { data[Source] },
      ConfidenceLevel -> 0.95 |>]];
  ];

  issues
]]










Attributes[scanAbstractSyntaxErrorNodes] = {HoldRest}

scanAbstractSyntaxErrorNodes[pos_List, astIn_] :=
Module[{ast, node, tag, data, tagString, children},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  tag = node[[1]];
  children = node[[2]];
  data = node[[3]];

  tagString = Block[{$ContextPath = {"AbstractSyntaxError`", "System`"}, $Context = "CodeInspector`Scratch`"}, ToString[tag]];

  Switch[tagString,
    "LinearSyntaxBang",
      {InspectionObject["LinearSyntaxBang", "Invalid syntax for ``\\!``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "NonAssociativePatternTest",
      {InspectionObject["NonAssociativePatternTest", "Invalid syntax for ``?``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "OpenParen",
      {InspectionObject["OpenParen", "Invalid syntax for ``()``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "OpenSquare",
      {InspectionObject["OpenSquare", "Invalid syntax for ``[]``.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    "PatternColonError",
      {InspectionObject["PatternColonError", "Invalid syntax for ``:``: LHS must be a symbol.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
    ,
    _,
      {InspectionObject[tagString, "Syntax error.", "Fatal", <| data, ConfidenceLevel -> 1.0 |>]}
  ]
]





Attributes[scanAbstractSyntaxIssues] = {HoldRest}

(*
Just directly convert AbstractSyntaxIssues to Lints
*)
scanAbstractSyntaxIssues[pos_List, astIn_] :=
Module[{ast, data, issues, syntaxIssues, issuesToReturn, formatIssues, encodingIssues},
  ast = astIn;
  data = Extract[ast, {pos}][[1]];
  issues = data[AbstractSyntaxIssues];

  issuesToReturn = {};

  syntaxIssues = Cases[issues, SyntaxIssue[_, _, _, _]];

  issuesToReturn = issuesToReturn ~Join~ (InspectionObject[#[[1]], #[[2]], #[[3]], #[[4]]]& /@ syntaxIssues);

  formatIssues = Cases[issues, FormatIssue[_, _, _, _]];

  issuesToReturn = issuesToReturn ~Join~ (InspectionObject[#[[1]], #[[2]], #[[3]], #[[4]]]& /@ formatIssues);

  encodingIssues = Cases[issues, EncodingIssue[_, _, _, _]];

  issuesToReturn = issuesToReturn ~Join~ (InspectionObject[#[[1]], #[[2]], #[[3]], #[[4]]]& /@ encodingIssues);

  issuesToReturn
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
