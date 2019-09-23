BeginPackage["Lint`AbstractRules`"]

$DefaultAbstractRules


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

CallNode[LeafNode[Symbol, "List", _], { CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], _, _]... }, _] -> scanRules,

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
LeafNode[Symbol, "AnyFalse" | "System`AnyFalse" |
                  "AllFalse" | "System`AllFalse" |
                  "Failed" | "System`Failed" |
                  "Boolean" | "System`Boolean" |
                  "RealQ" | "System`RealQ" |
                  "FalseQ" | "System`FalseQ" |
                  "RationalQ" | "System`RationalQ" |
                  "ComplexQ" | "System`ComplexQ" |
                  "SymbolQ" | "System`SymbolQ", _] -> scanBadSymbols,

(*
Scan some symbols that are in System` but are undocumented
*)
LeafNode[Symbol, "CheckAll" | "System`CheckAll" |
                  "Closed" | "System`Closed" |
                  "Empty" | "System`Empty" |
                  "Evaluated" | "System`Evaluated" |
                  "Fail" | "System`Fail" |
                  "Open" | "System`Open" |
                  "TensorQ" | "System`TensorQ", _] -> scanUndocumentedSymbols,

(*
Scan symbols that are documented as OBSOLETE
*)
LeafNode[Symbol, "$$Media" | "System`$$Media" |
                  "$AsynchronousTask" | "System`$AsynchronousTask" |
                  "$DefaultFont" | "System`$DefaultFont" |
                  "$EntityStores" | "System`$EntityStores" |
                  "$FormatType" | "System`$FormatType" |
                  "$HTTPCookies" | "System`$HTTPCookies" |
                  "$InstallationDate" | "System`$InstallationDate" |
                  "$MachineDomain" | "System`$MachineDomain" |
                  "$ProductInformation" | "System`$ProductInformation" |
                  "$ProgramName" | "System`$ProgramName" |
                  "$RandomState" | "System`$RandomState" |
                  "$ScheduledTask" | "System`$ScheduledTask" |
                  "$TemporaryPrefix" | "System`$TemporaryPrefix" |
                  "$TextStyle" | "System`$TextStyle" |
                  "$TopDirectory" | "System`$TopDirectory" |
                  "$UserAddOnsDirectory" | "System`$UserAddOnsDirectory" |
                  "AbortScheduledTask" | "System`AbortScheduledTask" |
                  "Active" | "System`Active" |
                  "AlgebraicRules" | "System`AlgebraicRules" |
                  "Alias" | "System`Alias" |
                  "AmbientLight" | "System`AmbientLight" |
                  "AnatomyForm" | "System`AnatomyForm" |
                  "AnimationCycleOffset" | "System`AnimationCycleOffset" |
                  "AnimationCycleRepetitions" | "System`AnimationCycleRepetitions" |
                  "AnimationDisplayTime" | "System`AnimationDisplayTime" |
                  "AspectRatioFixed" | "System`AspectRatioFixed" |
                  "AstronomicalData" | "System`AstronomicalData" |
                  "AsynchronousTaskObject" | "System`AsynchronousTaskObject" |
                  "AsynchronousTasks" | "System`AsynchronousTasks" |
                  "AudioDevice" | "System`AudioDevice" |
                  "ButtonEvaluator" | "System`ButtonEvaluator" |
                  "ButtonExpandable" | "System`ButtonExpandable" |
                  "ButtonFrame" | "System`ButtonFrame" |
                  "ButtonMargins" | "System`ButtonMargins" |
                  "ButtonNote" | "System`ButtonNote" |
                  "ButtonStyle" | "System`ButtonStyle" |
                  "CDFInformation" | "System`CDFInformation" |
                  "CellArray" | "System`CellArray" |
                  "ChebyshevDistance" | "System`ChebyshevDistance" |
                  "ClassifierInformation" | "System`ClassifierInformation" |
                  "ClipFill" | "System`ClipFill" |
                  "ColorOutput" | "System`ColorOutput" |
                  "ColumnForm" | "System`ColumnForm" |
                  "Compose" | "System`Compose" |
                  "ConstrainedMax" | "System`ConstrainedMax" |
                  "ConstrainedMin" | "System`ConstrainedMin" |
                  "ContourGraphics" | "System`ContourGraphics" |
                  "ContourLevels" | "System`ContourLevels" |
                  "ContourLines" | "System`ContourLines" |
                  "ContourSpacing" | "System`ContourSpacing" |
                  "ConversionOptions" | "System`ConversionOptions" |
                  "CreateScheduledTask" | "System`CreateScheduledTask" |
                  "CreateTemporary" | "System`CreateTemporary" |
                  "Date" | "System`Date" |
                  "Debug" | "System`Debug" |
                  "DefaultColor" | "System`DefaultColor" |
                  "DefaultFont" | "System`DefaultFont" |
                  "DensityGraphics" | "System`DensityGraphics" |
                  "Display" | "System`Display" |
                  "DisplayString" | "System`DisplayString" |
                  "DotPlusLayer" | "System`DotPlusLayer" |
                  "DragAndDrop" | "System`DragAndDrop" |
                  "DSolveConstants" | "System`DSolveConstants" |
                  "Dump" | "System`Dump" |
                  "EdgeLabeling" | "System`EdgeLabeling" |
                  "EdgeRenderingFunction" | "System`EdgeRenderingFunction" |
                  "EvaluateScheduledTask" | "System`EvaluateScheduledTask" |
                  "ExpectedValue" | "System`ExpectedValue" |
                  "FactorComplete" | "System`FactorComplete" |
                  "FontForm" | "System`FontForm" |
                  "FormTheme" | "System`FormTheme" |
                  "FromASCII" | "System`FromASCII" |
                  "FromDate" | "System`FromDate" |
                  "FullOptions" | "System`FullOptions" |
                  "GraphicsArray" | "System`GraphicsArray" |
                  "GraphicsSpacing" | "System`GraphicsSpacing" |
                  "GridBaseline" | "System`GridBaseline" |
                  "HeldPart" | "System`HeldPart" |
                  "HiddenSurface" | "System`HiddenSurface" |
                  "HomeDirectory" | "System`HomeDirectory" |
                  "HTMLSave" | "System`HTMLSave" |
                  "ImageRotated" | "System`ImageRotated" |
                  "InstanceNormalizationLayer" | "System`InstanceNormalizationLayer" |
                  "LegendreType" | "System`LegendreType" |
                  "LightSources" | "System`LightSources" |
                  "LinkOpen" | "System`LinkOpen" |
                  "Literal" | "System`Literal" |
                  "LongestMatch" | "System`LongestMatch" |
                  "LUBackSubstitution" | "System`LUBackSubstitution" |
                  "MeshRange" | "System`MeshRange" |
                  "NextScheduledTaskTime" | "System`NextScheduledTaskTime" |
                  "NotebookCreate" | "System`NotebookCreate" |
                  "OpenTemporary" | "System`OpenTemporary" |
                  "PackingMethod" | "System`PackingMethod" |
                  "Plot3Matrix" | "System`Plot3Matrix" |
                  "PlotDivision" | "System`PlotDivision" |
                  "PlotJoined" | "System`PlotJoined" |
                  "PolygonIntersections" | "System`PolygonIntersections" |
                  "PredictorInformation" | "System`PredictorInformation" |
                  "QuantityThread" | "System`QuantityThread" |
                  "Random" | "System`Random" |
                  "RasterArray" | "System`RasterArray" |
                  "RecognitionThreshold" | "System`RecognitionThreshold" |
                  "Release" | "System`Release" |
                  "RemoveAsynchronousTask" | "System`RemoveAsynchronousTask" |
                  "RemoveScheduledTask" | "System`RemoveScheduledTask" |
                  "RenderAll" | "System`RenderAll" |
                  "ReplaceHeldPart" | "System`ReplaceHeldPart" |
                  "ResetMedium" | "System`ResetMedium" |
                  "ResetScheduledTask" | "System`ResetScheduledTask" |
                  "ResumePacket" | "System`ResumePacket" |
                  "RunScheduledTask" | "System`RunScheduledTask" |
                  "ScheduledTaskActiveQ" | "System`ScheduledTaskActiveQ" |
                  "ScheduledTaskInformation" | "System`ScheduledTaskInformation" |
                  "ScheduledTaskObject" | "System`ScheduledTaskObject" |
                  "ScheduledTasks" | "System`ScheduledTasks" |
                  "SelectionAnimate" | "System`SelectionAnimate" |
                  "SequenceAttentionLayer" | "System`SequenceAttentionLayer" |
                  "SequenceForm" | "System`SequenceForm" |
                  "Shading" | "System`Shading" |
                  "ShortestMatch" | "System`ShortestMatch" |
                  "SingularValues" | "System`SingularValues" |
                  "SkinStyle" | "System`SkinStyle" |
                  "Splice" | "System`Splice" |
                  "StartAsynchronousTask" | "System`StartAsynchronousTask" |
                  "StartScheduledTask" | "System`StartScheduledTask" |
                  "StateDimensions" | "System`StateDimensions" |
                  "StopAsynchronousTask" | "System`StopAsynchronousTask" |
                  "StopScheduledTask" | "System`StopScheduledTask" |
                  "StyleForm" | "System`StyleForm" |
                  "StylePrint" | "System`StylePrint" |
                  "Subscripted" | "System`Subscripted" |
                  "SurfaceColor" | "System`SurfaceColor" |
                  "SurfaceGraphics" | "System`SurfaceGraphics" |
                  "SuspendPacket" | "System`SuspendPacket" |
                  "TeXSave" | "System`TeXSave" |
                  "TextStyle" | "System`TextStyle" |
                  "ThreadDepth" | "System`ThreadDepth" |
                  "TimeWarpingCorrespondence" | "System`TimeWarpingCorrespondence" |
                  "TimeWarpingDistance" | "System`TimeWarpingDistance" |
                  "ToASCII" | "System`ToASCII" |
                  "ToDate" | "System`ToDate" |
                  "ToFileName" | "System`ToFileName" |
                  "ToHeldExpression" | "System`ToHeldExpression" |
                  "URLFetch" | "System`URLFetch" |
                  "URLFetchAsynchronous" | "System`URLFetchAsynchronous" |
                  "URLSave" | "System`URLSave" |
                  "URLSaveAsynchronous" | "System`URLSaveAsynchronous" |
                  "VertexCoordinateRules" | "System`VertexCoordinateRules" |
                  "VertexLabeling" | "System`VertexLabeling" |
                  "VertexRenderingFunction" | "System`VertexRenderingFunction" |
                  "WaitAsynchronousTask" | "System`WaitAsynchronousTask" |
                  "WindowMovable" | "System`WindowMovable", _] -> scanObsoleteSymbols,

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

CallNode[LeafNode[Symbol, "LoadJavaClass" | "JLink`LoadJavaClass", _], { LeafNode[String, "\"java.lang.System\"", _] }, _] -> scanLoadJavaClassSystem,





(*
scan for a := a  and  a = a
possible results from batch renaming symbols
*)
CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], { LeafNode[Symbol, token_, _], LeafNode[Symbol, token_, _] }, _] -> scanSelfAssignments,


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
 Module[{ast, node, data, head, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  head = node[[1]];
  name = head["String"];
  data = head[[3]];

  issues = {};

    Switch[name,
    "String",
      AppendTo[issues, Lint["BadCall", "Calling ``String`` as a function.\n\
Did you mean ``StringQ``?\n\
This may be ok if ``String`` is handled programmatically.", "Error", <| data, CodeActions->{ CodeAction["Replace ``String`` with ``StringQ``", ReplaceNode, <| "ReplacementNode" -> ToNode[StringQ] |>] } |>]]
    ,
      "Integer",
      AppendTo[issues, Lint["BadCall", "Calling ``Integer`` as a function.\n\
Did you mean ``IntegerQ``?\n\
This may be ok if ``Integer`` is handled programmatically.", "Error", <| data, CodeActions->{ CodeAction["Replace ``Integer`` with ``IntegerQ``", ReplaceNode, <| "ReplacementNode" -> ToNode[IntegerQ] |>] } |>]]
    ,
      "Real",
      AppendTo[issues, Lint["BadCall", "Calling ``Real`` as a function.\n\
Did you mean ``Developer`RealQ``?\n\
This may be ok if ``Real`` is handled programmatically.", "Error", <| data, CodeActions->{ CodeAction["Replace ``Real`` with ``Developer`RealQ``", ReplaceNode, <| "ReplacementNode" -> ToNode[Developer`RealQ] |>] } |>]]
    ,
      "True",
      AppendTo[issues, Lint["BadCall", "Calling ``True`` as a function.\n\
Did you mean ``TrueQ``?\n\
This may be ok if ``True`` is handled programmatically.", "Error", <| data, CodeActions->{ CodeAction["Replace ``True`` with ``TrueQ``", ReplaceNode, <| "ReplacementNode" -> ToNode[TrueQ] |>] } |>]]
    ,
    _,
      AppendTo[issues, Lint["BadCall", "Calling " <> format[name] <> " as a function.", "Error", data]]
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
 Module[{ast, node, children, data, duplicates, selected, issues, keys, srcs, actions},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[!MatchQ[children, {CallNode[LeafNode[Symbol, "Rule" | "RuleDelayed", _], _, _] ...}],

    (*

    too noisy

    AppendTo[issues, Lint["AssociationArguments", "``Association`` does not have ``Rule`` arguments.\n\
This may be ok if ``Association`` is used programmatically.", "Remark", data]];

    Throw[issues] *)

    Throw[{}]
  ];
  
    keys = children[[All, 2, 1]];

    duplicates = Keys[Select[CountsBy[keys, ToFullFormString], # > 1&]];
   selected = Flatten[Select[children, Function[{rule}, ToFullFormString[rule[[2, 1]]] === #]]& /@ duplicates, 1];

   If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

       actions = MapIndexed[CodeAction["Delete key " <> ToString[#2[[1]]], DeleteNode, <|Source->#|>]&, srcs];

      AppendTo[issues, Lint["DuplicateKeys", "Duplicate keys in ``Association``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs], CodeActions->actions |> ]];
   ];

    issues

  ]]





Attributes[scanRules] = {HoldRest}

scanRules[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, issues, keys, srcs},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};
  
  keys = children[[All, 2, 1]];

  (*
  heuristic

  if something like {_ -> {}, _ -> {}} then do not warn, it is just a pattern
  *)
  If[AnyTrue[keys, MatchQ[#, CallNode[LeafNode[Symbol, "Blank", _], _, _]]&],
    Throw[issues]
  ];

    duplicates = Keys[Select[CountsBy[keys, ToFullFormString], # > 1&]];
   selected = Flatten[Select[children, Function[{rule}, ToFullFormString[rule[[2, 1]]] === #]]& /@ duplicates, 1];

   If[!empty[selected],

      (*
      It is perfectly valid to have things like {1 -> NetPort["Output"], 1 -> 2 -> NetPort["Sum"]} in NetGraph

      So make Remark for now
      *)

      srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateKeys", "Duplicate keys in ``List`` of ``Rule``s.", "Remark",
            <| Source->First[srcs], "AdditionalSources"->Rest[srcs] |>]];
   ];

   issues

  ]]




Attributes[scanWhichs] = {HoldRest}

scanWhichs[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, issues, span, duplicates, selected, lintData, srcs},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  
  issues = {};

  If[empty[children],
    AppendTo[issues, 
     Lint["WhichArguments", "``Which`` does not have any arguments.\n\
This may be ok if ``Which`` has pattern arguments.", "Error", data]];
    Throw[issues]
  ];

  If[!EvenQ[Length[children]],
    AppendTo[issues, 
     Lint["WhichArguments", "``Which`` does not have even number of arguments.\n\
This may be ok if ``Which`` has pattern arguments.", "Error", data]];
    Throw[issues]
  ];


  If[MatchQ[children[[1]], LeafNode[Symbol, "$OperatingSystem", _]],
    span = children[[1]][[3]];
   AppendTo[issues, 
    Lint["SwitchWhichConfusion", "``Which`` has ``$OperatingSystem`` in first place.\n\
Did you mean ``Switch``?", "Error", span]];
  ];

  If[MatchQ[children[[-2]], CallNode[LeafNode[Symbol, "Blank", _], _, _]],
    lintData = children[[-2]][[3]];
   AppendTo[issues, 
    Lint["SwitchWhichConfusion", "``Which`` has ``_`` in last place.\n\
Did you mean ``True``?", "Error", <| lintData, CodeActions->{CodeAction["Replace ``_`` with ``True``", ReplaceNode, <| "ReplacementNode" -> ToNode[True] |>]} |>]];
  ];


  Scan[(If[MatchQ[#, CallNode[LeafNode[Symbol, "Set", _], _, _]],
    AppendTo[issues, Lint["WhichSet", "``Which`` has ``=`` as a clause.\n\
Did you mean ``==``?", "Error", #[[3]]]];
  ];)&, children[[;;;;2]]];


    duplicates = Keys[Select[CountsBy[children[[;;;;2]], ToFullFormString], # > 1&]];
   selected = Flatten[Select[children[[;;;;2]], Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];

   If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``Which``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs] |> ]];
   ];

  issues
  ]]




Attributes[scanSwitchs] = {HoldRest}

scanSwitchs[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, span, cases, duplicates, issues, selected, srcs},
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

  If[MatchQ[children[[1]], LeafNode[Symbol, "$OperatingSystem", _]],
   cases = Cases[children[[2;;-1;;2]], LeafNode[String, "\"Linux\"", _], {0, Infinity}];
   If[cases =!= {},
    span = cases[[1]][[3]];
    AppendTo[issues, Lint["OperatingSystemLinux", "``\"Linux\"`` is not a value of ``$OperatingSystem``.\n\
Did you mean ``\"Unix\"``?", "Error", span]];
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
      AppendTo[issues, Lint["SwitchArguments", "``Switch`` does not take ``Rules`` for arguments.", "Error", data]];
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
    span = children[[-2]][[3]];
    AppendTo[issues, Lint["SwitchWhichConfusion", "``Switch`` has ``True`` in last place.\n\
Did you mean ``_``?", "Warning", span]];
   ]
  ];


  duplicates = Keys[Select[CountsBy[children[[2;;;;2]], ToFullFormString], # > 1&]];
  selected = Flatten[Select[children[[2;;;;2]], Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];

  If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``Switch``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs] |> ]];
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
 Module[{ast, node, children, data, duplicates, issues, selected, srcs},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];
  
  issues = {};

  Which[
    Length[children] == 0,
      AppendTo[issues, Lint["IfArguments", "``If`` has zero arguments.", "Error", data]];
      Throw[issues];
      ,
    Length[children] == 1,
      AppendTo[issues, Lint["IfArguments", "``If`` only has one argument.\n\
This may be ok if ``If`` has pattern arguments.", "Error", data]];
      Throw[issues];
  ];

  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Set", _], _, _]],
    AppendTo[issues, Lint["IfSet", "``If`` has ``=`` as first argument.\n\
Did you mean ``==``?", "Warning", children[[1]][[3]]]];
  ];

  srcs = {};
  If[Length[children] >= 3,
      duplicates = Keys[Select[CountsBy[children[[2;;3]], ToFullFormString], # > 1&]];
      selected = Flatten[Select[children[[2;;3]], Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];

      If[!empty[selected],

            srcs = #[[3, Key[Source]]]& /@ selected;

            AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``If``.", "Warning", <|Source->First[srcs], "AdditionalSources"->Rest[srcs]|>]]
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
 Module[{ast, node, patSymbol, name, rhs, children, patterns, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[Length[children] != 2,
    AppendTo[issues, Lint["PatternArguments", "``Pattern`` takes 2 arguments.\n\
This may be ok if ``Pattern`` is handled programmatically.", "Error", data]];
    Throw[issues];
  ];

  patSymbol = children[[1]];
  name = patSymbol["String"];
  rhs = children[[2]];

  patterns = Cases[rhs, CallNode[LeafNode[Symbol, "Pattern", _], _, _], {0, Infinity}];
  Scan[(
    If[#[[2, 1]]["String"] == name,
      (*
      This is never correct code, but make a Warning for now because it is noisy
      *)
      AppendTo[issues, Lint["DuplicateNamedPattern", "Duplicate named pattern " <> format[name] <> " in RHS of ``Pattern``.",
            "Warning", <| Source -> #[[2, 1, 3, Key[Source]]], "AdditionalSources" -> { patSymbol[[3, Key[Source]]] } |> ]];
    ];
  )&, patterns];

  issues
]]



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

  {Lint["Control", format[s] <> " appears but is not called.\n\
Did you mean " <> format[s<>"[]"] <>"?\n\
This may be ok if " <> format[s] <> " is used as a symbol.", "Warning", data]}
  ]]




Attributes[scanModules] = {HoldRest}

scanModules[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, params, issues, vars, used, unusedParams},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[empty[children],
    AppendTo[issues, Lint["ModuleArguments", "``Module`` does not have 2 arguments.\n\
This may be ok if ``Module`` is handled programmatically.", "Error", data]];
    Throw[issues]
  ];

  (*
  Used as a pattern, so no issues
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _]],
    Throw[issues]
  ];

  If[Length[children] != 2,
    AppendTo[issues, Lint["ModuleArguments", "``Module`` does not have 2 arguments.\n\
This may be ok if ``Module`` is handled programmatically.", "Error", data]];
    Throw[issues]
  ];

  If[!MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
    AppendTo[issues, Lint["ModuleArguments", "``Module`` does not have a ``List`` for argument 1.\n\
This may be ok if " <> format[ToFullFormString[children[[1]]]] <> " is a pattern variable.", "Error", data]];
    Throw[issues]
  ];


  params = children[[1,2]];
   vars = # /. {CallNode[LeafNode[Symbol, "Set"|"SetDelayed", _], {sym:LeafNode[Symbol, _, _], _}, _] :> sym,
            sym:LeafNode[Symbol, _, _] :> sym,
            (*

            Compiler syntax includes:
            Module[{ Typed[x, "Integer64"] }, x]

            TODO: support this

            CallNode[SymbolNode["Typed", {}, _], { sym:SymbolNode[_, _, _], _ }, _] :> sym
            *)
            err_ :> (AppendTo[issues, Lint["ModuleArguments", "Variable " <> format[ToFullFormString[err]] <>
              " does not have proper form.\n\
This may be ok if ``Module`` is handled programmatically.", "Error", #[[3]]]]; Nothing)}& /@ params;

    duplicates = Keys[Select[CountsBy[vars, ToFullFormString], # > 1&]];
    selected = Flatten[Select[vars, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];

    If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateVariables", "Duplicate variables in ``Module``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs] |> ]];
      ];

  used = ToFullFormString /@ Cases[children[[2]], LeafNode[Symbol, _, _], {0, Infinity}];
  unusedParams = Select[vars, Function[{c}, !MemberQ[used, ToFullFormString[c]]]];

  Scan[AppendTo[issues, Lint["UnusedVariables", "Unused variables in ``Module``: " <> format[ToFullFormString[#]] <> ".", "Warning", <| #[[3]], CodeActions->{CodeAction["Delete", DeleteNode, <||>]} |> ]]&, unusedParams];

  issues
]]


Attributes[scanDynamicModules] = {HoldRest}

scanDynamicModules[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, params, issues, vars, used, unusedParams},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[empty[children],
    AppendTo[issues, Lint["DynamicModuleArguments", "``DynamicModule`` does not have 2 arguments.\n\
This may be ok if ``DynamicModule`` is handled programmatically.", "Error", data]];
    Throw[issues]
  ];

  (*
  Being used as a pattern, so no issues
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _]],
    Throw[issues]
  ];

  If[!(Length[children] >= 2),
    AppendTo[issues, Lint["DynamicModuleArguments", "``DynamicModule`` does not have 2 arguments.\n\
This may be ok if ``DynamicModule`` is handled programmatically.", "Error", data]];
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

  If[!MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
    AppendTo[issues, Lint["DynamicModuleArguments", "``DynamicModule`` does not have a ``List`` for argument 1.\n\
This may be ok if " <> format[ToFullFormString[children[[1]]]] <> " is a pattern variable.", "Error", data]];
    Throw[issues]
  ];


  params = children[[1,2]];
   vars = # /. {CallNode[LeafNode[Symbol, "Set"|"SetDelayed", _], {sym:LeafNode[Symbol, _, _], _}, _] :> sym,
            sym:LeafNode[Symbol, _, _] :> sym,
            err_ :> (AppendTo[issues, Lint["DynamicModuleArguments", "Variable " <> format[ToFullFormString[err]] <>
              " does not have proper form.\n\
This may be ok if ``DynamicModule`` is handled programmatically.", "Error", #[[3]]]]; Nothing)}& /@ params;
    duplicates = Keys[Select[CountsBy[vars, ToFullFormString], # > 1&]];
    selected = Flatten[Select[vars, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];

    If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateVariables", "Duplicate variables in ``DynamicModule``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs] |> ]];
      ];

  used = ToFullFormString /@ Cases[children[[2]], LeafNode[Symbol, _, _], {0, Infinity}];
  unusedParams = Select[vars, Function[{c}, !MemberQ[used, ToFullFormString[c]]]];

  Scan[AppendTo[issues, Lint["UnusedVariables", "Unused variables in ``DynamicModule``: " <> format[ToFullFormString[#]] <> ".", "Warning", #[[3]]]]&, unusedParams];

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
 Module[{ast, node, children, data, duplicates, selected, paramLists, issues, varsAndVals, vars, vals, usedBody, unusedParams},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  If[empty[children],
    AppendTo[issues, Lint["WithArguments", "``With`` does not have 2 or more arguments.\n\
This may be ok if ``With`` is handled programmatically.", "Error", data]];
    Throw[issues];
  ];

  (*
  Being used as a pattern, so no issues
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _]],
    Throw[issues]
  ];

  If[Length[children] < 2,
    AppendTo[issues, Lint["WithArguments", "``With`` does not have 2 or more arguments.\n\
This may be ok if ``With`` is handled programmatically.", "Error", data]];
    Throw[issues];
  ];

  If[!MatchQ[Most[children], {CallNode[LeafNode[Symbol, "List", _], _, _]...}],
    AppendTo[issues, Lint["WithArguments", "``With`` does not have a ``List`` for most arguments.\n\
This may be ok if " <> format[ToFullFormString[children[[1]]]] <> " is a pattern variable.", "Error", data]];
    Throw[issues];
  ];

  paramLists = Most[children][[All, 2]];
   
   varsAndVals = Function[{list}, # /. {CallNode[LeafNode[Symbol, "Set"|"SetDelayed", _], {sym:LeafNode[Symbol, _, _], val_}, _] :> {sym, val},
            err_ :> (AppendTo[issues, Lint["WithArguments", "Variable " <> format[ToFullFormString[err]] <> " does not have proper form.\n\
This may be ok if ``With`` is handled programmatically.", "Error", #[[3]]]]; Nothing)}& /@ list] /@ paramLists;

  If[varsAndVals == {{}},
    Throw[issues];
  ];

   {vars, vals} = Transpose[Transpose /@ varsAndVals];

    duplicates = Keys[Select[CountsBy[#, ToFullFormString], # > 1 &]]& /@ vars;
      selected = Flatten[Function[{duplicates, vars}, (Select[vars, Function[{c}, ToFullFormString[c] === #]])& /@ duplicates] @@@ Transpose[{duplicates, vars}]];

      If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateVariables", "Duplicate variables in ``With``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs] |> ]];
      ];

  usedBody = ToFullFormString /@ Cases[Last[children], LeafNode[Symbol, _, _], {0, Infinity}];

  usedAtVariousScopes = FoldList[Join[#1, ToFullFormString /@ Cases[#2, LeafNode[Symbol, _, _], {0, Infinity}]]&, usedBody, vals // Reverse] // Reverse;

  unusedParams = Function[{vars, useds}, Select[vars, Function[{c}, !MemberQ[useds, ToFullFormString[c]]]]] @@@ Transpose[{vars, Most[usedAtVariousScopes]}];

  unusedParams = Flatten[unusedParams];

  Scan[AppendTo[issues, Lint["UnusedVariables", "Unused variables in ``With``: " <> format[ToFullFormString[#]] <> ".", "Warning", #[[3]]]]&, unusedParams];

  issues
]]



Attributes[scanBlocks] = {HoldRest}

scanBlocks[pos_List, astIn_] :=
Catch[
 Module[{ast, node, head, children, data, duplicates, selected, params, issues, varsWithSet, varsWithoutSet, toDelete},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  head = node[[1]];
  children = node[[2]];
  data = node[[3]];
  issues = {};

  If[empty[children],
    AppendTo[issues, Lint["BlockArguments", format[head["String"]] <> " does not have 2 arguments.\n\
This may be ok if " <> format[head["String"]] <> " is handled programmatically.", "Error", data]];
    Throw[issues]
  ];

  (*
  Being used as a pattern, so no issues
  *)
  If[MatchQ[children[[1]], CallNode[LeafNode[Symbol, "Pattern" | "Blank" | "BlankSequence" | "BlankNullSequence", _], _, _]],
    Throw[issues]
  ];

  If[Length[children] != 2,
    AppendTo[issues, Lint["BlockArguments", format[head["String"]] <> " does not have 2 arguments.\n\
This may be ok if " <> format[head["String"]] <> " is handled programmatically.", "Error", data]];
    Throw[issues]
  ];

  If[!MatchQ[children[[1]], CallNode[LeafNode[Symbol, "List", _], _, _]],
    AppendTo[issues, Lint["BlockArguments", format[head["String"]] <> " does not have a ``List`` for argument 1.\n\
This may be ok if " <> format[ToFullFormString[children[[1]]]] <> " is a pattern variable.", "Error", data]];
    Throw[issues]
  ];

  params = children[[1,2]];

  varsWithSet = {};
  varsWithoutSet = {};

  Scan[# /. {
    CallNode[LeafNode[Symbol, "Set"|"SetDelayed", _], {sym:LeafNode[_, _, _], _}, _] :> (AppendTo[varsWithSet, sym]),
    sym:LeafNode[Symbol, _, _] :> (AppendTo[varsWithoutSet, sym]),
    err_ :> (AppendTo[issues, Lint["BlockArguments", "Variable " <> format[ToFullFormString[err]] <> " does not have proper form.\n\
This may be ok if " <> format[head["String"]] <> " is handled programmatically.", "Error", #[[3]]]])}&, params];

  vars = varsWithSet ~Join~ varsWithoutSet;

  duplicates = Keys[Select[CountsBy[vars, ToFullFormString], # > 1&]];
  selected = Flatten[Select[vars, Function[{c}, ToFullFormString[c] === #]]& /@ duplicates, 1];
  
  If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateVariables", "Duplicate variables in ``Block``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs] |> ]];
      ];

  (*
  Give unused Block variables its own tag
  *)

  used = ToFullFormString /@ Cases[children[[2]], LeafNode[Symbol, _, _], {0, Infinity}];
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
  toDelete = Select[unusedParams, fullyQualifiedSymbolQ];
  unusedParams = Complement[unusedParams, toDelete];

  (*
  after removing fully-qualified symbols, now scan for lowercase symbols and only let those through

  on the assumption that lowercase symbols will be treated as "local" variables
  *)
  unusedParams = Select[unusedParams, lowercaseSymbolQ];
  
  Scan[AppendTo[issues, Lint["UnusedBlockVariables", "Unused variables in " <> format[head["String"]] <> ": `" <>
    format[ToFullFormString[#]] <> ".", "Warning", #[[3]]]]&, unusedParams];

  issues
]]

(*
if there is a ` anywhere in the symbol, then assume it is fully-qualified
*)
fullyQualifiedSymbolQ[LeafNode[Symbol, s_, _]] :=
  StringContainsQ[s, "`"]

lowercaseSymbolQ[LeafNode[Symbol, s_, _]] :=
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
  pats = Cases[opt, CallNode[LeafNode[Symbol, "Pattern", _], _, _], {0, Infinity}];
  Scan[(
    AppendTo[issues, Lint["NamedPatternInOptional", "Named pattern " <> format[ToFullFormString[#[[2]][[1]]]] <> " in ``Optional``.", "Error", #[[3]]]]
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
    "Failed" | "System`Failed",
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``Failed``.\n\
Symbol ``Failed`` does not exist in **System`** context.\n\
Did you mean ``$Failed``?", "Error", data]]
    ,
    "AnyFalse" | "System`AnyFalse",
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``AnyFalse``.\n\
Symbol ``AnyFalse`` does not exist in **System`** context.\n\
Did you mean ``AllTrue`` (and also inverting the logic)?", "Error", data]]
    ,
    "AllFalse" | "System`AllFalse",
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``AllFalse``.\n\
Symbol ``AllFalse`` does not exist in **System`** context.\n\
Did you mean ``AnyTrue`` (and also inverting the logic)?", "Error", data]]
    ,
    "Boolean" | "System`Boolean",
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``Boolean``.\n\
Symbol ``Boolean`` does not exist in **System`** context.\n\
Did you mean ``True|False``?", "Error", data]]
    ,
    "RealQ" | "System`RealQ",
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``RealQ``.\n\
Symbol ``RealQ`` does not exist in **System`** context.", "Error", data]]
    ,
    "FalseQ" | "System`FalseQ",
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``FalseQ``.\n\
Symbol ``FalseQ`` does not exist in **System`** context.\n\
Did you mean ``TrueQ`` (and also inverting the logic)?", "Error", data]]
    ,
    "RationalQ" | "System`RationalQ",
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``RationalQ``.\n\
Symbol ``RationalQ`` does not exist in **System`** context.", "Error", data]]
    ,
    "ComplexQ" | "System`ComplexQ",
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``ComplexQ``.\n\
Symbol ``ComplexQ`` does not exist in **System`** context.", "Error", data]]
    ,
    "SymbolQ" | "System`SymbolQ",
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``SymbolQ``.\n\
Symbol ``SymbolQ`` does not exist in **System`** context.", "Error", data]]
    ,
    _,
      AppendTo[issues, Lint["BadSymbol", "Bad symbol: ``" <> name <> "``.", "Error", data]]
  ];

  issues
]

Attributes[scanUndocumentedSymbols] = {HoldRest}

scanUndocumentedSymbols[pos_List, astIn_] :=
 Module[{ast, node, name, data, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  name = node["String"];
  data = node[[3]];

  issues = {};

  Switch[name,
    "Fail" | "System`Fail",
      AppendTo[issues, Lint["UndocumentedSymbol", "Undocumented symbol: ``Fail``.\n\
Symbol ``Fail`` is an undocumented **System`** symbol.\n\
Did you mean ``$Failed``?", "Warning", data]]
    ,
    _,
      AppendTo[issues, Lint["UndocumentedSymbol", "Symbol " <> format[name] <> " is an undocumented **System`** symbol.", "Remark", data]]
  ];

  issues
]


Attributes[scanObsoleteSymbols] = {HoldRest}

scanObsoleteSymbols[pos_List, astIn_] :=
 Module[{ast, node, name, data, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  name = node["String"];
  data = node[[3]];

  issues = {};

  AppendTo[issues, Lint["ObsoleteSymbol", "Symbol " <> format[name] <> " is an obsolete **System`** symbol.", "Warning", data]];

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
    If[MatchQ[parent, CallNode[LeafNode[Symbol, "Block" | "DynamicModule" | "Module" | "With", _], _, _]],

      (* and make sure to only skip  With[{a = a}, foo]  and still report   With[{}, a=a] *)
      If[pos[[-3]] == 1,
        Throw[{}]
      ]
    ]
  ];

  {Lint["SelfAssignment", "Self assignment: " <> format[ToFullFormString[var]] <> ".", "Warning", data]}
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



Attributes[scanSessionSymbols] = {HoldRest}

scanSessionSymbols[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  data = node[[3]];

  {Lint["SessionSymbol", "Suspicious use of session symbol " <> format[node["String"]] <> ".", "Warning", data]}
]]



Attributes[scanSessionCalls] = {HoldRest}

scanSessionCalls[pos_List, astIn_] :=
Catch[
 Module[{ast, node, data, head},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  head = node[[1]];
  data = node[[3]];

  {Lint["SessionSymbol", "Suspicious use of session function " <> format[head["String"]] <> ".", "Warning", data]}
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
 Module[{ast, node, children, data, duplicates, selected, issues, consts},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};
  
  consts = Cases[children, LeafNode[Symbol, "True"|"False", _]];
  Scan[(AppendTo[issues, Lint["LogicalConstant", "Logical constant in ``And``.", "Warning", #[[3]]]])&, consts];


  duplicates = Keys[Select[CountsBy[children, ToFullFormString], # > 1&]];
  selected = Flatten[Select[children, Function[{key}, ToFullFormString[key] === #]]& /@ duplicates, 1];

  If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``And``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs] |> ]];
      ];

  issues

  ]]

Attributes[scanOrs] = {HoldRest}

scanOrs[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, issues, consts},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};
  
  consts = Cases[children, LeafNode[Symbol, "True"|"False", _]];
  Scan[(AppendTo[issues, Lint["LogicalConstant", "Logical constant in ``Or``.", "Warning", #[[3]]]])&, consts];

  duplicates = Keys[Select[CountsBy[children, ToFullFormString], # > 1&]];
  selected = Flatten[Select[children, Function[{key}, ToFullFormString[key] === #]]& /@ duplicates, 1];

  If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``Or``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs] |> ]];
      ];

  issues

  ]]

Attributes[scanAlternatives] = {HoldRest}

scanAlternatives[pos_List, astIn_] :=
Catch[
 Module[{ast, node, children, data, duplicates, selected, issues},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};
  
  duplicates = Keys[Select[CountsBy[children, ToFullFormString], # > 1&]];
  selected = Flatten[Select[children, Function[{key}, ToFullFormString[key] === #]]& /@ duplicates, 1];

  If[!empty[selected],
       srcs = #[[3, Key[Source]]]& /@ selected;

      AppendTo[issues, Lint["DuplicateClauses", "Duplicate clauses in ``Alternatives``.", "Error",
          <| Source->First[srcs], "AdditionalSources"->Rest[srcs] |> ]];
      ];

  issues

  ]]


















Attributes[scanAbstractSyntaxErrorNodes] = {HoldRest}

scanAbstractSyntaxErrorNodes[pos_List, astIn_] :=
 Module[{ast, node, token, data, tokString},
  ast = astIn;
  node = Extract[ast, {pos}][[1]];
  token = node[[1]];
  data = node[[3]];

  tokString = Block[{$ContextPath = {"AbstractSyntaxError`", "System`"}, $Context = "Lint`Scratch`"}, ToString[token]];

  {Lint["AbstractSyntaxError", "Abstract syntax error: " <> format[tokString] <> ".", "Fatal", data]}
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
