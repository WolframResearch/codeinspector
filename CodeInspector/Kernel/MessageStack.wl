BeginPackage["CodeInspector`MessageStack`"]

codeWithMessageStackInspectAST


Begin["`Private`"]

Needs["CodeInspector`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*
"The objective function `1` is not convex so the minimization cannot be solved by convex methods."
*)
codeWithMessageStackInspectAST[ast_, {HoldForm[Message[General::ctwcmin, args___]], ___}] :=
Catch[
Module[{case},

	case = FirstCase[ast, CallNode[LeafNode[Symbol, "ParametricConvexOptimization", _], {first_, ___}, _] :> first, $Failed, Infinity];
	
	If[FailureQ[case],
		Throw[{}]
	];

	{InspectionObject["General", ToString[StringForm[General::ctwcmin, args]], "Error", <|Source -> case[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]}
]]

(*
"The Import element \"`1`\" is not present when importing as `2`."
*)
codeWithMessageStackInspectAST[ast_, {HoldForm[Message[Import::noelem, args___]], ___}] :=
Catch[
Module[{case},

	case = FirstCase[ast, CallNode[LeafNode[Symbol, "ImportString", _], {_, second_, ___}, _] :> second, $Failed, Infinity];
	
	If[FailureQ[case],
		Throw[{}]
	];

	{InspectionObject["Import", ToString[StringForm[Import::noelem, args]], "Error", <|Source -> case[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]}
]]

(*
"The only allowed DistanceFunction specifications for GeoPosition are Automatic or GeoDistance."
*)
codeWithMessageStackInspectAST[ast_, {HoldForm[Message[SpatialMedian::geodist, args___]], ___}] :=
Catch[
Module[{case},

  case = FirstCase[ast, CallNode[LeafNode[Symbol, "SpatialMedian", _], {___, CallNode[LeafNode[Symbol, "Rule", _], {LeafNode[Symbol, "DistanceFunction", _], opt_}, _], ___}, _] :> opt, $Failed, Infinity];
  
  If[FailureQ[case],
    Throw[{}]
  ];

  {InspectionObject["SpatialMedian", ToString[StringForm[SpatialMedian::geodist, args]], "Error", <|Source -> case[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]}
]]

codeWithMessageStackInspectAST[ast_, stack_] = {}



End[]

EndPackage[]
