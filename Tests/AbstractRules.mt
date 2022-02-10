
Needs["CodeInspector`"]

Needs["CodeParser`"]


(*
StringCall
*)
TestMatch[
	CodeInspect["{ String[1,2,3] }"]
	,
	{InspectionObject["BadCall", _, _, _]}
	,
	TestID->"AbstractRules-20190522-C4I4L9"
]


(*
IntegerCall
*)
TestMatch[
	CodeInspect["{ Integer[1,2,3] }"]
	,
	{InspectionObject["BadCall", _, _, _]}
	,
	TestID->"AbstractRules-20190523-F3D4K1"
]

(*
RealCall
*)
TestMatch[
	CodeInspect["{ Real[1,2,3] }"]
	,
	{InspectionObject["BadCall", _, _, _]}
	,
	TestID->"AbstractRules-20190523-N1Z9G4"
]



(*
DuplicateKeys
*)
TestMatch[
	CodeInspect["<| 1->2, 1->3 |>"]
	,
	{InspectionObject["DuplicateKeys", _, _, _]}
	,
	TestID->"AbstractRules-20190523-M5A4H9"
]







(*
WhichArguments
*)
TestMatch[
	CodeInspect[" Which[] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-J1K9V9"
]

TestMatch[
	CodeInspect[" Which[1] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-V5Y3C5"
]



(*
SwitchWhichConfusion
*)
TestMatch[
	CodeInspect[" Which[$OperatingSystem, 1] "]
	,
	{InspectionObject["SwitchWhichConfusion", _, _, _]}
	,
	TestID->"AbstractRules-20190523-K1F7I6"
]

TestMatch[
	CodeInspect[" Which[a, b, _, 1] "]
	,
	{InspectionObject["SwitchWhichConfusion", _, _, _]}
	,
	TestID->"AbstractRules-20190523-E9G8B8"
]


(*
DuplicateClauses
*)

TestMatch[
	CodeInspect[" Which[a, 1, a, 2, b, 3, b, 4] "]
	,
	{InspectionObject["DuplicateClauses", _, _, _], InspectionObject["DuplicateClauses", _, _, _]}
	,
	TestID->"AbstractRules-20190523-X9U0J9"
]






(*
SwitchArguments
*)
TestMatch[
	CodeInspect[" Switch[] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-D4B0S0"
]

TestMatch[
	CodeInspect[" Switch[1] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-E2M2S7"
]

TestMatch[
	CodeInspect[" Switch[1, 2] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-O2W9G5"
]

(*
OperatingSystemLinux
*)
TestMatch[
	CodeInspect[" Switch[$OperatingSystem, \"Linux\", 2] "]
	,
	{InspectionObject["OperatingSystemLinux", _, _, _]}
	,
	TestID->"AbstractRules-20190523-Q2B2S9"
]


(*
SwitchWhichConfusion
*)
TestMatch[
	CodeInspect[" Switch[a, b, 1, True, 2] "]
	,
	{InspectionObject["SwitchWhichConfusion", _, _, _]}
	,
	TestID->"AbstractRules-20190523-B0P8P4"
]


(*
DuplicateClauses
*)
TestMatch[
	CodeInspect[" Switch[a, 1, 2, 1, 2] "]
	,
	{InspectionObject["DuplicateClauses", _, _, _]}
	,
	TestID->"AbstractRules-20190523-X0L3I6"
]





(*
DuplicateClauses
*)
TestMatch[
	CodeInspect[" If[a, b, b] "]
	,
	{InspectionObject["DuplicateClauses", _, _, _]}
	,
	TestID->"AbstractRules-20190717-B6J9M6"
]








(*
DuplicateNamedPattern
*)

TestMatch[
	CodeInspect[" a : ( a:2 ) "]
	,
	{InspectionObject["DuplicatePatternName", _, _, _]}
	,
	TestID->"AbstractRules-20190523-I6L1Y3"
]







(*
Control
*)

TestMatch[
	CodeInspect[" f[Return] "]
	,
	{InspectionObject["Control", _, _, _]}
	,
	TestID->"AbstractRules-20190523-D3W5H4"
]

TestMatch[
	CodeInspect[" f[Break] "]
	,
	{InspectionObject["Control", _, _, _]}
	,
	TestID->"AbstractRules-20190523-C0B7T6"
]

TestMatch[
	CodeInspect[" f[Continue] "]
	,
	{InspectionObject["Control", _, _, _]}
	,
	TestID->"AbstractRules-20190523-T4U6R6"
]











(*
ModuleArguments
*)
TestMatch[
	CodeInspect[" Module[] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-B5L5C3"
]

TestMatch[
	CodeInspect[" Module[1] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-C7Y5J5"
]

TestMatch[
	CodeInspect[" Module[1, 2] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-S4E7T5"
]

TestMatch[
	CodeInspect[" Module[{}] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-H1R4I0"
]

(*
DuplicateVariables
*)
TestMatch[
	CodeInspect[" Module[{a, a}, a+1] "]
	,
	{InspectionObject["DuplicateVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-O2P3W5"
]

(*
UnusedVariables
*)
TestMatch[
	CodeInspect[" Module[{a}, b] "]
	,
	{InspectionObject["UnusedVariable", _, _, _]}
	,
	TestID->"AbstractRules-20190523-A3S7S9"
]

TestMatch[
	CodeInspect[" Module[{a}, b] "]
	,
	{InspectionObject["UnusedVariable", _, _, _]}
	,
	TestID->"AbstractRules-20190523-V4X7X0"
]









(*
DynamicModuleArguments
*)
TestMatch[
	CodeInspect[" DynamicModule[] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-L8J1T4"
]

TestMatch[
	CodeInspect[" DynamicModule[1] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-W2N3A3"
]

TestMatch[
	CodeInspect[" DynamicModule[1, 2] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-V4L5J8"
]

TestMatch[
	CodeInspect[" DynamicModule[{}] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-D0L1F5"
]

(*
DuplicateVariables
*)
TestMatch[
	CodeInspect[" DynamicModule[{a, a}, a+1] "]
	,
	{InspectionObject["DuplicateVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-X3I6F8"
]

(*
UnusedVariables
*)

TestMatch[
	CodeInspect[" DynamicModule[{a}, b] "]
	,
	{InspectionObject["UnusedVariable", _, _, _]}
	,
	TestID->"AbstractRules-20190523-I7M0H0"
]







(*
WithArguments
*)
TestMatch[
	CodeInspect[" With[] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-S0M2V2"
]

TestMatch[
	CodeInspect[" With[1] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-U9S3P9"
]

TestMatch[
	CodeInspect[" With[1, 2] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-M8E9Y5"
]

TestMatch[
	CodeInspect[" With[{}] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-X9G8L3"
]

TestMatch[
	CodeInspect["With[{}, {}, 34]"]
	,
	{InspectionObject["NoParameters", _, _, _], InspectionObject["NoParameters", _, _, _]}
	,
	TestID -> "AbstractRules-20191120-N6B9B5"
]

(*
DuplicateVariables
*)
TestMatch[
	CodeInspect[" With[{a=1, a=2}, a+1] "]
	,
	{InspectionObject["DuplicateVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-I2P7L0"
]

(*
UnusedVariables
*)

TestMatch[
	CodeInspect[" With[{a=2}, b] "]
	,
	{InspectionObject["UnusedParameter", _, _, _]}
	,
	TestID->"AbstractRules-20190523-Q7Z2V1"
]










(*
BlockArguments
*)
TestMatch[
	CodeInspect[" Block[] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-O5T9E8"
]

TestMatch[
	CodeInspect[" Block[1] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-P0N3N7"
]

TestMatch[
	CodeInspect[" Block[1, 2] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-M5D6K6"
]

TestMatch[
	CodeInspect[" Block[{}] "]
	,
	{InspectionObject["Arguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-Y7G9I9"
]



(*
DuplicateVariables
*)
TestMatch[
	CodeInspect[" Block[{a, a}, a+1] "]
	,
	{InspectionObject["DuplicateVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-Y6N0J1"
]

(*
UnusedBlockVariables
*)

TestMatch[
	CodeInspect[" Block[{a}, b] "]
	,
	{InspectionObject["UnusedVariable", _, _, _]}
	,
	TestID->"AbstractRules-20190523-Q7K3O8"
]




(*
NamedPatternInOptional
*)

TestMatch[
	CodeInspect[" _:b:c "]
	,
	{InspectionObject["NamedPatternInOptional", _, _, _]}
	,
	TestID->"AbstractRules-20190523-V0R4Z2"
]
	
	
	



(*
BadSymbol
*)

TestMatch[
	CodeInspect[" Failed "]
	,
	{InspectionObject["BadSymbol", _, _, _]}
	,
	TestID->"AbstractRules-20190523-I3X1Y6"
]


TestMatch[
	CodeInspect[" AnyFalse "]
	,
	{InspectionObject["BadSymbol", _, _, _]}
	,
	TestID->"AbstractRules-20190523-U6Q9N5"
]

TestMatch[
	CodeInspect[" AllFalse "]
	,
	{InspectionObject["BadSymbol", _, _, _]}
	,
	TestID->"AbstractRules-20190523-X6A5I6"
]












(*
SelfAssignment
*)
TestMatch[
	CodeInspect[" a = a "]
	,
	{InspectionObject["SelfAssignment", _, _, _]}
	,
	TestID->"AbstractRules-20190523-G9E3U1"
]





(*
LoadJavaClassSystem
*)

TestMatch[
	CodeInspect[" LoadJavaClass[\"java.lang.System\"] "]
	,
	{InspectionObject["LoadJavaClassSystem", _, _, _]}
	,
	TestID->"AbstractRules-20190523-L1S8Q4"
]

TestMatch[
	CodeInspect[" JLink`LoadJavaClass[\"java.lang.System\"] "]
	,
	{InspectionObject["LoadJavaClassSystem", _, _, _]}
	,
	TestID->"AbstractRules-20190523-L5T8B9"
]






(*
SuspiciousPrivateContext
*)

sample = FileNameJoin[{DirectoryName[$CurrentTestSource], "files", "sample.wl"}]



TestMatch[
	CodeInspect[File[sample]]
	,
	{InspectionObject["SuspiciousPrivateContext", _, _, _]}
	,
	TestID->"AbstractRules-20190523-U0K0P7"
]






Test[
	CodeInspect["a ~~ b c"]
	,
	{InspectionObject[
		"ImplicitTimesInStringExpression",
		"Suspicious implicit ``Times`` in ``StringExpression``.",
		"Error",
		<|Source -> {{1, 6}, {1, 9}}, ConfidenceLevel -> 0.95|>]}
	,
	TestID->"AbstractRules-20220211-P3T9Q1"
]





