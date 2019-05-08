
Needs["Lint`"]

Needs["AST`"]


(*
StringCall
*)
TestMatch[
	LintString["{ String[1,2,3] }"]
	,
	{Lint["StringCall", _, _, _]}
	,
	TestID->"AbstractRules-20190522-C4I4L9"
]


(*
IntegerCall
*)
TestMatch[
	LintString["{ Integer[1,2,3] }"]
	,
	{Lint["IntegerCall", _, _, _]}
	,
	TestID->"AbstractRules-20190523-F3D4K1"
]

(*
RealCall
*)
TestMatch[
	LintString["{ Real[1,2,3] }"]
	,
	{Lint["RealCall", _, _, _]}
	,
	TestID->"AbstractRules-20190523-N1Z9G4"
]



(*
DuplicateKeys
*)
TestMatch[
	LintString["<| 1->2, 1->3 |>"]
	,
	{Lint["DuplicateKeys", _, _, _], Lint["DuplicateKeys", _, _, _]}
	,
	TestID->"AbstractRules-20190523-M5A4H9"
]







(*
WhichArguments
*)
TestMatch[
	LintString[" Which[] "]
	,
	{Lint["WhichArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-J1K9V9"
]

TestMatch[
	LintString[" Which[1] "]
	,
	{Lint["WhichArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-V5Y3C5"
]



(*
SwitchWhichConfusion
*)
TestMatch[
	LintString[" Which[$OperatingSystem, 1] "]
	,
	{Lint["SwitchWhichConfusion", _, _, _]}
	,
	TestID->"AbstractRules-20190523-K1F7I6"
]

TestMatch[
	LintString[" Which[a, b, _, 1] "]
	,
	{Lint["SwitchWhichConfusion", _, _, _]}
	,
	TestID->"AbstractRules-20190523-E9G8B8"
]


(*
DuplicateClauses
*)

TestMatch[
	LintString[" Which[a, b, a, b] "]
	,
	{Lint["DuplicateClauses", _, _, _], Lint["DuplicateClauses", _, _, _]}
	,
	TestID->"AbstractRules-20190523-X9U0J9"
]






(*
SwitchArguments
*)
TestMatch[
	LintString[" Switch[] "]
	,
	{Lint["SwitchArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-D4B0S0"
]

TestMatch[
	LintString[" Switch[1] "]
	,
	{Lint["SwitchArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-E2M2S7"
]

TestMatch[
	LintString[" Switch[1, 2] "]
	,
	{Lint["SwitchArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-O2W9G5"
]

(*
OperatingSystemLinux
*)
TestMatch[
	LintString[" Switch[$OperatingSystem, \"Linux\", 2] "]
	,
	{Lint["OperatingSystemLinux", _, _, _]}
	,
	TestID->"AbstractRules-20190523-Q2B2S9"
]


(*
SwitchWhichConfusion
*)
TestMatch[
	LintString[" Switch[a, b, 1, True, 2] "]
	,
	{Lint["SwitchWhichConfusion", _, _, _]}
	,
	TestID->"AbstractRules-20190523-B0P8P4"
]


(*
DuplicateClauses
*)
TestMatch[
	LintString[" Switch[a, 1, 2, 1, 2] "]
	,
	{Lint["DuplicateClauses", _, _, _], Lint["DuplicateClauses", _, _, _]}
	,
	TestID->"AbstractRules-20190523-X0L3I6"
]






(*
DuplicateNamedPattern
*)

TestMatch[
	LintString[" a : ( a:2 ) "]
	,
	{Lint["DuplicateNamedPattern", _, _, _]}
	,
	TestID->"AbstractRules-20190523-I6L1Y3"
]







(*
Control
*)

TestMatch[
	LintString[" f[Return] "]
	,
	{Lint["Control", _, _, _]}
	,
	TestID->"AbstractRules-20190523-D3W5H4"
]

TestMatch[
	LintString[" f[Break] "]
	,
	{Lint["Control", _, _, _]}
	,
	TestID->"AbstractRules-20190523-C0B7T6"
]

TestMatch[
	LintString[" f[Continue] "]
	,
	{Lint["Control", _, _, _]}
	,
	TestID->"AbstractRules-20190523-T4U6R6"
]











(*
ModuleArguments
*)
TestMatch[
	LintString[" Module[] "]
	,
	{Lint["ModuleArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-B5L5C3"
]

TestMatch[
	LintString[" Module[1] "]
	,
	{Lint["ModuleArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-C7Y5J5"
]

TestMatch[
	LintString[" Module[1, 2] "]
	,
	{Lint["ModuleArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-S4E7T5"
]

TestMatch[
	LintString[" Module[{}] "]
	,
	{Lint["ModuleArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-H1R4I0"
]

(*
DuplicateVariables
*)
TestMatch[
	LintString[" Module[{a, a}, a+1] "]
	,
	{Lint["DuplicateVariables", _, _, _], Lint["DuplicateVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-O2P3W5"
]

(*
UnusedVariables
*)
TestMatch[
	LintString[" Module[{a}, b] "]
	,
	{Lint["UnusedVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-A3S7S9"
]

TestMatch[
	LintString[" Module[{a}, b] "]
	,
	{Lint["UnusedVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-V4X7X0"
]









(*
DynamicModuleArguments
*)
TestMatch[
	LintString[" DynamicModule[] "]
	,
	{Lint["DynamicModuleArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-L8J1T4"
]

TestMatch[
	LintString[" DynamicModule[1] "]
	,
	{Lint["DynamicModuleArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-W2N3A3"
]

TestMatch[
	LintString[" DynamicModule[1, 2] "]
	,
	{Lint["DynamicModuleArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-V4L5J8"
]

TestMatch[
	LintString[" DynamicModule[{}] "]
	,
	{Lint["DynamicModuleArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-D0L1F5"
]

(*
DuplicateVariables
*)
TestMatch[
	LintString[" DynamicModule[{a, a}, a+1] "]
	,
	{Lint["DuplicateVariables", _, _, _], Lint["DuplicateVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-X3I6F8"
]

(*
UnusedVariables
*)

TestMatch[
	LintString[" DynamicModule[{a}, b] "]
	,
	{Lint["UnusedVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-I7M0H0"
]







(*
WithArguments
*)
TestMatch[
	LintString[" With[] "]
	,
	{Lint["WithArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-S0M2V2"
]

TestMatch[
	LintString[" With[1] "]
	,
	{Lint["WithArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-U9S3P9"
]

TestMatch[
	LintString[" With[1, 2] "]
	,
	{Lint["WithArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-M8E9Y5"
]

TestMatch[
	LintString[" With[{}] "]
	,
	{Lint["WithArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-X9G8L3"
]


(*
DuplicateVariables
*)
TestMatch[
	LintString[" With[{a=1, a=2}, a+1] "]
	,
	{Lint["DuplicateVariables", _, _, _], Lint["DuplicateVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-I2P7L0"
]

(*
UnusedVariables
*)

TestMatch[
	LintString[" With[{a=2}, b] "]
	,
	{Lint["UnusedVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-Q7Z2V1"
]










(*
BlockArguments
*)
TestMatch[
	LintString[" Block[] "]
	,
	{Lint["BlockArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-O5T9E8"
]

TestMatch[
	LintString[" Block[1] "]
	,
	{Lint["BlockArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-P0N3N7"
]

TestMatch[
	LintString[" Block[1, 2] "]
	,
	{Lint["BlockArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-M5D6K6"
]

TestMatch[
	LintString[" Block[{}] "]
	,
	{Lint["BlockArguments", _, _, _]}
	,
	TestID->"AbstractRules-20190523-Y7G9I9"
]



(*
DuplicateVariables
*)
TestMatch[
	LintString[" Block[{a, a}, a+1] "]
	,
	{Lint["DuplicateVariables", _, _, _], Lint["DuplicateVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-Y6N0J1"
]

(*
UnusedBlockVariables
*)

TestMatch[
	LintString[" Block[{a}, b] "]
	,
	{Lint["UnusedBlockVariables", _, _, _]}
	,
	TestID->"AbstractRules-20190523-Q7K3O8"
]




(*
NamedPatternInOptional
*)

TestMatch[
	LintString[" _:b:c "]
	,
	{Lint["NamedPatternInOptional", _, _, _]}
	,
	TestID->"AbstractRules-20190523-V0R4Z2"
]
	
	
	



(*
BadSymbol
*)

TestMatch[
	LintString[" Failed "]
	,
	{Lint["BadSymbol", _, _, _]}
	,
	TestID->"AbstractRules-20190523-I3X1Y6"
]


TestMatch[
	LintString[" AnyFalse "]
	,
	{Lint["BadSymbol", _, _, _]}
	,
	TestID->"AbstractRules-20190523-U6Q9N5"
]

TestMatch[
	LintString[" AllFalse "]
	,
	{Lint["BadSymbol", _, _, _]}
	,
	TestID->"AbstractRules-20190523-X6A5I6"
]












(*
SelfAssignment
*)
TestMatch[
	LintString[" a = a "]
	,
	{Lint["SelfAssignment", _, _, _]}
	,
	TestID->"AbstractRules-20190523-G9E3U1"
]





(*
LoadJavaClassSystem
*)

TestMatch[
	LintString[" LoadJavaClass[\"java.lang.System\"] "]
	,
	{Lint["LoadJavaClassSystem", _, _, _]}
	,
	TestID->"AbstractRules-20190523-L1S8Q4"
]

TestMatch[
	LintString[" JLink`LoadJavaClass[\"java.lang.System\"] "]
	,
	{Lint["LoadJavaClassSystem", _, _, _]}
	,
	TestID->"AbstractRules-20190523-L5T8B9"
]






(*
SuspiciousPrivateContext
*)

sample = FileNameJoin[{DirectoryName[$CurrentTestSource], "sample.wl"}]



TestMatch[
	LintFile[sample]
	,
	{Lint["SuspiciousPrivateContext", _, _, _]}
	,
	TestID->"AbstractRules-20190523-U0K0P7"
]







