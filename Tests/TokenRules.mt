(* Wolfram Language Test file *)

(*
These tests are for when there is a TokenRules mechanism
*)


BeginTestSection["TokenRules", False]

TestMatch[
	CodeInspect["{ a;;\nb }"]
	,
	{ InspectionObject["EndOfLine", _, "Warning", _], InspectionObject["SpanDifferentLine", _, "Warning", _] }
	,
	TestID->"TokenRules-20190522-S2L6J4"
]

EndTestSection[]
