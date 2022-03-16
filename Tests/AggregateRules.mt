
Needs["CodeInspector`"]

Needs["CodeParser`"]


(*
ImplicitTimesAcrossLines
*)
TestMatch[
	CodeInspect["{ a\nb }"]
	,
	{InspectionObject["ImplicitTimesAcrossLines", _, _, _]}
	,
	TestID->"AggregateRules-20190522-D9Q1R2"
]

(*
ImplicitTimesBlanks
*)
TestMatch[
	CodeInspect["{ ____ }"]
	,
	{InspectionObject["ImplicitTimesBlanks", _, _, _]}
	,
	TestID->"AggregateRules-20190522-M3F3T8"
]


(*
DotDifferentLine
*)
TestMatch[
	CodeInspect["{ a.\nb }"]
	,
	{InspectionObject["DotDifferentLine", _, _, _]}
	,
	TestID->"AggregateRules-20190522-U4K0M9"
]


(*
SuspiciousSpan
*)
TestMatch[
	CodeInspect[" a;; "]
	,
	{ InspectionObject["SuspiciousSpan", _, _, _] }
	,
	TestID->"AggregateRules-20190522-S9S6W2"
]


(*
StraySemicolon
*)
TestMatch[
	CodeInspect["f[ a;b; ;x ]"]
	,
	{ InspectionObject["UnexpectedSemicolon", _, "Warning", _] }
	,
	TestID->"AggregateRules-20190630-H1H8N7"
]


(*
DifferentLine
*)
TestMatch[
	CodeInspect["(f[]
; Throw[$Failed, $tag])"]
	,
	{ InspectionObject["DifferentLine", _, "Warning", _] }
	,
	TestID->"AggregateRules-20190522-I8L1E6"
]







(*
SuspiciousPatternTestCall
*)
TestMatch[
	CodeInspect["a_?b[x]"]
	,
	{InspectionObject["SuspiciousPatternTestCall", _, _, _]}
	,
	TestID->"AggregateRules-20190522-X8Z0P3"
]





(*
AssociationCall
*)
TestMatch[
	CodeInspect["a?Association"]
	,
	{InspectionObject["PatternTest", _, _, _], InspectionObject["AssociationCall", _, _, _]}
	,
	TestID->"AggregateRules-20190522-Y0P7H3"
]

(*
StringCall
*)
TestMatch[
	CodeInspect["a?String"]
	,
	{InspectionObject["PatternTest", _, _, _], InspectionObject["StringCall", _, _, _]}
	,
	TestID->"AggregateRules-20190522-N1P9K9"
]

(*
IntegerCall
*)
TestMatch[
	CodeInspect["a?Integer"]
	,
	{InspectionObject["PatternTest", _, _, _], InspectionObject["IntegerCall", _, _, _]}
	,
	TestID->"AggregateRules-20190522-I2E9A2"
]

(*
RealCall
*)
TestMatch[
	CodeInspect["a?Real"]
	,
	{InspectionObject["PatternTest", _, _, _], InspectionObject["RealCall", _, _, _]}
	,
	TestID->"AggregateRules-20190522-S0H6K7"
]

(*
FailureCall
*)
TestMatch[
	CodeInspect["a?Failure"]
	,
	{InspectionObject["PatternTest", _, _, _], InspectionObject["FailureCall", _, _, _]}
	,
	TestID->"AggregateRules-20190522-C8C7F4"
]




(*
SuspiciousRuleFunction
*)

TestMatch[
	CodeInspect[" a->b& "]
	,
	{InspectionObject["SuspiciousRuleFunction", _, _, _]}
	,
	TestID->"AggregateRules-20190522-G2J4D6"
]

TestMatch[
	CodeInspect["(a->b&)"]
	,
	{}
	,
	TestID->"AggregateRules-20190522-N7H5I3"
]

TestMatch[
	CodeInspect["a->b& @ x"]
	,
	{}
	,
	TestID->"AggregateRules-20190522-Z0X7E6"
]


TestMatch[
	CodeInspect["Map[a->b&, x]"]
	,
	{}
	,
	TestID->"AggregateRules-20190522-D9J5I0"
]

TestMatch[
	CodeInspect["a->b&[x]"]
	,
	{}
	,
	TestID->"AggregateRules-20190522-U2S5F4"
]









(*
SuspiciousPatternTestFunction
*)
TestMatch[
	CodeInspect[" a?b& "]
	,
	{InspectionObject["PatternTest", _, _, _], InspectionObject["SuspiciousPatternTestFunction", _, _, _]}
	,
	TestID->"AggregateRules-20190523-U6J6X9"
]


TestMatch[
	CodeInspect["a_?b[x]"]
	,
	{InspectionObject["SuspiciousPatternTestCall", _, _, _]}
	,
	TestID->"AggregateRules-20210830-B4E6L0"
	
]


(*
SuspiciousPatternTestCallFunction
*)
TestMatch[
	CodeInspect[" a?b[#]& "]
	,
	{InspectionObject["PatternTest", _, _, _], InspectionObject["SuspiciousPatternTestCallFunction", _, _, _]}
	,
	TestID->"AggregateRules-20190523-W2H4Z1"
]


TestMatch[
	CodeInspect["a_?b[x]&"]
	,
	{InspectionObject["SuspiciousPatternTestCallFunction", _, _, _]}
	,
	TestID->"AggregateRules-20210830-G1O8L9"
]


TestMatch[
	CodeInspect["a_?Association"]
	,
	{InspectionObject["AssociationCall", _, _, _]}
	,
	TestID->"AggregateRules-20210830-M3N7M8"
]


TestMatch[
	CodeInspect["a_?b:c"]
	,
	{InspectionObject["PatternTestPattern", _, _, _]}
	,
	TestID->"AggregateRules-20210830-G1S6J6"
]





(*
SuspiciousPatternBlankOptional
*)
TestMatch[
	CodeInspect[" a_:_ "]
	,
	{InspectionObject["SuspiciousPatternBlankOptional", _, _, _]}
	,
	TestID->"AggregateRules-20190523-T7Q3D0"
]


(*
example taken from:
https://bugs.wolfram.com/show?number=373945
*)
TestMatch[
	CodeInspect["checkSystemID[sid_: ($platformPattern | Automatic)] := sid"]
	,
	{InspectionObject["SuspiciousPatternBlankOptional", _, _, _]}
	,
	TestID->"AggregateRules-20190523-B2O7A2"
]








(*
SyntaxError:

taken from SyntaxErrorNodes
*)


(*
SyntaxError ExpectedOperand:
*)

TestMatch[
	CodeInspect["{ + }"]
	,
	{
		InspectionObject["ExpectedOperand", _, "Fatal", _],
		InspectionObject["PrefixPlus", _, "Remark", _]}
	,
	TestID->"AggregateRules-20190523-C2Y2X2"
]

TestMatch[
	CodeInspect["{ * }"]
	,
	{
		InspectionObject["ExpectedOperand", _, "Fatal", _],
		InspectionObject["ExpectedOperand", _, "Fatal", _]}
	,
	TestID->"AggregateRules-20191212-O0N6L9"
]



(*
SyntaxError NonAssociative:
*)

TestMatch[
	CodeInspect["a ? b ? c"]
	,
	{InspectionObject["NonAssociativePatternTest", _, _, _], InspectionObject["PatternTest", _, _, _], InspectionObject["PatternTest", _, _, _]}
	,
	TestID->"AggregateRules-20190523-W9O8V3"
]


(*
SyntaxError ExpectedTilde:
*)

TestMatch[
	CodeInspect["a ~f"]
	,
	{InspectionObject["ExpectedTilde", _, _, _]}
	,
	TestID->"AggregateRules-20190523-N4B7X7"
]



(*
SyntaxError ExpectedSymbol:
*)

TestMatch[
	CodeInspect["1:2"]
	,
	{InspectionObject["PatternColonError", _, _, _]}
	,
	TestID->"AggregateRules-20190523-F9Z1T6"
]

(*
SyntaxError ExpectedSet:
*)

TestMatch[
	CodeInspect["a /: b * c"]
	,
	{InspectionObject["ExpectedSet", _, _, _]}
	,
	TestID->"AggregateRules-20190523-A6O0J4"
]

(*
SyntaxError ExpectedPossible:
*)

TestMatch[
	CodeInspect["&"]
	,
	{InspectionObject["ExpectedOperand", _, _, _]}
	,
	TestID->"AggregateRules-20190523-H9C7R8"
]




TestMatch[
	CodeInspect["8 Pi*\"M\""]
	,
	{}
	,
	TestID->"AggregateRules-20191121-L0N6E3"
]


TestMatch[
	CodeInspect["\
DocLinter`DocNotebookLint[dir_String?DirectoryQ, opts_:OptionsPattern[]] := Module[{nbs},
	nbs = FileNames[\"*.nb\", dir, Infinity];
	DocLinter`DocNotebookLint[nbs, opts]
]"]
	,
	{InspectionObject["SuspiciousPatternBlankOptional", _, _, _]}
	,
	TestID->"AggregateRules-20220302-Z0S4S7"
]
	







TestMatch[
	CodeInspect["Function[{x, y}, x + y]&[1,2]"]
	,
	{InspectionObject["FunctionAmp", _, _, _]}
	,
	TestID->"AggregateRules-20220307-V3U4J1"
]



TestMatch[
	CodeInspect["\"a\" <> _"]
	,
	{InspectionObject["PatternStringJoin", _, "Error", _]}
	,
	TestID->"AggregateRules-20220310-Q4H6M5"
]



Test[
	CodeInspect["replaceheads_Alternatives : Alternatives[]"]
	,
	{InspectionObject["SuspiciousPatternBlankOptional", "Suspicious use of ``_Alternatives``.", "Warning", <|
		Source -> {{1, 1}, {1, 43}},
		ConfidenceLevel -> 0.85,
		"AdditionalDescriptions" -> {"This may be ok if ``replaceheads`` is used as a pattern."},
		CodeActions -> {
			CodeAction["Replace with ``replaceheads : Alternatives[]``", ReplaceNode, <|
				"ReplacementNode" ->
					BinaryNode[Pattern, {
						LeafNode[Symbol, "replaceheads", <|Source -> {{1, 1}, {1, 13}}|>],
						LeafNode[Token`Colon, ":", <||>],
						CallNode[LeafNode[Symbol, "Alternatives", <|Source -> {{1, 29}, {1, 41}}|>], {
							GroupNode[GroupSquare, {
								LeafNode[Token`OpenSquare, "[", <|Source -> {{1, 41}, {1, 42}}|>],
								LeafNode[Token`CloseSquare, "]", <|Source -> {{1, 42}, {1, 43}}|>]}, <|Source -> {{1, 41}, {1, 43}}|>]}, <|Source -> {{1, 29}, {1, 43}}|>]}, <||>], Source -> {{1, 1}, {1, 43}}|>]}|>]}
	,
	TestID->"AggregateRules-20220311-K6D5T9"
]



(*
inspired by bug 421310
*)

TestMatch[
	CodeInspect["audioData = Round[audioData**(2.^15-1)]"]
	,
	{InspectionObject["NonCommutativeMultiply", _, "Error", KeyValuePattern[ {
		CodeActions -> {
			CodeAction["Replace with ``*``", ReplaceNode, _],
			CodeAction["Replace with ``^``", ReplaceNode, _]} } ]]}
	,
	TestID->"AggregateRules-20220316-K3D9D9"
]




(*
inspired by bug 421311
*)
TestMatch[
	CodeInspect["masking:_Image | All:_:All"]
	,
	{InspectionObject["NestedOptionals", _, "Error", _]}
	,
	TestID->"AggregateRules-20220316-I9J0H0"
]



(*
inspired by bug 421309
*)
TestMatch[
	CodeInspect["r_?cQ:1"]
	,
	{InspectionObject["PatternTestPattern", _, "Error", KeyValuePattern[ {
		CodeActions -> {
			CodeAction["Insert ``:``", ReplaceNode, _],
			CodeAction["Replace with ``Optional``", ReplaceNode, _]} } ]]}
	,
	TestID->"AggregateRules-20220316-H3X3G7"
]








