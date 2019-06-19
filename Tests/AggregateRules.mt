
Needs["Lint`"]

(*
ImplicitTimesAcrossLines
*)
TestMatch[
	LintString["{ a\nb }"]
	,
	{Lint["ImplicitTimesAcrossLines", _, _, _]}
	,
	TestID->"AggregateRules-20190522-D9Q1R2"
]

(*
ImplicitTimesBlanks
*)
TestMatch[
	LintString["{ ____ }"]
	,
	{Lint["ContiguousImplicitTimesBlanks", _, _, _]}
	,
	TestID->"AggregateRules-20190522-M3F3T8"
]


(*
DotDifferentLine
*)
TestMatch[
	LintString["{ a.\nb }"]
	,
	{Lint["DotDifferentLine", _, _, _]}
	,
	TestID->"AggregateRules-20190522-U4K0M9"
]


(*
SuspiciousSpan
*)
TestMatch[
	LintString[" a;; "]
	,
	{Lint["SuspiciousSpan", _, _, _]}
	,
	TestID->"AggregateRules-20190522-S9S6W2"
]

TestMatch[
	LintString["{ a;;\nb }"]
	,
	{ Lint["EndOfLine", _, "Warning", _], Lint["DifferentLine", _, "Warning", _] }
	,
	TestID->"AggregateRules-20190522-S2L6J4"
]





(*
StraySemicolon
*)
TestMatch[
	LintString["f[ a;b; ;x ]"]
	,
	{ Lint["StraySemicolon", _, "Warning", _] }
	,
	TestID->"AggregateRules-20190630-H1H8N7"
]


(*
DifferentLine
*)
TestMatch[
	LintString["(f[]
; Throw[$Failed, $tag])"]
	,
	{ Lint["DifferentLine", _, "Warning", _] }
	,
	TestID->"AggregateRules-20190522-I8L1E6"
]




(*
SuspiciousOut
*)
TestMatch[
	LintString["{ % }"]
	,
	{Lint["SuspiciousOut", _, _, _], Lint["SessionSymbol", _, _, _]}
	,
	TestID->"AggregateRules-20190522-O5O0L9"
]




(*
SuspiciousPatternTestCall
*)
TestMatch[
	LintString["a_?b[x]"]
	,
	{Lint["SuspiciousPatternTestCall", _, _, _]}
	,
	TestID->"AggregateRules-20190522-X8Z0P3"
]





(*
AssociationCall
*)
TestMatch[
	LintString["a?Association"]
	,
	{Lint["AssociationCall", _, _, _]}
	,
	TestID->"AggregateRules-20190522-Y0P7H3"
]

(*
StringCall
*)
TestMatch[
	LintString["a?String"]
	,
	{Lint["StringCall", _, _, _]}
	,
	TestID->"AggregateRules-20190522-N1P9K9"
]

(*
IntegerCall
*)
TestMatch[
	LintString["a?Integer"]
	,
	{Lint["IntegerCall", _, _, _]}
	,
	TestID->"AggregateRules-20190522-I2E9A2"
]

(*
RealCall
*)
TestMatch[
	LintString["a?Real"]
	,
	{Lint["RealCall", _, _, _]}
	,
	TestID->"AggregateRules-20190522-S0H6K7"
]

(*
FailureCall
*)
TestMatch[
	LintString["a?Failure"]
	,
	{Lint["FailureCall", _, _, _]}
	,
	TestID->"AggregateRules-20190522-C8C7F4"
]




(*
SuspiciousRuleFunction
*)

TestMatch[
	LintString[" a->b& "]
	,
	{Lint["SuspiciousRuleFunction", _, _, _]}
	,
	TestID->"AggregateRules-20190522-G2J4D6"
]

TestMatch[
	LintString["(a->b&)"]
	,
	{}
	,
	TestID->"AggregateRules-20190522-N7H5I3"
]

TestMatch[
	LintString["a->b& @ x"]
	,
	{}
	,
	TestID->"AggregateRules-20190522-Z0X7E6"
]


TestMatch[
	LintString["Map[a->b&, x]"]
	,
	{}
	,
	TestID->"AggregateRules-20190522-D9J5I0"
]

TestMatch[
	LintString["a->b&[x]"]
	,
	{}
	,
	TestID->"AggregateRules-20190522-U2S5F4"
]









(*
SuspiciousPatternTestFunction
*)
TestMatch[
	LintString[" a?b& "]
	,
	{Lint["SuspiciousPatternTestFunction", _, _, _]}
	,
	TestID->"AggregateRules-20190523-U6J6X9"
]




(*
SuspiciousPatternTestCallFunction
*)
TestMatch[
	LintString[" a?b[#]& "]
	,
	{Lint["SuspiciousPatternTestCallFunction", _, _, _]}
	,
	TestID->"AggregateRules-20190523-W2H4Z1"
]








(*
SuspiciousPatternBlankOptional
*)
TestMatch[
	LintString[" a_:_ "]
	,
	{Lint["SuspiciousPatternBlankOptional", _, _, _]}
	,
	TestID->"AggregateRules-20190523-T7Q3D0"
]


(*
example taken from:
https://bugs.wolfram.com/show?number=373945
*)
TestMatch[
	LintString["checkSystemID[sid_: ($platformPattern | Automatic)] := sid"]
	,
	{Lint["SuspiciousPatternBlankOptional", _, _, _]}
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
	LintString["{ + }"]
	,
	{Lint["SyntaxError", _, _, _]}
	,
	TestID->"AggregateRules-20190523-C2Y2X2"
]


(*
SyntaxError NonAssociative:
*)

TestMatch[
	LintString["a ? b ? c"]
	,
	{Lint["SyntaxError", _, _, _]}
	,
	TestID->"AggregateRules-20190523-W9O8V3"
]


(*
SyntaxError ExpectedTilde:
*)

TestMatch[
	LintString["a ~f"]
	,
	{Lint["SyntaxError", _, _, _]}
	,
	TestID->"AggregateRules-20190523-N4B7X7"
]



(*
SyntaxError ExpectedSymbol:
*)

TestMatch[
	LintString["1:2"]
	,
	{Lint["SyntaxError", _, _, _]}
	,
	TestID->"AggregateRules-20190523-F9Z1T6"
]

(*
SyntaxError ExpectedSet:
*)

TestMatch[
	LintString["a /: b * c"]
	,
	{Lint["SyntaxError", _, _, _]}
	,
	TestID->"AggregateRules-20190523-A6O0J4"
]

(*
SyntaxError ExpectedPossible:
*)

TestMatch[
	LintString["&"]
	,
	{Lint["SyntaxError", _, _, _]}
	,
	TestID->"AggregateRules-20190523-H9C7R8"
]













