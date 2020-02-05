
Needs["CodeInspector`"]



(*
ImplicitTimesAcrossLines
*)
TestMatch[
	CodeInspect["{ a\nb }"]
	,
	{ InspectionObject["ImplicitTimesAcrossLines", _, _, _] }
	,
	TestID->"ConcreteRules-20190522-D9Q1R2"
]



(*
DotDifferentLine
*)
TestMatch[
	CodeInspect["{ a.\nb }"]
	,
	{ InspectionObject["DotDifferentLine", _, _, _] }
	,
	TestID->"ConcreteRules-20190522-U4K0M9"
]




TestMatch[
	CodeInspect["{ a;;\nb }"]
	,
	{ InspectionObject["EndOfLine", _, "Warning", _], InspectionObject["SpanDifferentLine", _, "Warning", _] }
	,
	TestID->"ConcreteRules-20190522-S2L6J4"
]



(*
DifferentLine
*)
TestMatch[
	CodeInspect["(f[]\n; Throw[$Failed, $tag])"]
	,
	{ InspectionObject["DifferentLine", _, "Warning", _] }
	,
	TestID->"ConcreteRules-20190522-I8L1E6"
]


(*
DifferentLine:
*)

TestMatch[
	CodeInspect["-\na"]
	,
	{InspectionObject["PrefixDifferentLine", _, _, _]}
	,
	TestID->"ConcreteRules-20190521-Y8O9L2"
]

TestMatch[
	CodeInspect["{ a\n! }"]
	,
	{InspectionObject["PostfixDifferentLine", _, _, _]}
	,
	TestID->"ConcreteRules-20190521-R2X2T0"
]

(*

TODO: should we have "DifferentLine" warnings about ; ?

TestMatch[
	FirstCase[ConcreteParseString["{ a\n; }", HoldNode[Hold, #[[1]], <|SyntaxIssues -> #[[3]]|>] &],
		KeyValuePattern[SyntaxIssues -> _], $Failed, {0, Infinity}]
	,
	KeyValuePattern[SyntaxIssues -> {SyntaxIssue["DifferentLine", _, _, _]}]
	,
	TestID->"SyntaxIssues-20190521-M5K3M2"
]
*)

TestMatch[
	CodeInspect["{ a\n;; }"]
	,
	{InspectionObject["SpanDifferentLine", _, _, _]}
	,
	TestID->"ConcreteRules-20190521-E0L3O1"
]

TestMatch[
	CodeInspect["{ a~\nf~b } "]
	,
	{InspectionObject["TernaryTildeDifferentLine", _, _, _]}
	,
	TestID->"ConcreteRules-20190521-S5U4W8"
]


TestMatch[
	CodeInspect["{ a~f~\nb } "]
	,
	{}
	,
	TestID->"ConcreteRules-20191212-Y2G4G8"
]


TestMatch[
	CodeInspect["<<\na"]
	,
	{InspectionObject["PrefixDifferentLine", _, _, _]}
	,
	TestID->"ConcreteRules-20191212-S0K7T1"
]



(*
ImplicitTimesSpan
*)

TestMatch[
	CodeInspect[";;b;;"]
	,
	{ InspectionObject["UnexpectedImplicitTimes", _, _, _] }
	,
	TestID->"ConcreteRules-20190523-I1D9N0"
]


TestMatch[
	CodeInspect["a;;b;;"]
	,
	{ InspectionObject["UnexpectedImplicitTimes", _, _, _] }
	,
	TestID->"ConcreteRules-20190523-L7M6K3"
]


