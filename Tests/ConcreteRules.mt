
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
	CodeInspect["{ a \n . b }"]
	,
	{ InspectionObject["DifferentLine", _, _, _] }
	,
	TestID->"ConcreteRules-20190522-U4K0M9"
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
	{InspectionObject["DifferentLine", _, _, _]}
	,
	TestID->"ConcreteRules-20190521-Y8O9L2"
]

TestMatch[
	CodeInspect["{ a\n! }"]
	,
	{InspectionObject["DifferentLine", _, _, _]}
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
	{InspectionObject["DifferentLine", _, _, _]}
	,
	TestID->"ConcreteRules-20190521-E0L3O1"
]

TestMatch[
	CodeInspect["{ a~\nf~b } "]
	,
	{InspectionObject["DifferentLine", _, _, _]}
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
	{InspectionObject["DifferentLine", _, _, _]}
	,
	TestID->"ConcreteRules-20191212-S0K7T1"
]



(*
ImplicitTimesSpan
*)

TestMatch[
	CodeInspect[";;b;;"]
	,
	{ InspectionObject["ImplicitTimesSpan", _, _, _] }
	,
	TestID->"ConcreteRules-20190523-I1D9N0"
]


TestMatch[
	CodeInspect["a;;b;;"]
	,
	{ InspectionObject["ImplicitTimesSpan", _, _, _] }
	,
	TestID->"ConcreteRules-20190523-L7M6K3"
]



TestMatch[
	CodeInspect["a_..b"]
	,
	{ InspectionObject["UnexpectedDot", _, _, _], InspectionObject["BackwardsCompatibility", _, _, _] }
	,
	TestID->"ConcreteRules-20200821-G8T2K3"
]

TestMatch[
	CodeInspect["a_..."]
	,
	{ InspectionObject["UnexpectedDot", _, _, _], InspectionObject["BackwardsCompatibility", _, _, _] }
	,
	TestID->"ConcreteRules-20200821-R7H2A4"
]






TestMatch[
	CodeInspect["1.2`-3"]
	,
	{ InspectionObject["UnexpectedSign", _, _, _]}
	,
	TestID->"ConcreteRules-20220316-Z1O7X6"
]

TestMatch[
	CodeInspect["1.2`-.3"]
	,
	{ InspectionObject["UnexpectedSign", _, _, _]}
	,
	TestID->"ConcreteRules-20220316-T6F9N5"
]










