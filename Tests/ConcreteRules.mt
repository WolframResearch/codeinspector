
Needs["Lint`"]



(*
ImplicitTimesAcrossLines
*)
TestMatch[
	LintString["{ a\nb }"]
	,
	{ Lint["ImplicitTimesAcrossLines", _, _, _], Lint["TopLevel", _, _, _] }
	,
	TestID->"ConcreteRules-20190522-D9Q1R2"
]



(*
DotDifferentLine
*)
TestMatch[
	LintString["{ a.\nb }"]
	,
	{ Lint["DotDifferentLine", _, _, _], Lint["TopLevel", _, _, _] }
	,
	TestID->"ConcreteRules-20190522-U4K0M9"
]




TestMatch[
	LintString["{ a;;\nb }"]
	,
	{ Lint["EndOfLine", _, "Warning", _], Lint["SpanDifferentLine", _, "Warning", _], Lint["TopLevel", _, _, _] }
	,
	TestID->"ConcreteRules-20190522-S2L6J4"
]



(*
DifferentLine
*)
TestMatch[
	LintString["(f[]\n; Throw[$Failed, $tag])"]
	,
	{ Lint["DifferentLine", _, "Warning", _] }
	,
	TestID->"ConcreteRules-20190522-I8L1E6"
]


(*
DifferentLine:
*)

TestMatch[
	LintString["-\na"]
	,
	{Lint["PrefixDifferentLine", _, _, _], Lint["TopLevel", _, _, _]}
	,
	TestID->"ConcreteRules-20190521-Y8O9L2"
]

TestMatch[
	LintString["{ a\n! }"]
	,
	{Lint["PostfixDifferentLine", _, _, _], Lint["TopLevel", _, _, _]}
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
	LintString["{ a\n;; }"]
	,
	{Lint["SpanDifferentLine", _, _, _], Lint["TopLevel", _, _, _]}
	,
	TestID->"ConcreteRules-20190521-E0L3O1"
]

TestMatch[
	LintString["{ a~\nf~b } "]
	,
	{Lint["TernaryTildeDifferentLine", _, _, _], Lint["TopLevel", _, _, _]}
	,
	TestID->"ConcreteRules-20190521-S5U4W8"
]


TestMatch[
	LintString["{ a~f~\nb } "]
	,
	{Lint["TernaryTildeDifferentLine", _, _, _], Lint["TopLevel", _, _, _]}
	,
	TestID->"ConcreteRules-20191212-Y2G4G8"
]


TestMatch[
	LintString["<<\na"]
	,
	{Lint["PrefixDifferentLine", _, _, _]}
	,
	TestID->"ConcreteRules-20191212-S0K7T1"
]



(*
ImplicitTimesSpan
*)

TestMatch[
	LintString[";;b;;"]
	,
	{Lint["ImplicitTimes", _, _, _], Lint["EndOfLine", _, _, _]}
	,
	TestID->"ConcreteRules-20190523-I1D9N0"
]


TestMatch[
	LintString["a;;b;;"]
	,
	{Lint["ImplicitTimes", _, _, _], Lint["EndOfLine", _, _, _]}
	,
	TestID->"ConcreteRules-20190523-L7M6K3"
]


