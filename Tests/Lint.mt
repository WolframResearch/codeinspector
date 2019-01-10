
Needs["AST`"]
Needs["Lint`"]

TestMatch[
	lints = LintString["Which[a==b,1,a==b,2]"];
	lints
	,
	{Lint["DuplicateClauses", _, _, _], Lint["DuplicateClauses", _, _, _]}
	,
	TestID->"Lint-20181215-L4E3M5"
]
