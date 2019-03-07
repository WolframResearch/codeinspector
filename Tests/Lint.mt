
Needs["AST`"]
Needs["Lint`"]

TestMatch[
	LintString["Which[a==b,1,a==b,2]"]
	,
	{Lint["DuplicateClauses", _, _, _], Lint["DuplicateClauses", _, _, _]}
	,
	TestID->"Lint-20181215-L4E3M5"
]

TestMatch[
	LintString["1+f[,2]"]
	,
	{Lint["SyntaxError", _, _, _]}
	,
	TestID->"Lint-20190306-V4B4W1"
]
