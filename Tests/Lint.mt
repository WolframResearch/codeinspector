
Needs["Lint`"]

Test[
	LintString["Which[a==b,1,a==b,2]"]
	,
	{Lint["DuplicateClauses", "Duplicate clauses in Which: a==b", "Error", <|AST`Source -> {{1, 7}, {1, 10}}|>],
	Lint["DuplicateClauses", "Duplicate clauses in Which: a==b", "Error", <|AST`Source -> {{1, 14}, {1, 17}}|>]}
]
