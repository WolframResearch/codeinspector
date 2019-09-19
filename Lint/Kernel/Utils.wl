BeginPackage["Lint`Utils`"]


expandLineNumberExclusions

severityColor

format

shadows


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]
Needs["Lint`"]


(*

a = <|1 | 2 -> {"foo"}, 2 | 3 -> {"bar", "baz"}|>

expandLineNumberExclusions[a] =>
  <|1 -> {"foo"}, 2 -> {"foo", "bar", "baz"}, 3 -> {"bar", "baz"}|>

*)
expandLineNumberExclusions[lineNumberExclusions_Association] :=
  Merge[KeyValueMap[Function[{key, value},
    Association[(# -> value)& /@ (If[IntegerQ[key], List @ key, List @@ key])]], lineNumberExclusions], Catenate]





severityToInteger["ImplicitTimes" | "Formatting"] = 0
severityToInteger["Remark"] = 1
severityToInteger["Warning"] = 2
severityToInteger["Error"] = 3
severityToInteger["Fatal"] = 4

(*
return the highest severity from list of Lints
*)
severityColor[lints:{_Lint..}] :=
Module[{maxSeverity},
	maxSeverity = MaximalBy[lints[[All, 3]], severityToInteger][[1]];
	Switch[maxSeverity,
		"Formatting" | "Remark" | "ImplicitTimes", Blue,
		"Warning", Darker[Orange],
		"Error" | "Fatal", Red
	]
]



$shortenLimit = 100

shorten[s_String] /; StringLength[s] < $shortenLimit := s

shorten[s_String] :=
Module[{toElide, part},
	toElide = StringLength[s] - $shortenLimit;
	part = Floor[StringLength[s]/2] + Floor[toElide/2] * {-1, 1};
	StringReplacePart[s, "\[Ellipsis]", part]
]



format[s_String] :=
Module[{containsDoubleTicks, containsDoubleStars},

	containsDoubleTicks = StringContainsQ[s, "``"];
	containsDoubleStars = StringContainsQ[s, "**"];

	Which[
		containsDoubleTicks && containsDoubleStars, Message[Lint::cannotFormat, s];shorten[s],
		containsDoubleTicks, "**" <> shorten[s] <> "**",
		True, "``" <> shorten[s] <> "``"
	]
]





shadows[lint1_Lint, lint2_Lint] :=
	(severityToInteger[lint1["Severity"]] <= severityToInteger[lint2["Severity"]] &&
		SourceMemberQ[lint2[[4, Key[Source]]], lint1[[4, Key[Source]]]]) &&
   	(lint1 =!= lint2)


End[]

EndPackage[]
