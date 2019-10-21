BeginPackage["Lint`Utils`"]


expandLineNumberExclusions

severityColor

severityColorNewStyle

format

shadows

boldify

gridify

plainify


$characterReplacementRules


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]
Needs["Lint`"]
Needs["Lint`Format`"]


$shortenLimit = 100


$LintDescriptionLimit = 100




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


severityColorNewStyle[lints:{_Lint..}] :=
Module[{maxSeverity},
	maxSeverity = MaximalBy[lints[[All, 3]], severityToInteger][[1]];
	Switch[maxSeverity,
		"Formatting" | "Remark" | "ImplicitTimes", {
				RGBColor[{0/255, 118/255, 255/255}] (*primary icon color*), 
				RGBColor[{255/255, 255/255, 255/255}] (*secondary icon color*), 
				RGBColor[{203/255, 230/255, 255/255}] (*primary bar color*), 
				RGBColor[{5/255, 89/255, 218/255}] } (*secondary bar color*), 
		"Warning", {
				GrayLevel[118/255] (*primary icon color*), 
				GrayLevel[255/255] (*secondary icon color*), 
				GrayLevel[230/255] (*primary bar color*), 
				GrayLevel[89/255] } (*secondary bar color*), 
		"Error" | "Fatal", {
				RGBColor[{255/255, 118/255, 0/255}] (*primary icon color*), 
				RGBColor[{255/255, 255/255, 255/255}] (*secondary icon color*), 
				RGBColor[{255/255, 230/255, 203/255}] (*primary bar color*), 
				RGBColor[{218/255, 89/255, 5/255}] } (*secondary bar color*)
	]
]









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




(*
shadows[lint1, lint2] => True means lint is less severe and is contained by lint2; we will get rid of lint1

Get rid of Lints that have root causes that are also being reported
Only report the root cause
*)
shadows[lint1:Lint[lint1Tag_, _, lint1Severity__, lint1Data_], lint2:Lint[lint2Tag_, _, lint2Severity_, lint2Data_]] :=
	Which[
		lint1 === lint2,
			False,
		KeyExistsQ[lint1Data, CodeActions],
			False,
		!SourceMemberQ[lint2Data[ Source ], lint1Data[ Source ] ],
			False,
		MatchQ[lint1Tag, "UnhandledCharacter" | "AbstractSyntaxError"] && lint2Tag == "UnrecognizedCharacter",
			(*
			"UnrecognizedCharacter" is the "root" cause, so keep it and get rid of "UnhandledCharacter"
			*)
			True,
		lint1Tag == "UnrecognizedCharacter" && MatchQ[lint2Tag, "UnhandledCharacter" | "AbstractSyntaxError"],
			False,
		severityToInteger[lint1Severity] <= severityToInteger[lint2Severity],
			True,
		True,
			False
	]




distrib[s_String] := StringSplit[s, "\n"->"\n"]
distrib[StringExpression[args___]] := Flatten[distrib /@ {args}]
distrib[bold[s_String]] := LintBold /@ StringSplit[s, "\n"->"\n"]
distrib[preserve[s_String]] := LintPreserve[s]

(*
replace `` and ** markup
*)
boldify[s_String] :=
distrib[
	StringReplace[s, {
		RegularExpression["``(.*?)``"] :> bold["$1"],
		RegularExpression["\\*\\*(.*?)\\*\\*"] :> bold["$1"],
		RegularExpression["\\?\\?(.*?)\\?\\?"] :> preserve["$1"] }] ]

gridify[bolded_List] := Flatten[Partition[#, UpTo[$LintDescriptionLimit]]& /@ SequenceSplit[bolded, {"\n" | LintBold["\n", _]}], 1]

(*

do not send `` markup
do not send ** markup
do not send ?? markup
FIXME: what to do about ?? contents?
*)
plainify[s_String] := StringReplace[s, {
  RegularExpression["``(.*?)``"] :> "$1",
  RegularExpression["\\*\\*(.*?)\\*\\*"] :> "$1",
  RegularExpression["\\?\\?(.*?)\\?\\?"] :> "$1"}]







$characterReplacementRules = {
	"\.00" -> "\[UnknownGlyph]",
	"\.01" -> "\[UnknownGlyph]",
	"\.02" -> "\[UnknownGlyph]",
	"\.03" -> "\[UnknownGlyph]",
	"\.04" -> "\[UnknownGlyph]",
	"\.05" -> "\[UnknownGlyph]",
	"\.06" -> "\[UnknownGlyph]",
	"\.07" -> "\[UnknownGlyph]",
	"\.08" -> "\[UnknownGlyph]",
	(*
	We want everything to be 1 character wide.
	This keeps things simple
	*)
	"\t" -> " ",
	"\.0b" -> "\[UnknownGlyph]",
	"\.0c" -> "\[UnknownGlyph]",
	"\.0e" -> "\[UnknownGlyph]",
	"\.0f" -> "\[UnknownGlyph]",
	"\.10" -> "\[UnknownGlyph]",
	"\.11" -> "\[UnknownGlyph]",
	"\.12" -> "\[UnknownGlyph]",
	"\.13" -> "\[UnknownGlyph]",
	"\.14" -> "\[UnknownGlyph]",
	"\.15" -> "\[UnknownGlyph]",
	"\.16" -> "\[UnknownGlyph]",
	"\.17" -> "\[UnknownGlyph]",
	"\.18" -> "\[UnknownGlyph]",
	"\.19" -> "\[UnknownGlyph]",
	"\.1a" -> "\[UnknownGlyph]",
	"\.1b" -> "\[UnknownGlyph]",
	"\.1c" -> "\[UnknownGlyph]",
	"\.1d" -> "\[UnknownGlyph]",
	"\.1e" -> "\[UnknownGlyph]",
	"\.1f" -> "\[UnknownGlyph]",
	"\.7f" -> "\[UnknownGlyph]"
}








End[]

EndPackage[]
