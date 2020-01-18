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


uppercaseSymbolNameQ


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
shadows[lint1, lint2] => True means lint1 is less severe and is contained by lint2; we will get rid of lint1

Get rid of Lints that have root causes that are also being reported
Only report the root cause
*)
shadows[lint1:Lint[lint1Tag_, _, lint1Severity__, lint1Data_], lint2:Lint[lint2Tag_, _, lint2Severity_, lint2Data_]] :=
	Which[
		lint1 === lint2,
			False,
		KeyExistsQ[lint1Data, CodeActions],
			False,
		!SourceMemberQ[lint2Data[Source], lint1Data[Source]],
			False,
		MatchQ[lint1Tag, "UnhandledCharacter" | "AbstractSyntaxError"] && lint2Tag == "UnrecognizedCharacter",
			(*
			"UnrecognizedCharacter" is the "root" cause, so keep it and get rid of "UnhandledCharacter"
			*)
			True,
		lint1Tag == "UnrecognizedCharacter" && MatchQ[lint2Tag, "UnhandledCharacter" | "AbstractSyntaxError"],
			False,
		lint1Tag == "UnexpectedCharacter" && lint2Tag == "CharacterEncoding",
			(*
			"CharacterEncoding" is the "root" cause, so keep it and get rid of "UnexpectedCharacter"
			*)
			True,
		lint1Tag == "CharacterEncoding" && lint2Tag == "UnexpectedCharacter",
			False,
		severityToInteger[lint1Severity] < severityToInteger[lint2Severity],
			True,
		lint1Data[ConfidenceLevel] < lint2Data[ConfidenceLevel],
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






BeginStaticAnalysisIgnore[]

(*
Replace invisible characters with \[UnknownGlyph]
*)
$invisibleCharacters = {

	(*
	ASCII control characters
	*)
	"\.00",
	"\.01",
	"\.02",
	"\.03",
	"\.04",
	"\.05",
	"\.06",
	"\.07",
	"\.08",
	(*\t*)
	(*\n*)
	"\.0b",
	"\.0c",
	(*\r*)
	"\.0e",
	"\.0f",
	"\.10",
	"\.11",
	"\.12",
	"\.13",
	"\.14",
	"\.15",
	"\.16",
	"\.17",
	"\.18",
	"\.19",
	"\.1a",
	"\.1b",
	"\.1c",
	"\.1d",
	"\.1e",
	"\.1f",
	"\.7f",

	(*
	Virtual BOM character
	*)
	"\:e001"} ~Join~

	(*
	Unicode non-characters
	https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Non-characters
	*)
	CharacterRange["\:fdd0", "\:fdef"] ~Join~ {
	(*
	Unicode non-characters
	https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Non-characters
	*)
	"\|00fffe",
	"\|00ffff",
	"\|01fffe",
	"\|01ffff",
	"\|02fffe",
	"\|02ffff",
	"\|03fffe",
	"\|03ffff",
	"\|04fffe",
	"\|04ffff",
	"\|05fffe",
	"\|05ffff",
	"\|06fffe",
	"\|06ffff",
	"\|07fffe",
	"\|07ffff",
	"\|08fffe",
	"\|08ffff",
	"\|09fffe",
	"\|09ffff",
	"\|0afffe",
	"\|0affff",
	"\|0bfffe",
	"\|0bffff",
	"\|0cfffe",
	"\|0cffff",
	"\|0dfffe",
	"\|0dffff",
	"\|0efffe",
	"\|0effff",
	"\|0ffffe",
	"\|0fffff",
	"\|10fffe",
	"\|10ffff",

	(*
	ZERO WIDTH SPACE
  *)
  "\:200b",

  (*
  ZERO WIDTH NON-JOINER
  *)
  "\:200c",

  (*
  ZERO WIDTH JOINER
  *)
  "\:200d",

  (*
  LINE SEPARATOR
  *)
  "\:2028",

  (*
  WORD JOINER
  *)
  "\:2060",

  (*
  FUNCTION APPLICATION
  *)
  "\:2061"} ~Join~

  (*
  C1
  *)
  CharacterRange["\:0080", "\:009f"] ~Join~

  (*
  Plane 15 PUA
  *)
  CharacterRange["\|0f0000", "\|0ffffd"] ~Join~

  (*
  Plane 16 PUA
  *)
  CharacterRange["\|100000", "\|10fffd"]

EndStaticAnalysisIgnore[]


$characterReplacementRules = { 
	(*
	We want everything to be 1 character wide.
	This keeps things simple
	*)
	"\t" -> " ",
	Alternatives @@ $invisibleCharacters -> "\[UnknownGlyph]"
}



uppercaseSymbolNameQ[name_] := UpperCaseQ[StringPart[Last[StringSplit[name, "`"]], 1]]





End[]

EndPackage[]
