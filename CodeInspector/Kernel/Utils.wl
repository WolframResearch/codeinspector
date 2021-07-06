BeginPackage["CodeInspector`Utils`"]


expandLineNumberExclusions

severityToInteger

severityColor

severityColorNewStyle

format

shadows

boldify

gridify

plainify


$characterReplacementRules


uppercaseSymbolNameQ


isFirstError



firstTokenWithSource



lexOrderingForLists



scopingDataObjectToLints


betterRiffle


filterLints


conventionAgnosticSourceOrdering


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Scoping`"] (* for scopingDataObject *)
Needs["CodeParser`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`SuppressedRegions`"] (* for suppressedRegion *)


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





severityToInteger["ImplicitTimes" | "Formatting" | "Scoping"] = 0
severityToInteger["Remark"] = 1
severityToInteger["Warning"] = 2
severityToInteger["Error"] = 3
severityToInteger["Fatal"] = 4

(*
return the highest severity from list of Lints
*)
severityColor[lints:{_InspectionObject..}] :=
Module[{maxSeverity},
	maxSeverity = MaximalBy[lints[[All, 3]], severityToInteger][[1]];
	Switch[maxSeverity,
		"Formatting" | "Remark" | "ImplicitTimes" | "Scoping", Blue,
		"Warning", Darker[Orange],
		"Error" | "Fatal", Red
	]
]


severityColorNewStyle[lints:{_InspectionObject..}] :=
Module[{maxSeverity},
	maxSeverity = MaximalBy[lints[[All, 3]], severityToInteger][[1]];
	severityColorNewStyle[maxSeverity]
]

severityColorNewStyle[maxSeverity_] :=
	Switch[maxSeverity,
		"Formatting" | "Remark" | "ImplicitTimes" | "Scoping", {
				RGBColor[0/255, 118/255, 255/255] (*primary icon color*), 
				RGBColor[255/255, 255/255, 255/255] (*secondary icon color*), 
				RGBColor[203/255, 230/255, 255/255] (*primary bar color*), 
				RGBColor[5/255, 89/255, 218/255] } (*secondary bar color*), 
		"Warning", {
				GrayLevel[118/255] (*primary icon color*), 
				GrayLevel[255/255] (*secondary icon color*), 
				GrayLevel[230/255] (*primary bar color*), 
				GrayLevel[89/255] } (*secondary bar color*), 
		"Error" | "Fatal", {
				RGBColor[255/255, 118/255, 0/255] (*primary icon color*), 
				RGBColor[255/255, 255/255, 255/255] (*secondary icon color*), 
				RGBColor[255/255, 230/255, 203/255] (*primary bar color*), 
				RGBColor[218/255, 89/255, 5/255] } (*secondary bar color*)
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
		containsDoubleTicks && containsDoubleStars, Message[InspectionObject::cannotFormat, s];shorten[s],
		containsDoubleTicks, "**" <> shorten[s] <> "**",
		True, "``" <> shorten[s] <> "``"
	]
]




(*
shadows[lint1, lint2] => True means lint1 is less severe and is contained by lint2; we will get rid of lint1

Get rid of Lints that have root causes that are also being reported
Only report the root cause
*)
shadows[lint1:InspectionObject[lint1Tag_, _, lint1Severity__, lint1Data_], lint2:InspectionObject[lint2Tag_, _, lint2Severity_, lint2Data_]] :=
	Which[
		lint1 === lint2,
			False
		,
		KeyExistsQ[lint1Data, CodeActions] &&
			(!KeyExistsQ[lint2Data, CodeActions] || lint1Data[CodeActions] =!= lint2Data[CodeActions]),
			(*
			If lint1 has CodeActions and lint2 does not, then keep lint1
			If they both have CodeActions, then only keep lint1 if the actions are different
			*)
			False
		,
		!SourceMemberQ[lint2Data[Source], lint1Data[Source]],
			False
		,
		MatchQ[lint1Tag, "UnhandledCharacter" | "AbstractSyntaxError"] && lint2Tag == "UnrecognizedCharacter",
			(*
			"UnrecognizedCharacter" is the "root" cause, so keep it and get rid of "UnhandledCharacter"
			*)
			True
		,
		lint1Tag == "UnrecognizedCharacter" && MatchQ[lint2Tag, "UnhandledCharacter" | "AbstractSyntaxError"],
			False
		,
		lint1Tag == "UnexpectedCharacter" && lint2Tag == "CharacterEncoding",
			(*
			"CharacterEncoding" is the "root" cause, so keep it and get rid of "UnexpectedCharacter"
			*)
			True
		,
		lint1Tag == "CharacterEncoding" && lint2Tag == "UnexpectedCharacter",
			False
		,
		severityToInteger[lint1Severity] < severityToInteger[lint2Severity],
			True
		,
		severityToInteger[lint1Severity] == severityToInteger[lint2Severity] &&
			lint1Data[ConfidenceLevel] <= lint2Data[ConfidenceLevel],
			(*
			Given all of this:
			lint1 is SourceMemberQ of lint2
			same severity
			same confidence
			then get rid of lint1
			*)
			True
		,
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
		(*
		there may be \n inside of `` group, so turn on (?s) "single line mode" to allow . to match newlines

		FIXME: probably there should be some sanitization of characters in `` groups
		*)
		RegularExpression["(?s)``(.*?)``"] :> bold["$1"],
		RegularExpression["(?s)\\*\\*(.*?)\\*\\*"] :> bold["$1"],
		RegularExpression["(?s)\\?\\?(.*?)\\?\\?"] :> preserve["$1"] }]]

gridify[bolded_List] := Flatten[Partition[#, UpTo[$LintDescriptionLimit]]& /@ SequenceSplit[bolded, {"\n" | LintBold["\n", _]}], 1]

(*

do not send `` markup
do not send ** markup
do not send ?? markup
FIXME: what to do about ?? contents?
*)
plainify[s_String] := StringReplace[s, {
	RegularExpression["(?s)``(.*?)``"] :> "$1",
	RegularExpression["(?s)\\*\\*(.*?)\\*\\*"] :> "$1",
	RegularExpression["(?s)\\?\\?(.*?)\\?\\?"] :> "$1"}]







(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::UnexpectedCharacter:: *)

(*
Replace invisible characters with \[UnknownGlyph]
*)
$invisibleBMPCharacters = {

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
	"\b",
	(*\t*)
	(*\n*)
	"\.0b",
	"\f",
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
	"\[RawEscape]",
	"\.1c",
	"\.1d",
	"\.1e",
	"\.1f",

	(*
	DEL
	*)
	"\.7f",

	(*
	COMBINING GRAPHEME JOINER
	*)
	"\:034F",

	(*
	Ogham Space Mark
	*)
	"\:1680",

	(*
	Mongolian Vowel Separator
	*)
	"\:180E",

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
	LEFT-TO-RIGHT-MARK
	*)
	"\:200e",
	
	"\[NoBreak]",

	(*
	FUNCTION APPLICATION
	*)
	"\:2061",

	"\[InvisibleTimes]",

	(*
	INVISIBLE SEPARATOR
	*)
	"\:2063",

	(*
	INVISIBLE PLUS
	*)
	"\:2064",

	(*
	Virtual BOM character
	*)
	"\:e001",

	"\[InvisibleSpace]",
	"\[InvisibleApplication]",
	"\[InvisibleComma]",
	"\[ImplicitPlus]",
	"\[InvisiblePrefixScriptBase]",
	"\[InvisiblePostfixScriptBase]",

	"\[NegativeVeryThinSpace]",
	"\[NegativeThinSpace]",
	"\[NegativeMediumSpace]",
	"\[NegativeThickSpace]",

	(*
	COMPATIBILITYNoBreak
	*)
	16^^f3a2,

	"\[AutoSpace]",

	"\[PageBreakAbove]",
	"\[PageBreakBelow]",
	"\[DiscretionaryPageBreakAbove]",
	"\[DiscretionaryPageBreakBelow]",

	"\[AlignmentMarker]",

	(*
	ZERO WIDTH NO-BREAK SPACE
	*)
	"\:feff"

} ~Join~

	(*
	C1
	*)
	CharacterRange["\.80", "\.9f"] ~Join~

	(*
	Unicode non-characters
	https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Non-characters
	*)
	CharacterRange["\:fdd0", "\:fdef"] ~Join~
	{"\:fffe", "\:ffff"}


(*
Handling of \|XXXXXX notation did not become correct until version 12.0

e.g., in version 12:
StringLength["\|10ffff"] => 1

in version 11:
StringLength["\|10ffff"] => 8 (or 2 if coming from FE!)

*)
If[$VersionNumber >= 12.0,

	$invisibleNonBMPCharacters = {
		(*
		Unicode non-characters
		https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Non-characters
		*)
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
		"\|10ffff"
	}
	,
	(*
	$VersionNumber < 12.0
	*)
	$invisibleNonBMPCharacters = {}
]


$invisibleCharacters = $invisibleBMPCharacters ~Join~ $invisibleNonBMPCharacters

$characterReplacementRules = {
	Alternatives @@ $invisibleCharacters -> "\[UnknownGlyph]"
}

(* :!CodeAnalysis::EndBlock:: *)


uppercaseSymbolNameQ[name_] := UpperCaseQ[StringPart[Last[StringSplit[name, "`"]], 1]]



isFirstError[lints:{_InspectionObject..}, lineNumber_Integer, column_Integer] :=
Module[{any},
	any = MemberQ[First /@ {#[[4, Key[Source]]]} ~Join~ Lookup[#[[4]], "AdditionalSources", {}], {lineNumber, column}]& /@ lints;
	Or @@ any
]



firstTokenWithSource[node_] :=
	Catch[
	Module[{cases},
		cases = Cases[node, (LeafNode | ErrorNode)[_, _, KeyValuePattern[Source -> _]], {0, Infinity}];
		If[cases != {},
			Throw[First[SortBy[cases, #[[3, Key[Source]]]&, conventionAgnosticSourceOrdering]]]
		];
		cases = Cases[node, CallNode[_, _, KeyValuePattern[Source -> _]], {0, Infinity}];
		If[cases != {},
			Throw[First[SortBy[cases, #[[3, Key[Source]]]&, conventionAgnosticSourceOrdering]]]
		];
	]]



(*
order deepest first

given the srcs {{1, 3}, {1, 3, 1, 1}}

it is important to process {1, 3, 1, 1} first because adding the StyleBox changes the shape of box, so must work from more-specific to less-specific positions

For example, the boxes of this expression:
f[%[[]]]

which are:
RowBox[{"f", "[", RowBox[{"%", "[", RowBox[{"[", "]"}], "]"}], "]"}]

give lints with positions {1, 3} and {1, 3, 1, 1}
*)
positionSpecOrdering[{}, {}] := 0
positionSpecOrdering[{}, b_List] := -1
positionSpecOrdering[a_List, {}] := 1
positionSpecOrdering[a_List, b_List] :=
	Order[Take[a, 1], Take[b, 1]] /.
		0 :> positionSpecOrdering[Drop[a, 1], Drop[b, 1]]

(*
Position-spec (or possibly SourceCharacterIndex)

If this is SourceCharacterIndex, then the deepest-first behavior does not matter since sources are always length 2
*)
conventionAgnosticSourceOrdering[a:{(_Integer | _Real)...}, b:{(_Integer | _Real)...}] :=
	positionSpecOrdering[a, b]

(*
Replace After[{1, 3, 2}] with {1, 3, 2.5} for easier processing
*)
conventionAgnosticSourceOrdering[a:After[{most___, last_}], b_] :=
	conventionAgnosticSourceOrdering[{most, last + 0.5}, b]

conventionAgnosticSourceOrdering[a_, b:After[{most___, last_}]] :=
	conventionAgnosticSourceOrdering[a, {most, last + 0.5}]


(*
Replace {1, 3, 2, Intra[4, 4]} with {1, 3, 2, 4} for easier processing
*)
conventionAgnosticSourceOrdering[a:{most___, Intra[i_, i_]}, b_] :=
	conventionAgnosticSourceOrdering[{most, i}, b]

conventionAgnosticSourceOrdering[a_, b:{most___, Intra[i_, i_]}] :=
	conventionAgnosticSourceOrdering[a, {most, i}]



(*
LineColumn

just use natural order
*)
conventionAgnosticSourceOrdering[a:{{_Integer, _Integer}, {_Integer, _Integer}}, b:{{_Integer, _Integer}, {_Integer, _Integer}}] :=
	Order[a, b]


conventionAgnosticSourceOrdering[args___] := (
	Message[conventionAgnosticSourceOrdering::unhandled, {args}];
	$Failed
)


(*
More severe before less severe

So negate ordering result of severityToInteger[]
*)
severityOrdering[a_String, b_String] :=
	-Order[severityToInteger[a], severityToInteger[b]]



(*
This is used in filterLints[] to order:
first: severity
then Source
*)
severityThenSourceOrdering[{aSev_, aSrc_}, {bSev_, bSrc_}] :=
	severityOrdering[aSev, bSev] /. 0 :> conventionAgnosticSourceOrdering[aSrc, bSrc]








scopingDataObjectToLints[scopingDataObject[src_, {___, lastScope : "Module" | "DynamicModule"}, modifiers_, name_]] :=
Module[{},
	
	(*
	create separate lints for Unused, Shadowed, etc.
	and ignore other modifiers
	*)

	Replace[modifiers,
		{
			"unused" -> InspectionObject["UnusedVariable", "Unused " <> "``" <> lastScope <> "`` " <> "variable: ``" <> name <> "``.", "Scoping", <|
				Source -> src,
				ConfidenceLevel -> 0.95,
				"Argument" -> "Module",
				CodeActions -> {
					CodeAction["Remove variable ``" <> name <> "``", DeleteNode, <|Source -> src|>]
				}
			|>],
			"shadowed" -> InspectionObject["ShadowedVariable", "Shadowed " <> "``" <> lastScope <> "`` " <> "variable: ``" <> name <> "``.", "Scoping", <|Source -> src, ConfidenceLevel -> 0.95, "Argument" -> "Module"|>],
			"error" -> InspectionObject["VariableError", "``" <> lastScope <> "`` " <> "variable error: ``" <> name <> "``.", "Error", <|Source -> src, ConfidenceLevel -> 0.95, "Argument" -> "Module"|>],
			_ :> Sequence @@ {}
		}
		,
		{1}
	]
]

scopingDataObjectToLints[scopingDataObject[src_, {___, lastScope : "Block" | "Internal`InheritedBlock"}, modifiers_, name_]] :=
Module[{},
	
	(*
	create separate lints for Unused, Shadowed, etc.
	and ignore other modifiers
	*)

	Replace[modifiers,
		{
			"unused" -> InspectionObject["UnusedVariable", "Unused " <> "``" <> lastScope <> "`` " <> "variable: ``" <> name <> "``.", "Scoping", <|
				Source -> src,
				ConfidenceLevel -> 0.95,
				"Argument" -> "Block",
				CodeActions -> {
					CodeAction["Remove variable ``" <> name <> "``", DeleteNode, <|Source -> src|>]
				}
			|>],
			"shadowed" -> InspectionObject["ShadowedVariable", "Shadowed " <> "``" <> lastScope <> "`` " <> "variable: ``" <> name <> "``.", "Scoping", <|Source -> src, ConfidenceLevel -> 0.95, "Argument" -> "Block"|>],
			"error" -> InspectionObject["VariableError", "``" <> lastScope <> "`` " <> "variable error: ``" <> name <> "``.", "Error", <|Source -> src, ConfidenceLevel -> 0.95, "Argument" -> "Block"|>],
			_ :> Sequence @@ {}
		}
		,
		{1}
	]
]

scopingDataObjectToLints[o:scopingDataObject[src_, scope:{}, modifiers_, name_]] :=
Module[{},
	
	(*
	Currently, Slot is the only possibility for unscoped error

	FIXME: guard against other unscoped errors in the future
	*)

	Replace[modifiers,
		{
			(*
			lower confidence because this is somewhat common
			*)
			"error" -> InspectionObject["UnscopedObjectError", "Unscoped ``Slot`` error: ``" <> name <> "``.", "Error", <|Source -> src, ConfidenceLevel -> 0.80|>],
			(*
			The only modifier should be "error"
			*)
			_ :> Failure["Unhandled", <| "Function" -> scopingDataObjectToLints, "Arguments" -> {o} |>]
		}
		,
		{1}
	]
]

scopingDataObjectToLints[o:scopingDataObject[src_, scope:{___, "SetDelayed" | "RuleDelayed" | "TagSetDelayed" | "UpSetDelayed", "Error"}, modifiers_, name_]] :=
Module[{},

	Replace[modifiers,
		{
			"error" -> InspectionObject["ParameterError", "Pattern error: ``" <> name <> "``.", "Error", <|Source -> src, ConfidenceLevel -> 0.95|>],
			(*
			The only modifier should be "error"
			*)
			_ :> Failure["Unhandled", <| "Function" -> scopingDataObjectToLints, "Arguments" -> {o} |>]
		}
		,
		{1}
	]
]

scopingDataObjectToLints[o:scopingDataObject[src_, scope:{___, "Error"}, modifiers_, name_]] :=
Module[{},

	Replace[modifiers,
		{
			"error" -> InspectionObject["ParameterError", "Parameter error: ``" <> name <> "``.", "Error", <|Source -> src, ConfidenceLevel -> 0.95|>],
			(*
			The only modifier should be "error"
			*)
			_ :> Failure["Unhandled", <| "Function" -> scopingDataObjectToLints, "Arguments" -> {o} |>]
		}
		,
		{1}
	]
]

scopingDataObjectToLints[scopingDataObject[src_, scope:{___, "SlotFunction", "SlotFunction"}, modifiers_, name_]] :=
Module[{},

	Replace[modifiers,
		{
			"shadowed" -> InspectionObject["ShadowedSlot", "Shadowed ``Slot`` caused by nested ``Function``s.", "Warning", <|
				Source -> src,
				ConfidenceLevel -> 0.95,
				"AdditionalDescriptions" -> {"Consider using named ``Function`` parameters instead."}
				|>
			],
			(*
			The only modifier should be "shadowed"
			*)
			_ :> Failure["Unhandled", <| "Function" -> scopingDataObjectToLints, "Arguments" -> {scopingDataObject[src, scope, modifiers]} |>]
		}
		,
		{1}
	]
]

scopingDataObjectToLints[scopingDataObject[src_, {___, lastScope_}, modifiers_, name_]] :=
Module[{},
	
	(*
	create separate lints for Unused, Shadowed, etc.
	and ignore other modifiers
	*)

	Replace[modifiers,
		{
			"unused" -> InspectionObject["UnusedParameter", "Unused " <> "``" <> lastScope <> "`` " <> "parameter: ``" <> name <> "``.", "Scoping", <|
				Source -> src,
				ConfidenceLevel -> 0.95,
				CodeActions -> {
					CodeAction["Remove parameter ``" <> name <> "``", DeleteNode, <|Source -> src|>]
				}
			|>],
			"shadowed" -> InspectionObject["ShadowedParameter", "Shadowed " <> "``" <> lastScope <> "`` " <> "parameter: ``" <> name <> "``.", "Scoping", <|Source -> src, ConfidenceLevel -> 0.95|>],
			"error" -> InspectionObject["ParameterError", "``" <> lastScope <> "`` " <> "parameter error: ``" <> name <> "``.", "Error", <|Source -> src, ConfidenceLevel -> 0.95|>],
			_ :> Sequence @@ {}
		}
		,
		{1}
	]
]




(*
It is convenient to have a function that does:

betterRiffle[{1}, {2}] => {1}

Unfortunately, Riffle does this:
Riffle[{1}, {2}] => {1, 2}

So introduce betterRiffle
*)
betterRiffle[{a_}, _] := {a}

betterRiffle[a_, b_] := Riffle[a, b]



(*
Overload SourceMemberQ for InspectionObject and CellIndex
*)
SourceMemberQ[suppressedRegion[<|CellIndex -> start_|>, <|CellIndex -> end_|>], InspectionObject[_, _, _, KeyValuePattern[CellIndex -> {index_}]]] := (
	start <= index <= end
)

SourceMemberQ[suppressedRegion[<|Source -> start_|>, <|Source -> end_|>], InspectionObject[_, _, _, KeyValuePattern[Source -> src_]]] := (
	SourceMemberQ[{start, end}, src]
)







(*
filter based on:
SeverityExclusions
TagExclusions
ConfidenceLevel
LintLimit

This is called by CodeInspect and CodeInspectSummarize

This function does not have options because:
CodeInspect and CodeInspectSummarize have different values for the options and it doesn't make sense to give defaults here

So values must be provided explicitly
*)
filterLints[lintsIn:{___InspectionObject}, tagExclusionsIn_, severityExclusionsIn_, confidence_, lintLimit_] :=
Catch[
Module[{lints, tagExclusions, severityExclusions, shadowing, confidenceTest, badLints},

	lints = lintsIn;
	tagExclusions = tagExclusionsIn;
	severityExclusions = severityExclusionsIn;

	(*
	Support None for the various exclusion options
	*)
	If[tagExclusions === None,
		tagExclusions = {}
	];

	If[severityExclusions === None,
		severityExclusions = {}
	];

	If[!empty[tagExclusions],
		lints = DeleteCases[lints, InspectionObject[Alternatives @@ tagExclusions, _, _, _]];
		If[$Debug,
			Print["lints: ", lints];
		];
	];

	If[empty[lints],
		Throw[{}]
	];

	If[!empty[severityExclusions],
		lints = DeleteCases[lints, InspectionObject[_, _, Alternatives @@ severityExclusions, _]];
		If[$Debug,
			Print["lints: ", lints];
		];
	];

	If[empty[lints],
		Throw[{}]
	];

	badLints = Cases[lints, InspectionObject[_, _, _, data_?(Not @* KeyExistsQ[ConfidenceLevel])]];
	If[!empty[badLints],
		Message[InspectionObject::confidence, badLints]
	];

	confidenceTest = GreaterEqualThan[confidence];
	lints = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> c_?confidenceTest]]];


	(*

	Disable shadow filtering for now

	Below is quadratic time

	(*
	If a Fatal lint and an Error lint both have the same Source, then only keep the Fatal lint
	*)
	shadowing = Select[lints, Function[lint, AnyTrue[lints, shadows[lint, #]&]]];

	If[$Debug,
	Print["shadowing: ", shadowing];
	];

	lints = Complement[lints, shadowing];
	If[$Debug,
	Print["lints: ", lints];
	];
	*)

	If[empty[lints],
		Throw[{}]
	];

	(*
	Make sure to sort lints before taking

	Sort by severity, then sort by Source
	*)
	lints = SortBy[lints, {#[[3]], #[[4, Key[Source]]]}&, severityThenSourceOrdering];

	lints = Take[lints, UpTo[lintLimit]];

	lints
]]





End[]

EndPackage[]
