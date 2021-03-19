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


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Scoping`"] (* for scopingDataObject *)
Needs["CodeParser`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Format`"]


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







(* CodeInspect::Push *)
(* CodeInspect::Disable::UnexpectedCharacter *)

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
	"\[NoBreak]",

	(*
	FUNCTION APPLICATION
	*)
	"\:2061",

	(*
	Virtual BOM character
	*)
	"\:e001"
} ~Join~

	(*
	C1
	*)
	CharacterRange["\.80", "\.9f"] ~Join~

	(*
	Unicode non-characters
	https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Non-characters
	*)
	CharacterRange["\:fdd0", "\:fdef"]


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
		"\:fffe",
		"\:ffff",
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

(* CodeInspect::Pop *)


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
			Throw[First[SortBy[cases, #[[3, Key[Source]]]&]]]
		];
		cases = Cases[node, CallNode[_, _, KeyValuePattern[Source -> _]], {0, Infinity}];
		If[cases != {},
			Throw[First[SortBy[cases, #[[3, Key[Source]]]&]]]
		];
	]]



lexOrderingForLists[{}, {}] := 0
lexOrderingForLists[{}, b_] := 1
lexOrderingForLists[a_, {}] := -1
lexOrderingForLists[a_, b_] :=
  Order[Take[a, 1], Take[b, 1]] /. 
    0 :> lexOrderingForLists[Drop[a, 1], Drop[b, 1]]



scopingDataObjectToLints[scopingDataObject[src_, {___, lastScope : "Module" | "DynamicModule"}, modifiers_]] :=
Module[{text},
	
	text = "``" <> lastScope <> "`` " <> "variable";
	
	(*
	create separate lints for Unused, Shadowed, etc.
	and ignore other modifiers
	*)

	Replace[modifiers,
		{
			"unused" -> InspectionObject["UnusedModuleVariable", "Unused " <> text, "Scoping", <|Source -> src, ConfidenceLevel -> 0.95|>],
			"shadowed" -> InspectionObject["ShadowedModuleVariable", "Shadowed " <> text, "Scoping", <|Source -> src, ConfidenceLevel -> 0.95|>],
			"error" -> InspectionObject["ModuleVariableError", text <> " error", "Error", <|Source -> src, ConfidenceLevel -> 0.95|>],
			_ :> Sequence @@ {}
		}
		,
		{1}
	]
]

scopingDataObjectToLints[scopingDataObject[src_, {___, lastScope : "Block" | "Internal`InheritedBlock"}, modifiers_]] :=
Module[{text},
	
	text = "``" <> lastScope <> "`` " <> "variable";
	
	(*
	create separate lints for Unused, Shadowed, etc.
	and ignore other modifiers
	*)

	Replace[modifiers,
		{
			"unused" -> InspectionObject["UnusedBlockVariable", "Unused " <> text, "Scoping", <|Source -> src, ConfidenceLevel -> 0.95|>],
			"shadowed" -> InspectionObject["ShadowedBlockVariable", "Shadowed " <> text, "Scoping", <|Source -> src, ConfidenceLevel -> 0.95|>],
			"error" -> InspectionObject["BlockVariableError", text <> " error", "Error", <|Source -> src, ConfidenceLevel -> 0.95|>],
			_ :> Sequence @@ {}
		}
		,
		{1}
	]
]

scopingDataObjectToLints[scopingDataObject[src_, scope:{}, modifiers_]] :=
Module[{},
	
	(*
	Currently, Slot is only possibility for unscoped error

	FIXME: guard against other unscoped errors in the future
	*)

	Replace[modifiers,
		{
			(*
			lower confidence because this is somewhat common
			*)
			"error" -> InspectionObject["UnscopedObjectError", "Unscoped ``Slot`` error", "Error", <|Source -> src, ConfidenceLevel -> 0.80|>],
			(*
			The only modifier should be "error"
			*)
			_ :> Failure["Unhandled", <| "Function" -> scopingDataObjectToLints, "Arguments" -> {scopingDataObject[src, scope, modifiers]} |>]
		}
		,
		{1}
	]
]

scopingDataObjectToLints[scopingDataObject[src_, scope:{___, "SetDelayed" | "RuleDelayed" | "TagSetDelayed" | "UpSetDelayed", "Error"}, modifiers_]] :=
Module[{},

	Replace[modifiers,
		{
			"error" -> InspectionObject["ParameterError", "Pattern error", "Error", <|Source -> src, ConfidenceLevel -> 0.95|>],
			(*
			The only modifier should be "error"
			*)
			_ :> Failure["Unhandled", <| "Function" -> scopingDataObjectToLints, "Arguments" -> {scopingDataObject[src, scope, modifiers]} |>]
		}
		,
		{1}
	]
]

scopingDataObjectToLints[scopingDataObject[src_, scope:{___, "Error"}, modifiers_]] :=
Module[{},

	Replace[modifiers,
		{
			"error" -> InspectionObject["ParameterError", "Parameter error", "Error", <|Source -> src, ConfidenceLevel -> 0.95|>],
			(*
			The only modifier should be "error"
			*)
			_ :> Failure["Unhandled", <| "Function" -> scopingDataObjectToLints, "Arguments" -> {scopingDataObject[src, scope, modifiers]} |>]
		}
		,
		{1}
	]
]

scopingDataObjectToLints[scopingDataObject[src_, {___, "Function"}, modifiers_]] :=
Module[{text},
	
	(*
	Use the text "Slot" instead of "Function parameter"
	*)
	text = "``Slot``";
	
	(*
	create separate lints for Unused, Shadowed, etc.
	and ignore other modifiers
	*)

	Replace[modifiers,
		{
			"unused" -> InspectionObject["UnusedParameter", "Unused " <> text, "Scoping", <|Source -> src, ConfidenceLevel -> 0.95|>],
			"shadowed" -> InspectionObject["ShadowedParameter", "Shadowed " <> text, "Scoping", <|Source -> src, ConfidenceLevel -> 0.95|>],
			"error" -> InspectionObject["ParameterError", text <> " error", "Error", <|Source -> src, ConfidenceLevel -> 0.95|>],
			_ :> Sequence @@ {}
		}
		,
		{1}
	]
]

scopingDataObjectToLints[scopingDataObject[src_, {___, lastScope_}, modifiers_]] :=
Module[{text},
	
	text = "``" <> lastScope <> "`` " <> "parameter";
	
	(*
	create separate lints for Unused, Shadowed, etc.
	and ignore other modifiers
	*)

	Replace[modifiers,
		{
			"unused" -> InspectionObject["UnusedParameter", "Unused " <> text, "Scoping", <|Source -> src, ConfidenceLevel -> 0.95|>],
			"shadowed" -> InspectionObject["ShadowedParameter", "Shadowed " <> text, "Scoping", <|Source -> src, ConfidenceLevel -> 0.95|>],
			"error" -> InspectionObject["ParameterError", text <> " error", "Error", <|Source -> src, ConfidenceLevel -> 0.95|>],
			_ :> Sequence @@ {}
		}
		,
		{1}
	]
]






End[]

EndPackage[]
