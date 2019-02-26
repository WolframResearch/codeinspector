BeginPackage["Lint`Format`"]

LintMarkup

LintBold


LintSpaceIndicator::usage = "LintSpaceIndicator represents a space indicator in formatted output."

LintErrorIndicator::usage = "LintErrorIndicator represented an error indicator in formatted output."

LintContinuation::usage = "LintContinuation represents a continuation in formatted output."

LintTimes::usage = "LintTimes represents a times operator in formatted output."

LintEOF::usage = "LintEOF represents an EOF in formatted output."


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]
Needs["Lint`"]




Format[lint:Lint[tag_String, description_String, severity_String, data_Association], StandardForm] :=
  Column[{tag, LintMarkup[Row[{"Severity: ", severity}], FontColor->severityColor[{lint}], FontWeight->Bold], description}]

Format[lint:Lint[tag_String, description_List, severity_String, data_Association], StandardForm] :=
  Column[{tag, LintMarkup[Row[{"Severity: ", severity}], FontColor->severityColor[{lint}], FontWeight->Bold], Row[description]}]

Format[lint:Lint[tag_String, description_String, severity_String, data_Association], OutputForm] :=
  Row[{tag, " ", LintMarkup[Row[{"Severity: ", severity}], FontColor->severityColor[{lint}], FontWeight->Bold], " ", description}]

Format[lint:Lint[tag_String, description_List, severity_String, data_Association], OutputForm] :=
  Row[{tag, " ", LintMarkup[Row[{"Severity: ", severity}], FontColor->severityColor[{lint}], FontWeight->Bold], " ", Sequence @@ description}]




Options[LintedLine] = {
	"MaxLineNumberLength" -> 5
}

(*
treat {list, list} as Underscript

I originally wanted to do this:
Style[Underscript[line, under], ScriptSizeMultipliers->1]

But there is no guarantee that monospace fonts will be respected
So brute-force it with Grid so that it looks good
*)
Format[LintedLine[lineSource_String, lineNumber_Integer, hash_String, {lineList_List, underlineList_List}, lints:{___Lint}, opts___], StandardForm] :=
  Row[{formatLeftColumn[lineSource, lineNumber, hash, opts], Grid[{lineList, underlineList}, Spacings -> {0, 0}]}]

(*
Grid has too much space in OutputForm, so use Column[{Row[], Row[]}]

Not possible to construct: ab
                           cd

with Grid in OutputForm. bug?
*)
(*
underlineList is not used in OutputForm
*)
Format[LintedLine[lineSource_String, lineNumber_Integer, hash_String, {lineList_List, underlineList_List}, lints:{___Lint}, opts___], OutputForm] :=
Module[{maxLineNumberLength, paddedLineNumber, endingLints},
	maxLineNumberLength = OptionValue[LintedLine, {opts}, "MaxLineNumberLength"];
	paddedLineNumber = StringPadLeft[ToString[lineNumber], maxLineNumberLength, " "];

	(*
	lints that are ending on this line
	format them
	*)
	endingLints = Cases[lints, Lint[_, _, _, KeyValuePattern[Source -> {_, {lineNumber, _}}]]];

	Row[{Row[{"line ", paddedLineNumber, ": "}], Column[Join[Row /@ Partition[lineList, UpTo[150]],
																			Row /@ Partition[underlineList, UpTo[150]],
																			endingLints]]}]
]





Format[LintedLine[lineSource_String, lineNumber_Integer, hash_String, lineList_List, lints:{___Lint}, opts___], StandardForm] :=
  Row[{formatLeftColumn[lineSource, lineNumber, hash, opts], lineList}]

Format[LintedLine[lineSource_String, lineNumber_Integer, hash_String, lineList_List, lints:{___Lint}, opts___], OutputForm] :=
Module[{maxLineNumberLength, paddedLineNumber, endingLints},
	maxLineNumberLength = OptionValue[LintedLine, {opts}, "MaxLineNumberLength"];
	paddedLineNumber = StringPadLeft[ToString[lineNumber], maxLineNumberLength, " "];

	(*
	lints that are ending on this line
	format them
	*)
	endingLints = Cases[lints, Lint[_, _, _, KeyValuePattern[Source -> {_, {lineNumber, _}}]]];

	Row[{Row[{"line ", paddedLineNumber, ": "}], Column[Join[Row /@ Partition[lineList, UpTo[150]], endingLints]]}]
]




Options[formatLeftColumn] = {
	"MaxLineNumberLength" -> 5
}

formatLeftColumn[lineSource_String, lineNumber_Integer, hash_String, opts___] :=
Module[{maxLineNumberLength},
	
	maxLineNumberLength = OptionValue[formatLeftColumn, {opts}, "MaxLineNumberLength"];
	paddedLineNumber = StringPadLeft[ToString[lineNumber], maxLineNumberLength, " "];

	ActionMenu[
		Tooltip[
			Framed[Row[{"line", " ", paddedLineNumber, ":"}], FrameMargins -> {{5, 5}, {1, 1}}, Background -> GrayLevel[0.95]],
			"Click to open menu...", TooltipDelay -> Automatic],
		{"Copy line source" :> CopyToClipboard[lineSource],
		"Copy line number" :> CopyToClipboard[lineNumber],
		"Copy line hash" :> CopyToClipboard[escapeString[hash]]}, Appearance -> None, Method -> "Queued", DefaultBaseStyle -> {}]
]



Format[LintedCharacter[char_, lintList_List, opts___], StandardForm] :=
  Tooltip[LintMarkup[char, FontColor->severityColor[lintList], opts], Column[Riffle[lintList, ""]]]

Format[LintedCharacter[char_, lintList_List, opts___], OutputForm] :=
  LintMarkup[char, FontColor->severityColor[lintList], opts]




severityToInteger["ImplicitTimes"] = 0
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
    "Remark", Blue,
    "Warning", Orange,
    "Error", Red,
    "Fatal", Red,
    "ImplicitTimes", Red
  ]
]




(*
bug 351153
Cannot use characters like \[SpaceIndicator] and \[ErrorIndicator] in OutputForm and
get Grid alignment
*)

Format[LintSpaceIndicator, StandardForm] := "\[SpaceIndicator]"
Format[LintSpaceIndicator, OutputForm] := "~"

Format[LintErrorIndicator, StandardForm] := "\[ErrorIndicator]"
Format[LintErrorIndicator, OutputForm] := "^"

Format[LintContinuation, StandardForm] := "\[Continuation]"
Format[LintContinuation, OutputForm] := "\\"

Format[LintTimes, StandardForm] := "\[Times]"
Format[LintTimes, OutputForm] := "x"

Format[LintEOF, StandardForm] := "\[FilledSquare]"
Format[LintEOF, OutputForm] := "EOF"







Options[LintMarkup] = {
	FontColor -> Automatic,
	FontWeight -> Automatic,
	FontVariations -> Automatic,
	FontSize -> Automatic
}


(*
Windows console doesn't support ANSI escape sequences by default
But MacOSX and Unix should be fine
*)
$UseANSI = Switch[$OperatingSystem,
				"Windows",
				Catch[
				Module[{vals, level, ret},
					(*
					https://blogs.msdn.microsoft.com/commandline/2017/06/20/understanding-windows-console-host-settings/
					*)
					vals = Developer`ReadRegistryKeyValues["HKEY_CURRENT_USER\\Console"];
					If[FailureQ[vals],
						Throw[False]
					];
					level = "VirtualTerminalLevel" /. vals /. {"VirtualTerminalLevel" -> 0};
					ret = (level != 0);
					ret
				]]
				,
				_,
				True
			]

Format[LintMarkup[content_, opts___], StandardForm] := Style[content, opts]

Format[LintMarkup[content_, opts___], OutputForm] :=
Catch[
Module[{s, color, weight, setup},
	
	s = ToString[content, OutputForm];

	(*
	Notebook front end does not support ANSI escape sequences
	*)
	If[$Notebooks || !$UseANSI,
		Throw[s]
	];

	color = OptionValue[LintMarkup, {opts}, FontColor];
	weight = OptionValue[LintMarkup, {opts}, FontWeight];
	(* FontVariations is ignored for now *)
	(* FontSize is ignored for now *)

	(*
	The possibility of 38;5; sequences affecting the bold bit on Windows means that 
	weight should be set first.
	Still would like to completely understand the differences between platforms.
	*)
	setup = StringJoin[{weightANSICode[weight], colorANSICode[color]}];
	If[setup != "",
		(* only print reset if there is anything to reset *)
		s = StringJoin[{setup, s, resetANSICode[]}];
	];
	s
]]




LintBold[content_] := LintMarkup[content, FontWeight->Bold]



colorANSICode[GrayLevel[gray_]] :=
	With[{code = ToString[232 + Round[gray*23]]},
		"\[RawEscape][38;5;"<>code<>"m"
	]
colorANSICode[RGBColor[r_, g_, b_]] :=
	With[{code = ToString[16 + 36*Round[5*r] + 6*Round[5*g] + Round[5*b]]},
		"\[RawEscape][38;5;"<>code<>"m"
	]

(*
Use simpler sequences for common cases

This also works around an issue on Windows where the 38;5; sequences affect the bold bit
*)
colorANSICode[Black] := "\[RawEscape][30m"
colorANSICode[Red] := "\[RawEscape][31m"
colorANSICode[Green] := "\[RawEscape][32m"
colorANSICode[Yellow] := "\[RawEscape][33m"
colorANSICode[Blue] := "\[RawEscape][34m"
colorANSICode[Magenta] := "\[RawEscape][35m"
colorANSICode[Cyan] := "\[RawEscape][36m"
colorANSICode[White] := "\[RawEscape][37m"

colorANSICode[Automatic] = ""


weightANSICode[Bold] = "\[RawEscape][1m"
weightANSICode[Automatic] = ""

variationsANSICode[{"Underline"->True}] = "\[RawEscape][4m"
variationsANSICode[Automatic] = ""

resetANSICode[] = "\[RawEscape][0m"



End[]

EndPackage[]
