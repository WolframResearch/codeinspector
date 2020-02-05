BeginPackage["CodeInspector`Format`"]

LintMarkup

LintBold

LintPreserve



LintSpaceIndicator

LintErrorIndicator

LintErrorContinuationIndicator

LintContinuation

LintTimes

LintEOF





EnableNewLintStyle
DisableNewLintStyle


$Interactive

$NewLintStyle

$LintsPerLineLimit



Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`External`"]
Needs["CodeInspector`Utils`"]


$NewLintStyle = True




(*

The values for both $LintedLineWidth and $LintedLintItemSize are derived heuristically

$LintedLintItemSize is adjusted first to allow mixes of regular characters, \[SpaceIndicator], and \[ErrorIndicator] to
look good in a notebook.

Then $LintedLineWidth is adjusted to allow fitting inside a standard notebook window

*)

(*
How many ems for characters in the Grid ?

Somewhere between 0.606 and 0.607, \[ErrorIndicator] starts overlapping and actually overflow display on the cloud and looks bad

Related issues: CLOUD-15903

The desktop FE allows overlapping just fine

*)
$LintedLintItemSize = 0.65

(*
How many characters before partitioning a new line?
*)
$LintedLineWidth = 120



$LintsPerLineLimit = 5





InspectedFileObject::usage = "InspectedFileObject[file, lintedLines] represents a formatted object of linted lines found in file."

Format[lintedFile:InspectedFileObject[file_String, lintedLines:{___InspectedLineObject}], StandardForm] :=
	Interpretation[
		Framed[Column[{Row[{file}, ImageMargins -> {{0, 0}, {10, 10}}]} ~Join~ lintedLines, Left, 0], Background -> GrayLevel[0.97], RoundingRadius -> 5]
		,
		lintedFile]

Format[lintedFile:InspectedFileObject[file_String, lintedLines:{___InspectedLineObject}], OutputForm] :=
	Column[{Row[{file}], ""} ~Join~ lintedLines, Left]




InspectedStringObject::usage = "InspectedStringObject[string, lintedLines] represents a formatted object of linted lines found in string."

Format[lintedString:InspectedStringObject[stringIn_String, lintedLines:{___InspectedLineObject}], StandardForm] :=
Module[{string},

	string = StringReplace[stringIn, $characterReplacementRules];

	Interpretation[
		Framed[Column[{Row[{string}, ImageMargins -> {{0, 0}, {10, 10}}]} ~Join~ lintedLines, Left, 0], Background -> GrayLevel[0.97], RoundingRadius -> 5]
		,
		lintedString]
]

Format[lintedString:InspectedStringObject[stringIn_String, lintedLines:{___InspectedLineObject}], OutputForm] :=
Module[{string},

	string = StringReplace[stringIn, $characterReplacementRules];

	Column[{Row[{string}], ""} ~Join~ lintedLines, Left]
]



(*
to be overridden
*)
createButton[___] := Failure["Unimplemented", <||>]


createActionMenuItem[___] := Failure["Unimplemented", <||>]



newLintStyle[lint:InspectionObject[tag_, description_, severity_, data_]] :=
Module[{bolded, boldedBoxes, actions, items, menuItems, file, line, col},

	bolded = boldify[description];

	boldedBoxes = With[{bolded = bolded}, MakeBoxes[Row[bolded]]];

	If[$Debug,
		Print["data: ", data];
	];

	If[TrueQ[$Interactive],
		actions = Lookup[data, CodeActions, {}];
		If[$Debug,
			Print["actions: ", actions];
		];

		actions = actions ~Join~ { CodeAction["Dismiss this issue", Identity, <|Source->data[Source]|>] };

		menuItems = createActionMenuItem[#, lint]& /@ actions;

		menuItems = DeleteCases[menuItems, _?FailureQ];

		items = {
					"\"" <> ToString[tag] <> "\"" :> Null,
					"\"" <> "confidence: " <> ToString[PercentForm[data[ConfidenceLevel]]] <> "\"" :> Null,
					Delimiter } ~Join~
					menuItems;
		,
		(* non-Interactive *)

		If[KeyExistsQ[data, "File"],

			If[KeyExistsQ[data, Source],

				file = data["File"];

				line = data[[Key[Source], 1, 1]];
				col = data[[Key[Source], 1, 2]];

				items = With[{file = file, line = line, col = col}, {
					"\"" <> ToString[tag] <> "\"" :> Null,
					"\"" <> "confidence: " <> ToString[PercentForm[data[ConfidenceLevel]]] <> "\"" :> Null,
					Delimiter,
					"Open in editor" :> OpenInEditor[file, line, col] }]

				,

				(*
				Source may not exist in generated nodes
				*)

				items = {
					"\"" <> ToString[tag] <> "\"" :> Null,
					"\"" <> "confidence: " <> ToString[PercentForm[data[ConfidenceLevel]]] <> "\"" :> Null }
			];
			,
			items = {
				"\"" <> ToString[tag] <> "\"" :> Null,
				"\"" <> "confidence: " <> ToString[PercentForm[data[ConfidenceLevel]]] <> "\"" :> Null }
		]
	];

	If[$Debug,
		Print["items: ", items];
	];

	With[{boxes = TemplateBox[{StyleBox[boldedBoxes, "Text"], Sequence @@ severityColorNewStyle[{lint}], items}, "SuggestionGridTemplateXXX", DisplayFunction -> $suggestionGridTemplateDisplayFunction[$Interactive]]},
		RawBoxes[InterpretationBox[boxes, lint]]
	]
]


Format[lint:InspectionObject[tag_String, description_String, severity_String, data_Association], StandardForm] :=
Catch[
Module[{g, bolded, actions, actionButtonsOrFailures, format, menu},

	If[$NewLintStyle,
		Throw[newLintStyle[lint]]
	];

	bolded = boldify[description];

	g = gridify[bolded];


	menu = ActionMenu[
		Tooltip[
			"\[CenterEllipsis]",
			"More information", TooltipDelay -> Automatic], {
		tag :> Null,
		severity :> Null }, Appearance -> None, DefaultBaseStyle -> {}];

	g[[1]] = {
					menu,
					Spacer[20],
					Style["\[WarningSign]", Bold, Large, FontColor->severityColor[{lint}]],
					Spacer[20]} ~Join~ g[[1]];

	If[$Interactive,
		actions = Lookup[data, CodeActions, {}];
		If[$Debug,
				Print["actions: ", actions];
			];
		If[!empty[actions],

			actionButtonsOrFailures = createButton[#, lint]& /@ actions;

			If[$Debug,
				Print["actionButtonsOrFailures: ", actionButtonsOrFailures];
			];

			actionButtonsOrFailures = DeleteCases[actionButtonsOrFailures, _?FailureQ];

			If[!empty[actionButtonsOrFailures],
				g = g ~Join~ { actionButtonsOrFailures };
			];
		];
	];

	g = (Style[#, "Text", ShowStringCharacters->False]& /@ #)& /@ g;

  format = Interpretation[
  	Framed[Column[Row /@ g, {Left, Center}], Background -> GrayLevel[0.92], RoundingRadius -> 5, FrameMargins -> {{10, 10}, {0, 2}}],
  	lint];

  If[$Debug,
		Print["lint: ", format//InputForm];
	];

	format
]]

Format[lint:InspectionObject[tag_String, description_String, severity_String, data_Association], OutputForm] :=
Module[{g, bolded},

	bolded = boldify[description];

	g = gridify[bolded];

	g[[1]] = {LintBold[tag],
					" ",
					LintMarkup[severity, FontWeight->Bold, FontColor->severityColor[{lint}]],
					" " } ~Join~ g[[1]];

  Column[Row /@ g]
]










InspectedLineObject::usage = "InspectedLineObject[lineSource, lineNumber, hash, content, lintList] represents a formatted line of output."

Options[InspectedLineObject] = {
	"MaxLineNumberLength" -> 5,
	"Elided" -> False
}

(*
treat {list, list} as Underscript

I originally wanted to do this:
Style[Underscript[line, under], ScriptSizeMultipliers->1]

But there is no guarantee that monospace fonts will be respected
So brute-force it with Grid so that it looks good
*)
Format[InspectedLineObject[lineSourceIn_String, lineNumber_Integer, hash_String, {lineList_List, underlineList_List}, lints:{___InspectionObject}, opts___], StandardForm] :=
Catch[
Module[{lineSource, endingLints, endingAdditionalLintsAny, endingAdditionalLintsThisLine, elided, startingLints,
	grid, red, darkerOrange, blue},

	lineSource = StringReplace[lineSourceIn, $characterReplacementRules];

	elided = OptionValue[InspectedLineObject, {opts}, "Elided"];

	If[elided,
		Throw[Grid[{{"\[SpanFromAbove]"}}, Alignment -> Center]]
	];

	If[$Debug,
		Print["lineNumber: ", lineNumber];
	];

	startingLints = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[Source -> {{lineNumber, _}, _}]]];
	(*
	lints that are ending on this line
	format them
	*)
	endingLints = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[Source -> {_, {lineNumber, _}}]]];
	If[$Debug,
		Print["endingLints: ", endingLints];
	];

	endingAdditionalLintsAny = Cases[lints, InspectionObject[_, _, _, KeyValuePattern["AdditionalSources" -> _]]];
	If[$Debug,
		Print["endingAdditionalLintsAny: ", endingAdditionalLintsAny];
	];

	endingAdditionalLintsThisLine = Cases[lints, InspectionObject[_, _, _, KeyValuePattern["AdditionalSources" -> srcs_ /; MemberQ[srcs, {_, {lineNumber, _}}]]]];
	If[$Debug,
		Print["endingAdditionalLintsThisLine: ", endingAdditionalLintsThisLine];
	];

	(*
	Don't double-count Lints that have AdditionalSources
	*)
	endingLints = Complement[endingLints, endingAdditionalLintsAny];
	If[$Debug,
		Print["endingLints: ", endingLints];
	];

	endingLints = endingLints ~Join~ endingAdditionalLintsThisLine;
	If[$Debug,
		Print["endingLints: ", endingLints];
	];

	endingLints = Take[endingLints, UpTo[$LintsPerLineLimit]];

	(*
	Make sure to sort lints
	*)
	endingLints = SortBy[endingLints, #[[4, Key[Source] ]]&];
	If[$Debug,
	 Print["endingLints: ", endingLints];
	];

	grid = Transpose /@
		Partition[
			Transpose[{lineList, underlineList}]
			,
			UpTo[$LintedLineWidth]
		];

	(*
	make sure to remove any partitions that do not have errors in them
	This helps to have the formatting look good
	As a test: Create a HUGE single line lint, so that there are hundreds of partitions
	Make sure that only the partitions with errors are displayed.
	*)
	grid = If[MatchQ[#[[2]], {(" " | LintSpaceIndicator)...}], Nothing, #]& /@ grid;

	grid = Flatten[grid, 1];

	(*
	All possible styling options

	Bold is implied by any other markup
	*)
	red = Position[grid, LintMarkup[_, ___, FontColor -> Red, ___]];
	darkerOrange = Position[grid, LintMarkup[_, ___, FontColor -> Darker[Orange], ___]];
	blue = Position[grid, LintMarkup[_, ___, FontColor -> Blue, ___]];

	(*
	Collect all of the adjacent positions of a style into a single range

	This is then fed into the Grid ItemStyle machinery for efficient rendering and space

	TODO: Ranges across rows could also be coalesced

	TODO: Combinations of styles could also be considered

	For example, it would be nice for the top row and the bottom row of an error to be a single range -> {Bold, Red}
	and then the underline could then add in -> {Larger}

	*)
	red = coalesce[red];
	darkerOrange = coalesce[darkerOrange];
	blue = coalesce[blue];

	(*
	now remove markup from the grid itself
	*)
	grid = grid /. LintMarkup[content_, ___] :> content;

	(*
	Alignment option of Row has no effect: bug 93267
	So must use Grid in order to have formatLeftColumn[] aligned on top
	*)
	grid = Grid[
		If[startingLints == {}, Sequence@@{}, {{Row[{Spacer[10]}]}}] ~Join~
		{{formatLeftColumn[lineSource, lineNumber, hash, opts], Spacer[10],
			Column[{Grid[grid,
						Spacings -> {0, 0},
						ItemSize -> $LintedLintItemSize,
						ItemStyle -> {Automatic, Automatic,
							(# -> {Bold, Red}& /@ red) ~Join~
							(# -> {Bold, Darker[Orange]}& /@ darkerOrange) ~Join~
							(# -> {Bold, Blue}& /@ blue) } ] } ~Join~ endingLints]}} ~Join~
		If[endingLints == {}, Sequence@@{}, {{Row[{Spacer[10]}]}}]
		,
		Alignment -> {Left, Top}, Spacings -> {0, 0}];

	If[$Debug,
		Print["grid: ", grid//InputForm];
	];

	Style[grid, ShowStringCharacters->False]
]]


coalesce[list_] :=
  {{#[[1, 1]], #[[-1, 1]]}, {#[[1, 2]], #[[-1, 2]]}}& /@ Split[list, #1[[1]] == #2[[1]] && #1[[2]] + 1 == #2[[2]]&]


(*
Grid has too much space in OutputForm, so use Column[{Row[], Row[]}]

Not possible to construct: ab
                           cd

with Grid in OutputForm. bug?


underlineList is not used in OutputForm
*)
Format[InspectedLineObject[lineSourceIn_String, lineNumber_Integer, hash_String, {lineList_List, underlineList_List}, lints:{___InspectionObject}, opts___], OutputForm] :=
Catch[
Module[{maxLineNumberLength, paddedLineNumber, endingLints, elided, grid, endingAdditionalLintsAny,
	endingAdditionalLintsThisLine},

	lineSource = StringReplace[lineSourceIn, $characterReplacementRules];

	maxLineNumberLength = OptionValue[InspectedLineObject, {opts}, "MaxLineNumberLength"];
	elided = OptionValue[InspectedLineObject, {opts}, "Elided"];

	If[elided,
		Throw[Grid[{{"\[SpanFromAbove]"}}, Alignment -> Center]]
	];

	paddedLineNumber = StringPadLeft[ToString[lineNumber], maxLineNumberLength, " "];

	(*
	lints that are ending on this line
	format them
	*)
	endingLints = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[Source -> {_, {lineNumber, _}}]]];

	endingAdditionalLintsAny = Cases[lints, InspectionObject[_, _, _, KeyValuePattern["AdditionalSources" -> _]]];
	If[$Debug,
		Print["endingAdditionalLintsAny: ", endingAdditionalLintsAny];
	];

	endingAdditionalLintsThisLine = Cases[lints, InspectionObject[_, _, _, KeyValuePattern["AdditionalSources" -> srcs_ /; MemberQ[srcs, {_, {lineNumber, _}}]]]];
	If[$Debug,
		Print["endingAdditionalLintsThisLine: ", endingAdditionalLintsThisLine];
	];

	(*
	Don't double-count Lints that have AdditionalSources
	*)
	endingLints = Complement[endingLints, endingAdditionalLintsAny];
	If[$Debug,
		Print["endingLints: ", endingLints];
	];

	endingLints = endingLints ~Join~ endingAdditionalLintsThisLine;
	If[$Debug,
		Print["endingLints: ", endingLints];
	];

	endingLints = Take[endingLints, UpTo[$LintsPerLineLimit]];

	(*
	Make sure to sort lints
	*)
	endingLints = SortBy[endingLints, #[[4, Key[Source] ]]&];
	If[$Debug,
	 Print["endingLints: ", endingLints];
	];

	grid = Transpose /@
		Partition[
			Transpose[{lineList, underlineList}]
			,
			UpTo[$LintedLineWidth]
		];

	grid = If[MatchQ[#[[2]], {(" " | LintSpaceIndicator)...}], Nothing, #]& /@ grid;

	grid = Flatten[grid, 1];

	Row[{Row[{"line ", paddedLineNumber, ": "}],
			Column[{Column[Row /@ grid]} ~Join~
			endingLints] }]
]]





Format[InspectedLineObject[lineSourceIn_String, lineNumber_Integer, hash_String, lineList_List, lints:{___InspectionObject}, opts___], StandardForm] :=
Catch[
Module[{lineSource, endingLints, elided, startingLints, grid},
	
	lineSource = StringReplace[lineSourceIn, $characterReplacementRules];

	elided = OptionValue[InspectedLineObject, {opts}, "Elided"];

	If[elided,
		Throw[Grid[{{"\[SpanFromAbove]"}}, Alignment -> Center]]
	];

	startingLints = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[Source -> {{lineNumber, _}, _}]]];
	(*
	lints that are ending on this line
	format them
	*)
	endingLints = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[Source -> {_, {lineNumber, _}}]]];

	grid =
		Partition[
			lineList
			,
			UpTo[$LintedLineWidth]
		];

	(*
	TODO: properly remove partitions that do not have errors in them
	*)

	Grid[
		If[startingLints == {}, Sequence@@{}, {{Row[{Spacer[10]}]}}] ~Join~
		{{formatLeftColumn[lineSource, lineNumber, hash, opts], Spacer[20],
			Column[{Grid[grid,
						Spacings -> {0, 0}, ItemSize -> $LintedLintItemSize] } ~Join~ endingLints ]}} ~Join~
		If[endingLints == {}, Sequence@@{}, {{Row[{Spacer[10]}]}}],
			Alignment -> Top, Spacings -> {0, 0}]
]]

Format[InspectedLineObject[lineSource_String, lineNumber_Integer, hash_String, lineList_List, lints:{___InspectionObject}, opts___], OutputForm] :=
Catch[
Module[{maxLineNumberLength, paddedLineNumber, endingLints, elided, grid},

	maxLineNumberLength = OptionValue[InspectedLineObject, {opts}, "MaxLineNumberLength"];
	elided = OptionValue[InspectedLineObject, {opts}, "Elided"];

	If[elided,
		Throw[Grid[{{"\[SpanFromAbove]"}}, Alignment -> Center]]
	];

	paddedLineNumber = StringPadLeft[ToString[lineNumber], maxLineNumberLength, " "];

	(*
	lints that are ending on this line
	format them
	*)
	endingLints = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[Source -> {_, {lineNumber, _}}]]];

	grid =
		Partition[
			lineList
			,
			UpTo[$LintedLineWidth]
		];

	(*
	TODO: properly remove partitions that do not have errors in them
	*)
	
	Row[{Row[{"line ", paddedLineNumber, ": "}], Column[{Column[Row /@ grid]} ~Join~ endingLints] }]
]]




Options[formatLeftColumn] = {
	"MaxLineNumberLength" -> 5
}

formatLeftColumn[lineSource_String, lineNumber_Integer, hash_String, opts___] :=
Catch[
Module[{label, maxLineNumberLength, paddedLineNumber},
	
	maxLineNumberLength = OptionValue[formatLeftColumn, {opts}, "MaxLineNumberLength"];
	
	paddedLineNumber = StringPadLeft[ToString[lineNumber], maxLineNumberLength, " "];

	If[TrueQ[CodeInspector`Summarize`$Underlight],
		label = Style[Row[{"line", " ", paddedLineNumber, ":"}], ShowStringCharacters->False];
		,
		label = Framed[Style[Row[{"line", " ", paddedLineNumber, ":"}], ShowStringCharacters->False]];
	];

	(*
	Copying in cloud:
	CLOUD-14729

	ActionMenu CopyToClipboard with Evaluator->None:
	bug 374583
	*)

	With[{escaped = escapeString[hash]},

	ActionMenu[
		Tooltip[
			label,
			"Click to open menu...", TooltipDelay -> Automatic], {
		"Copy line source" :> CopyToClipboard[lineSource],
		"Copy line number" :> CopyToClipboard[lineNumber],
		"Copy line hash" :> CopyToClipboard[escaped] }, Appearance -> None, DefaultBaseStyle -> {}]
	]
]]





(*
bug 351153
Cannot use characters like \[SpaceIndicator] and \[ErrorIndicator] in OutputForm and
get Grid alignment
*)

LintSpaceIndicator::usage = "LintSpaceIndicator represents a space indicator in formatted output."

Format[LintSpaceIndicator, StandardForm] := "\[SpaceIndicator]"
Format[LintSpaceIndicator, OutputForm] := " "


LintErrorIndicator::usage = "LintErrorIndicator represents an error indicator in formatted output."

Format[LintErrorIndicator, StandardForm] := "\[ErrorIndicator]"
Format[LintErrorIndicator, OutputForm] := "^"


LintErrorContinuationIndicator::usage = "LintErrorContinuationIndicator represents an error continuation indicator in formatted output."

Format[LintErrorContinuationIndicator, StandardForm] := "\[ErrorIndicator]"
Format[LintErrorContinuationIndicator, OutputForm] := "~"



LintContinuation::usage = "LintContinuation represents a continuation in formatted output."

Format[LintContinuation, StandardForm] := "\[Continuation]"
Format[LintContinuation, OutputForm] := "\\"



LintTimes::usage = "LintTimes represents a times operator in formatted output."

Format[LintTimes, StandardForm] := "\[Times]"
Format[LintTimes, OutputForm] := "x"



LintEOF::usage = "LintEOF represents an EOF in formatted output."

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
$UseANSI = Catch[Switch[$OperatingSystem,
				"Windows",
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
				]
				,
				_,
				True
			]]

Format[LintMarkup[content_, specs___], StandardForm] := Style[content, specs]

Format[LintMarkup[content_, specs___], OutputForm] :=
Catch[
Module[{s, color, weight, setup, opts},
	
	s = ToString[content, OutputForm];

	(*
	Notebook front end does not support ANSI escape sequences
	*)
	If[$Notebooks || !$UseANSI,
		Throw[s]
	];

	opts = Cases[{specs}, (Rule | RuleDelayed)[_, _]];

	color = OptionValue[LintMarkup, opts, FontColor];
	weight = OptionValue[LintMarkup, opts, FontWeight];
	(* FontVariations is ignored for now *)
	(* FontSize is ignored for now *)

	(*
	The possibility of 38;5; sequences affecting the bold bit on Windows means that 
	weight should be set first.
	FIXME: Still would like to completely understand the differences between platforms.
	*)
	setup = StringJoin[{weightANSICode[weight], colorANSICode[color]}];
	If[setup != "",
		(* only print reset if there is anything to reset *)
		s = StringJoin[{setup, s, resetANSICode[]}];
	];
	s
]]




LintBold[content_] := LintMarkup[content, "Program", FontWeight->Bold, FontColor->Black]

LintPreserve[content_] := LintMarkup[content]






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











$prettyTooltipTemplateDisplayFunction = Function[
     TagBox[
      TooltipBox[
       #1,
       FrameBox[
        StyleBox[
         #2,
         "Text",
         FontColor -> RGBColor[
          0.537255,
          0.537255,
          0.537255
         ],
         FontSize -> 12,
         FontWeight -> "Plain",
         FontTracking -> "Plain",
         StripOnInput -> False
        ],
        Background -> RGBColor[
         0.960784,
         0.960784,
         0.960784
        ],
        FrameStyle -> RGBColor[
         0.898039,
         0.898039,
         0.898039
        ],
        FrameMargins -> 8,
        StripOnInput -> False
       ],
       TooltipDelay -> 0.1,
       TooltipStyle -> {Background -> None, CellFrame -> 0}
      ],
      Function[
       Annotation[
        #1,
        Framed[
         Style[
          FunctionResourceTools`BuildDefinitionNotebook`Private`$$tooltip,
          "Text",
          FontColor -> RGBColor[
           0.537255,
           0.537255,
           0.537255
          ],
          FontSize -> 12,
          FontWeight -> "Plain",
          FontTracking -> "Plain"
         ],
         Background -> RGBColor[
          0.960784,
          0.960784,
          0.960784
         ],
         FrameStyle -> RGBColor[
          0.898039,
          0.898039,
          0.898039
         ],
         FrameMargins -> 8
        ],
        "Tooltip"
       ]
      ]
     ]
    ]



$suggestionIconTemplateDisplayFunction = Function[
     GraphicsBox[
      {
       Thickness[0.05555555555555555],
       StyleBox[
        {
         FilledCurveBox[
          {
           {
            {1, 4, 3},
            {1, 3, 3},
            {1, 3, 3},
            {1, 3, 3},
            {1, 3, 3},
            {1, 3, 3},
            {1, 3, 3},
            {1, 3, 3}
           }
          },
          CompressedData[
           "\n1:eJxTTMoPSmVmYGBgBGJJIGZigIIGAwcIQ8kBxk94ekHp9k9Vh4qXaoYcOfoO\nm+a+X37stKZDbP+hrxpzdOA0TBymDqYPl7n2pnG7PHlk4PzZRxQ2FGWIwPWD\njI3p54WbLxuVYn3fnwluD8S8H/Yo9gD5KPYA+TB7YPph9sDMh9EwcZg6FPdh\nMRfdXpi7YPph7oaZD/MXzB5c4QCzBwA8nn+Z\n       "
          ]
         ]
        },
        FaceForm[#1]
       ],
       StyleBox[
        {
         FilledCurveBox[
          {
           {
            {0, 2, 0},
            {0, 1, 0},
            {0, 1, 0},
            {0, 1, 0},
            {0, 1, 0},
            {0, 1, 0}
           },
           {
            {1, 4, 3},
            {1, 3, 3},
            {1, 3, 3},
            {1, 3, 3},
            {1, 3, 3},
            {1, 3, 3},
            {1, 3, 3},
            {1, 3, 3}
           }
          },
          {
           {
            {8.175292500000001, 7.416875},
            {7.796855000000001, 11.3084375},
            {7.796855000000001, 13.38},
            {10.11998, 13.38},
            {10.11998, 11.3084375},
            {9.741542500000001, 7.416875},
            {8.175292500000001, 7.416875}
           },
           CompressedData[
            "\n1:eJxTTMoPSmViYGCQBGIQ/cTvZcLf/4oOD6tE1rk/5HNQjDzAkqeL4FsusdsW\n1KjgwAAGAg7hCSdehX2Xd5BvfR24Q07QwaZCOJPjjZyDHdf1xQW2Qg56LJYa\niWlyDv2HvmrEzBeG80GmVbmIwvkvtjT6Sb8Qg+t/BLLPUwJuPti6DEm4/WD7\n2qTg7gMZJyIm7QBzP4y/zEVob88lJTi/7+dk7hV1ynD9c3LzfPxZVODmr3ro\n0futUwVu/0bpbbqnzqjA3Qfjw9wP48P8B9MP8z/MfFj4wOyHhR/MfbDwRQ9/\nACBxmlc=\n        "
           ]
          }
         ]
        },
        FaceForm[#2]
       ]
      },
      ImageSize -> #3,
      PlotRange -> #4,
      AspectRatio -> Automatic,
      BaselinePosition -> Scaled[0.1]
     ]
    ]



$prettyTooltipBox[interactive_] :=
With[{$prettyTooltipTemplateDisplayFunction = $prettyTooltipTemplateDisplayFunction},
	If[TrueQ[interactive] || True,
         AdjustmentBox[
          TemplateBox[
           {
            ActionMenuBox[
             TagBox[
              PaneSelectorBox[
               {
                False -> GraphicsBox[
                 {
                  EdgeForm[Directive[GrayLevel[1, 0], Thickness[0.025]]],
                  FaceForm[#4],
                  RectangleBox[{-1.75, -2}, {1.75, 2}, RoundingRadius -> 0.2],
                  Thickness[0.15],
                  #5,
                  LineBox[{{-0.5, -1.}, {0.5, 0.}, {-0.5, 1.}}]
                 },
                 ImageSize -> {Automatic, 15},
                 ImageMargins -> 0
                ],
                True -> GraphicsBox[
                 {
                  EdgeForm[Directive[#5, Thickness[0.025]]],
                  FaceForm[#2],
                  RectangleBox[{-1.75, -2}, {1.75, 2}, RoundingRadius -> 0.2],
                  Thickness[0.15],
                  GrayLevel[1],
                  LineBox[{{-0.5, -1.}, {0.5, 0.}, {-0.5, 1.}}]
                 },
                 ImageSize -> {Automatic, 15},
                 ImageMargins -> 0
                ]
               },
               Dynamic[CurrentValue["MouseOver"]],
               ImageSize -> Automatic,
               FrameMargins -> 0
              ],
              MouseAppearanceTag["LinkHand"]
             ],
             #6,
             Appearance -> None,
             Method -> "Queued"
            ],
            "\"View suggestions\""
           },
           "PrettyTooltipTemplateXXX"
           ,
           DisplayFunction -> $prettyTooltipTemplateDisplayFunction
          ],
          BoxBaselineShift -> -0.3,
          BoxMargins -> {{2, 0}, {0, 0}}
         ]
         ,
         Sequence @@ {}
         ]
]



$suggestionGridTemplateDisplayFunction[interactive_] :=
With[{$suggestionIconTemplateDisplayFunction = $suggestionIconTemplateDisplayFunction,
	$prettyTooltipBox = $prettyTooltipBox[interactive],
	paneBoxImageSize = If[TrueQ[interactive], (*Full*)Automatic, Automatic],
	prettyTooltipBoxColumnItemSize = If[TrueQ[interactive], (*Fit*)4, (*Sequence @@ {}*)4]},
 Function[
  StyleBox[
   FrameBox[
    AdjustmentBox[
     TagBox[
      GridBox[
       {
        {
         TemplateBox[
          {#2, #3, {16., 16.}, {{1., 17.}, {1., 17.}}},
          "SuggestionIconTemplateXXX"
          ,
          DisplayFunction -> $suggestionIconTemplateDisplayFunction
         ],
         PaneBox[
          #1,
          (*ImageSizeAction -> "ShrinkToFit",*)
          BaselinePosition -> Baseline,
          ImageSize -> paneBoxImageSize
         ],

         $prettyTooltipBox
        }
       },
       GridBoxAlignment -> {"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
       AutoDelete -> False,
       GridBoxItemSize -> {
        "Columns" -> {Automatic, Automatic, prettyTooltipBoxColumnItemSize},
        "Rows" -> {{Automatic}}
       },
       GridBoxSpacings -> {"Columns" -> {{0.4}}}
      ],
      "Grid"
     ],
     BoxMargins -> {{0.25, -1.0}, {0, 0}}
    ],
    RoundingRadius -> {13, 75},
    Background -> #4,
    FrameStyle -> None,
    FrameMargins -> {{0, 8 + 8}, {0, 0}},
    ImageMargins -> {{0, 0}, {5, 5}},
    StripOnInput -> False
   ]
   ,
   FontColor -> #5
  ]
 ]
]



EnableNewLintStyle[nb_NotebookObject] := (
  $NewLintStyle = True;
)


DisableNewLintStyle[nb_NotebookObject] := (
  $NewLintStyle = False;
)





End[]

EndPackage[]
