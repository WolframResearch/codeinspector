BeginPackage["CodeInspector`Format`"]

LintMarkup

LintBold

LintPreserve



LintSpaceIndicatorCharacter

LintErrorIndicatorCharacter

LintMissingOpenerIndicatorCharacter
LintMissingCloserIndicatorCharacter

LintErrorContinuationIndicatorCharacter

LintContinuationCharacter

(*
There is confidence to believe that these are all possible combinations of implicit characters in syntactically valid text
*)
LintAllCharacter
LintNullCharacter
LintOneCharacter
LintTimesCharacter
LintSpaceTimesCharacter
LintExpectedOperandCharacter
LintAllCloseCharacter
LintAllOneCharacter
LintAllTimesCharacter
LintCloseCloseCharacter
LintCloseTimesCharacter
LintOpenOneCharacter
LintOpenOpenCharacter
LintTimesOneCharacter
LintExpectedOperandTimesCharacter
LintExpectedOperandCloseCharacter
LintOpenExpectedOperandCharacter
LintAllTimesOneCharacter
LintCloseTimesOneCharacter

LintEOFCharacter

LintUnhandledCharacter




insertFormatInspectionObjectsAsPills



$Interactive

$LintsPerLineLimit


$FormatInspectionObjectsAsPills

$LintTextFontWeight

$LintGridFontWeight


Begin["`Private`"]

Needs["CodeInspector`"]
Needs["CodeInspector`External`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]



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


(*
In Apr 26 2021 SW CodeTools meeting, SW wanted to have no bold at all.
The Program style is enough to differentiate it from text
*)
$LintTextFontWeight = "Medium"

$LintGridFontWeight = Bold



InspectedFileObject::usage = "InspectedFileObject[file, lintedLines] represents a formatted object of linted lines found in file."

Format[lintedFile:InspectedFileObject[file_String, lintedLines:{___InspectedLineObject}], StandardForm] :=
  Interpretation[
    Framed[Column[{Row[{File[file]}, ImageMargins -> {{0, 0}, {10, 10}}]} ~Join~ lintedLines, Left, 0], Background -> GrayLevel[0.985], RoundingRadius -> 5]
    ,
    lintedFile
  ]

Format[lintedFile:InspectedFileObject[file_String, lintedLines:{___InspectedLineObject}], OutputForm] :=
  Column[{Row[{file}], ""} ~Join~ lintedLines, Left]

Format[lintedFile:InspectedFileObject[file_String, lintedLines:{___InspectedLineObject}], ScriptForm] :=
  Format[lintedFile, OutputForm]



InspectedStringObject::usage = "InspectedStringObject[string, lintedLines] represents a formatted object of linted lines found in string."

Format[lintedString:InspectedStringObject[stringIn_String, lintedLines:{___InspectedLineObject}], StandardForm] :=
Module[{string},

  string = StringReplace[stringIn, $characterReplacementRules];

  string = displayAsSingleLine[string];

  Interpretation[
    Framed[Column[{Row[{string}, ImageMargins -> {{0, 0}, {10, 10}}]} ~Join~ lintedLines, Left, 0], Background -> GrayLevel[0.985], RoundingRadius -> 5]
    ,
    lintedString]
]

Format[lintedString:InspectedStringObject[stringIn_String, lintedLines:{___InspectedLineObject}], OutputForm] :=
Module[{string},

  string = StringReplace[stringIn, $characterReplacementRules];

  string = displayAsSingleLine[string];

  Column[{Row[{string}], ""} ~Join~ lintedLines, Left]
]

Format[lintedString:InspectedStringObject[stringIn_String, lintedLines:{___InspectedLineObject}], ScriptForm] :=
  Format[lintedString, OutputForm]



InspectedBytesObject::usage = "InspectedBytesObject[bytes, lintedLines] represents a formatted object of linted lines found in bytes."

Format[lintedBytes:InspectedBytesObject[bytesIn:{_Integer...}, lintedLines:{___InspectedLineObject}], StandardForm] :=
Catch[
Module[{string},

  string = SafeString[bytesIn];

  If[FailureQ[string],
    Throw[string]
  ];

  If[MissingQ[string],
    Throw[string]
  ];

  string = StringReplace[string, $characterReplacementRules];

  string = displayAsSingleLine[string];

  Interpretation[
    Framed[Column[{Row[{string}, ImageMargins -> {{0, 0}, {10, 10}}]} ~Join~ lintedLines, Left, 0], Background -> GrayLevel[0.985], RoundingRadius -> 5]
    ,
    lintedBytes]
]]

Format[lintedBytes:InspectedBytesObject[bytesIn:{_Integer...}, lintedLines:{___InspectedLineObject}], OutputForm] :=
Catch[
Module[{string},

  string = SafeString[bytesIn];

  If[FailureQ[string],
    Throw[string]
  ];
  
  If[MissingQ[string],
    Throw[string]
  ];

  string = displayAsSingleLine[string];

  string = StringReplace[string, $characterReplacementRules];

  Column[{Row[{string}], ""} ~Join~ lintedLines, Left]
]]

Format[lintedBytes:InspectedBytesObject[bytesIn:{_Integer...}, lintedLines:{___InspectedLineObject}], ScriptForm] :=
  Format[lintedBytes, OutputForm]



displayAsSingleLine[s_String] :=
Module[{lines},
  lines = StringSplit[StringTrim[s], "\r\n" | "\n" | "\r", All];
  If[Length[lines] == 1 && StringLength[lines[[1]]] <= $LintedLineWidth,
    lines[[1]]
    ,
    Grid[
      {{StringTake[lines[[1]], UpTo[$LintedLineWidth]], "\[SpanFromLeft]"}, {"\[SpanFromAbove]", "\[SpanFromBoth]"}}
      ,
      Alignment -> Left
    ]
  ]
]



(*
to be overridden
*)
createButton[___] := Failure["Unimplemented", <||>]


createActionMenuItem[___] := Failure["Unimplemented", <||>]


(*
newLintStyle[lint:InspectionObject[tag_, description_, severity_, data_]] :=
Module[{bolded, boldedBoxes, actions, items, menuItems, file, line, col, suggestions, rows, resolvedEditor},

  actions = Lookup[data, CodeActions, {}];

  If[TrueQ[$Interactive],
    
    bolded = boldify[description];

    boldedBoxes = With[{bolded = bolded}, MakeBoxes[Row[bolded]]];

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

    bolded = boldify[description];
    
    If[actions != {},
    
      suggestions = #[[1]]& /@ actions;
      
      suggestions = boldify /@ suggestions;

      rows = {Row[bolded]} ~Join~ {Row[{}], Row[{Style["Suggestions:", Smaller]}]} ~Join~ (Row /@ suggestions);

      boldedBoxes = With[{rows = rows}, MakeBoxes[Column[rows]]];
      ,
      (* no CodeActions *)
      boldedBoxes = With[{bolded = bolded}, MakeBoxes[Row[bolded]]];
    ];

    If[KeyExistsQ[data, "File"],

      If[KeyExistsQ[data, Source],

        file = data["File"];

        If[FileExistsQ[file],

          line = data[[Key[Source], 1, 1]];
          col = data[[Key[Source], 1, 2]];

          resolvedEditor = Lookup[data, "Editor", Automatic];
          If[resolvedEditor === Automatic,
            resolvedEditor = $Editor
          ];
          If[!StringQ[resolvedEditor],
            resolvedEditor = "FrontEnd"
          ];

          items = With[{file = file, line = line, col = col, resolvedEditor = resolvedEditor}, {
            "\"" <> ToString[tag] <> "\"" :> Null,
            "\"" <> "confidence: " <> ToString[PercentForm[data[ConfidenceLevel]]] <> "\"" :> Null,
            Delimiter,
            "Open in editor (" <> resolvedEditor <> ")" :> OpenInEditor[file, line, col, "Editor" -> Lookup[data, "Editor", Automatic]] }]

          ,

          (*
          May be doing something like:
          ImportString["If[a, b, b]", {"WL", "CodeInspections"}]

          Technically, CodeInspect was called with File[file], but that was a temp file and it is now deleted

          So do not show "Open in editor"
          *)

          items = {
            "\"" <> ToString[tag] <> "\"" :> Null,
            "\"" <> "confidence: " <> ToString[PercentForm[data[ConfidenceLevel]]] <> "\"" :> Null }
        ]
        ,

        (*
        Source may not exist in generated nodes
        *)

        items = {
          "\"" <> ToString[tag] <> "\"" :> Null,
          "\"" <> "confidence: " <> ToString[PercentForm[data[ConfidenceLevel]]] <> "\"" :> Null }
      ]
      ,

      (*
      May not have been called with a file
      *)

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
*)


(*
Make sure that contexts are removed from symbols
*)
niceToString[s_Symbol] := SymbolName[s]
niceToString[e_] := ToString[e]


Format[lint:InspectionObject[tag_String, description_String, severity_String, data_Association], StandardForm] :=
Catch[
Module[{g, bolded, actions, actionButtonsOrFailures, format, menu,
  boldedBoxes, items, menuItems, file, line, col, suggestions, rows, resolvedEditor, icon},

  If[!TrueQ[data["FormatInspectionObjectsAsPills"]],

    (*
    Format as SummaryBox
    *)

    (*
    Just hard-code with Warning gray until design review
    *)
    icon = RawBoxes[iconBoxesFromSeverity["Warning"]];

    Throw[
      RawBoxes@BoxForm`ArrangeSummaryBox[
        InspectionObject
        ,
        lint
        ,
        icon
        ,
        {
          Row[boldify[description]],
          If[KeyExistsQ[data, "AdditionalDescriptions"], Sequence @@ (Row[boldify[#]]& /@ data["AdditionalDescriptions"]), Nothing],
          If[KeyExistsQ[data, CodeActions], BoxForm`SummaryItem[{"Suggestions: ", Row[boldify[#["Label"]]]& /@ data[CodeActions]}], Nothing],
          If[KeyExistsQ[data, CellIndex], BoxForm`SummaryItem[{"CellIndex: ", data[CellIndex]}], Nothing],
          BoxForm`SummaryItem[{"Source: ", data[Source]}]
        }
        ,
        {
          BoxForm`SummaryItem[{"Severity: ", severity}],
          BoxForm`SummaryItem[{"Confidence: ", data[ConfidenceLevel]}],
          BoxForm`SummaryItem[{"Tag: ", tag}]
        } ~Join~
        
        KeyValueMap[Function[{k, v}, BoxForm`SummaryItem[{niceToString[k] <> ": ", v}]],
          KeyDrop[data, {
            (*
            Already displayed
            *)
            Source, ConfidenceLevel, "AdditionalDescriptions",
            (*
            Already handled
            *)
            CodeActions,
            (*
            Deliberately hide
            *)
            "FormatInspectionObjectsAsPills"
          }]
        ]
        ,
        StandardForm
      ]
    ]
  ];


  (*
  Format as pill
  *)

  actions = Lookup[data, CodeActions, {}];

  If[TrueQ[$Interactive],
    
    bolded = boldify[description];

    rows = {Row[bolded]};

    If[KeyExistsQ[data, "AdditionalDescriptions"],
      rows = rows ~Join~ (Row[boldify[#]]& /@ data["AdditionalDescriptions"])
    ];

    boldedBoxes = With[{rows = rows}, MakeBoxes[Column[rows]]];

    actions = actions ~Join~ { CodeAction["Dismiss this issue", Identity, <| Source -> data[Source] |>] };

    menuItems = createActionMenuItem[#, lint]& /@ actions;

    menuItems = DeleteCases[menuItems, _?FailureQ];

    items = {
          (*
          copy the correct syntax for TagExclusions to the clipboard
          *)
          If[KeyExistsQ[data, "Argument"],
            "\"" <> tag <> "\[VeryThinSpace]\:25bb\[VeryThinSpace]" <> data["Argument"] <> "\"" :> CopyToClipboard[ToString[{tag, data["Argument"]}, InputForm]]
            ,
            "\"" <> tag <> "\"" :> CopyToClipboard[ToString[tag, InputForm]]
          ],
          "\"" <> "confidence: " <> ToString[PercentForm[data[ConfidenceLevel]]] <> "\"" :> Null,
          Delimiter } ~Join~
          menuItems;
    ,
    (* non-Interactive *)
    
    If[actions != {},

      bolded = boldify[description];

      rows = {Row[bolded]};

      If[KeyExistsQ[data, "AdditionalDescriptions"],
        rows = rows ~Join~ (Row[boldify[#]]& /@ data["AdditionalDescriptions"])
      ];

      (*
      TODO: it would be good to have CodeAction objects format themselves
      Would need to figure out how to share formatting code such as boldify[]
      *)
      suggestions = Row[boldify[#["Label"]]]& /@ actions;

      rows = rows ~Join~ {Row[{}], Row[{Style["Suggestions:", Smaller]}]} ~Join~ suggestions;

      boldedBoxes = With[{rows = rows}, MakeBoxes[Column[rows]]];
      ,
      (* no CodeActions *)

      bolded = boldify[description];

      rows = {Row[bolded]};

      If[KeyExistsQ[data, "AdditionalDescriptions"],
        rows = rows ~Join~ (Row[boldify[#]]& /@ data["AdditionalDescriptions"])
      ];

      boldedBoxes = With[{rows = rows}, MakeBoxes[Column[rows]]];
    ];

    If[KeyExistsQ[data, "File"],

      If[KeyExistsQ[data, Source],

        file = data["File"];

        If[FileExistsQ[file],

          line = data[[Key[Source], 1, 1]];
          col = data[[Key[Source], 1, 2]];

          resolvedEditor = Lookup[data, "Editor", Automatic];
          If[resolvedEditor === Automatic,
            resolvedEditor = $Editor
          ];
          If[!StringQ[resolvedEditor],
            resolvedEditor = "FrontEnd"
          ];

          items = With[{file = file, line = line, col = col, resolvedEditor = resolvedEditor}, {
            (*
            copy the correct syntax for TagExclusions to the clipboard
            *)
            If[KeyExistsQ[data, "Argument"],
              "\"" <> tag <> "\[VeryThinSpace]\:25bb\[VeryThinSpace]" <> data["Argument"] <> "\"" :> CopyToClipboard[ToString[{tag, data["Argument"]}, InputForm]]
              ,
              "\"" <> tag <> "\"" :> CopyToClipboard[ToString[tag, InputForm]]
            ],
            "\"" <> "confidence: " <> ToString[PercentForm[data[ConfidenceLevel]]] <> "\"" :> Null,
            Delimiter,
            "Open in editor (" <> resolvedEditor <> ")" :> OpenInEditor[file, line, col, "Editor" -> Lookup[data, "Editor", Automatic]] }]

          ,

          (*
          May be doing something like:
          ImportString["If[a, b, b]", {"WL", "CodeInspections"}]

          Technically, CodeInspect was called with File[file], but that was a temp file and it is now deleted

          So do not show "Open in editor"
          *)

          items = {
            (*
            copy the correct syntax for TagExclusions to the clipboard
            *)
            If[KeyExistsQ[data, "Argument"],
              "\"" <> tag <> "\[VeryThinSpace]\:25bb\[VeryThinSpace]" <> data["Argument"] <> "\"" :> CopyToClipboard[ToString[{tag, data["Argument"]}, InputForm]]
              ,
              "\"" <> tag <> "\"" :> CopyToClipboard[ToString[tag, InputForm]]
            ],
            "\"" <> "confidence: " <> ToString[PercentForm[data[ConfidenceLevel]]] <> "\"" :> Null }
        ]
        ,

        (*
        Source may not exist in generated nodes
        *)

        items = {
          (*
          copy the correct syntax for TagExclusions to the clipboard
          *)
          If[KeyExistsQ[data, "Argument"],
            "\"" <> tag <> "\[VeryThinSpace]\:25bb\[VeryThinSpace]" <> data["Argument"] <> "\"" :> CopyToClipboard[ToString[{tag, data["Argument"]}, InputForm]]
            ,
            "\"" <> tag <> "\"" :> CopyToClipboard[ToString[tag, InputForm]]
          ],
          "\"" <> "confidence: " <> ToString[PercentForm[data[ConfidenceLevel]]] <> "\"" :> Null }
      ]
      ,

      (*
      May not have been called with a file
      *)

      items = {
        (*
        copy the correct syntax for TagExclusions to the clipboard
        *)
        If[KeyExistsQ[data, "Argument"],
          "\"" <> tag <> "\[VeryThinSpace]\:25bb\[VeryThinSpace]" <> data["Argument"] <> "\"" :> CopyToClipboard[ToString[{tag, data["Argument"]}, InputForm]]
          ,
          "\"" <> tag <> "\"" :> CopyToClipboard[ToString[tag, InputForm]]
        ],
        "\"" <> "confidence: " <> ToString[PercentForm[data[ConfidenceLevel]]] <> "\"" :> Null }
    ]
  ];

  If[$Debug,
    Print["items: ", items];
  ];

  With[{boxes = TemplateBox[{StyleBox[boldedBoxes, "Text"], Sequence @@ severityColorNewStyle[{lint}], items}, "SuggestionGridTemplateXXX", DisplayFunction -> $suggestionGridTemplateDisplayFunction[$Interactive]]},
    RawBoxes[InterpretationBox[boxes, lint]]
  ]
]]

Format[lint:InspectionObject[tag_String, description_String, severity_String, data_Association], OutputForm] :=
Module[{g, bolded, actions, suggestions},

  bolded = boldify[description];

  actions = Lookup[data, CodeActions, {}];

  g = gridify[bolded];

  If[actions != {},
    
    suggestions = boldify[#["Label"]]& /@ actions;

    g = g ~Join~ {{}} ~Join~ {{"Suggestions:"}} ~Join~ suggestions ~Join~ {{}};
  ];

  g[[1]] = {LintBold[tag],
          " ",
          LintMarkup[severity, FontWeight->$LintGridFontWeight, FontColor->severityColor[{lint}]],
          " " } ~Join~ g[[1]];

  Column[Row /@ g]
]



iconBoxesFromSeverity[sev_] :=
With[
  {color = severityColorNewStyle[sev]}
  ,
  {func = $suggestionIconTemplateDisplayFunction, c1 = color[[1]], c2 = color[[2]]}
  ,
    TemplateBox[{c1, c2, Dynamic[{Automatic, 3.5 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}]}, 
        "SuggestionIconTemplateXXX", 
        DisplayFunction -> func
  ]
]







InspectedLineObject::usage = "InspectedLineObject[lineSource, lineNumber, content, lints] represents a formatted line of output."

Options[InspectedLineObject] = {
  "MaxLineNumberLength" -> 5,
  "Elided" -> False,
  "Environ" -> False
}

(*
treat {list, list} as Underscript

I originally wanted to do this:
Style[Underscript[line, under], ScriptSizeMultipliers->1]

But there is no guarantee that monospace fonts will be respected
So brute-force it with Grid so that it looks good
*)
Format[InspectedLineObject[lineSourceIn_String, lineNumber_Integer, {lineList_List, underlineList_List}, lints:{___InspectionObject}, opts___], StandardForm] :=
Catch[
Module[{lineSource, endingLints, endingAdditionalLintsAny, endingAdditionalLintsThisLine, elided, startingLints,
  grid, red, darkerOrange, blue, environ},

  lineSource = StringReplace[lineSourceIn, $characterReplacementRules];

  elided = OptionValue[InspectedLineObject, {opts}, "Elided"];
  environ = OptionValue[InspectedLineObject, {opts}, "Environ"];

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

  (*
  Make sure to sort lints
  *)
  endingLints = SortBy[endingLints, #[[4, Key[Source]]]&, conventionAgnosticSourceOrdering];

  endingLints = Take[endingLints, UpTo[$LintsPerLineLimit]];

  (*
  add formatting instructions
  *)
  endingLints = insertFormatInspectionObjectsAsPills /@ endingLints;


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
  If[!environ,
    grid = If[MatchQ[#[[2]], {(" " | LintSpaceIndicatorCharacter)...}], Nothing, #]& /@ grid;
  ];
  
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
  and then the underline could then add in e.g., -> {Larger}

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
    {{formatLeftColumn[lineSource, lineNumber, FilterRules[{opts}, Options[formatLeftColumn]]], Spacer[10],
      Column[{Grid[grid,
            Spacings -> {0, 0},
            ItemSize -> $LintedLintItemSize,
            ItemStyle -> {Automatic, Automatic,
              (# -> {$LintGridFontWeight, Red}& /@ red) ~Join~
              (# -> {$LintGridFontWeight, Darker[Orange]}& /@ darkerOrange) ~Join~
              (# -> {$LintGridFontWeight, Blue}& /@ blue) } ] } ~Join~ endingLints]}} ~Join~
    If[endingLints == {}, Sequence@@{}, {{Row[{Spacer[10]}]}}]
    ,
    Alignment -> {Left, Top}, Spacings -> {0, 0}];

  If[$Debug,
    Print["grid: ", grid//InputForm];
  ];

  Style[grid, ShowStringCharacters->False]
]]


insertFormatInspectionObjectsAsPills[InspectionObject[tag_, desc_, sev_, data_]] :=
  InspectionObject[tag, desc, sev, <| data, "FormatInspectionObjectsAsPills" -> True |>]

insertFormatInspectionObjectsAsPills[line_InspectedLineObject] := line



coalesce[list_] :=
  {{#[[1, 1]], #[[-1, 1]]}, {#[[1, 2]], #[[-1, 2]]}}& /@ Split[list, (#1[[1]] == #2[[1]] && #1[[2]] + 1 == #2[[2]])&]


(*
Grid has too much space in OutputForm, so use Column[{Row[], Row[]}]

Not possible to construct: ab
                           cd

with Grid in OutputForm. bug?


underlineList is not used in OutputForm
*)
Format[InspectedLineObject[lineSourceIn_String, lineNumber_Integer, {lineList_List, underlineList_List}, lints:{___InspectionObject}, opts___], OutputForm] :=
Catch[
Module[{maxLineNumberLength, paddedLineNumber, endingLints, elided, grid, endingAdditionalLintsAny,
  endingAdditionalLintsThisLine, lineSource},

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

  (*
  Make sure to sort lints
  *)
  endingLints = SortBy[endingLints, #[[4, Key[Source]]]&, conventionAgnosticSourceOrdering];

  endingLints = Take[endingLints, UpTo[$LintsPerLineLimit]];
  
  If[$Debug,
   Print["endingLints: ", endingLints];
  ];

  grid = Transpose /@
    Partition[
      Transpose[{lineList, underlineList}]
      ,
      UpTo[$LintedLineWidth]
    ];

  grid = If[MatchQ[#[[2]], {(" " | LintSpaceIndicatorCharacter)...}], Nothing, #]& /@ grid;

  grid = Flatten[grid, 1];

  Row[{Row[{paddedLineNumber, " "}],
      Column[{Column[Row /@ grid]} ~Join~
      endingLints] }]
]]


Format[InspectedLineObject[lineSourceIn_String, lineNumber_Integer, lineList_List, lints:{___InspectionObject}, opts___], StandardForm] :=
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

  (*
  add formatting instructions
  *)
  endingLints = insertFormatInspectionObjectsAsPills /@ endingLints;


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
    {{formatLeftColumn[lineSource, lineNumber, FilterRules[{opts}, Options[formatLeftColumn]]], Spacer[20],
      Column[{Grid[grid,
            Spacings -> {0, 0}, ItemSize -> $LintedLintItemSize] } ~Join~ endingLints ]}} ~Join~
    If[endingLints == {}, Sequence@@{}, {{Row[{Spacer[10]}]}}],
      Alignment -> Top, Spacings -> {0, 0}]
]]

Format[InspectedLineObject[lineSource_String, lineNumber_Integer, lineList_List, lints:{___InspectionObject}, opts___], OutputForm] :=
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
  
  Row[{Row[{paddedLineNumber, " "}], Column[{Column[Row /@ grid]} ~Join~ endingLints] }]
]]


Format[InspectedLineObject[row_List], StandardForm] :=
  Row[row]

Format[InspectedLineObject[row_List], OutputForm] :=
  Row[row]




Options[formatLeftColumn] = {
  "MaxLineNumberLength" -> 5
}

formatLeftColumn[lineSource_String, lineNumber_Integer, opts___] :=
Catch[
Module[{label, maxLineNumberLength, paddedLineNumber},
  
  maxLineNumberLength = OptionValue[formatLeftColumn, {opts}, "MaxLineNumberLength"];
  
  paddedLineNumber = StringPadLeft[ToString[lineNumber], maxLineNumberLength, " "];

  label = Style[Row[{paddedLineNumber, " "}], ShowStringCharacters->False];

  (*
  Copying in cloud:
  CLOUD-14729

  ActionMenu CopyToClipboard with Evaluator->None:
  bug 374583
  *)
  ActionMenu[
    Tooltip[
      label,
      "Click to open menu...", TooltipDelay -> Automatic], {
    "Copy line source" :> CopyToClipboard[lineSource],
    "Copy line number" :> CopyToClipboard[lineNumber]}, Appearance -> None, DefaultBaseStyle -> {}
  ]
]]





(*
bug 351153
Cannot use characters like \[SpaceIndicator] and \[ErrorIndicator] in OutputForm and
get Grid alignment
*)

LintSpaceIndicatorCharacter::usage = "LintSpaceIndicatorCharacter represents a space indicator in formatted output."

Format[LintSpaceIndicatorCharacter, StandardForm] := "\[SpaceIndicator]"
Format[LintSpaceIndicatorCharacter, OutputForm] := " "


LintErrorIndicatorCharacter::usage = "LintErrorIndicatorCharacter represents an error indicator in formatted output."

Format[LintErrorIndicatorCharacter, StandardForm] := "\[ErrorIndicator]"
Format[LintErrorIndicatorCharacter, OutputForm] := "^"


LintMissingOpenerIndicatorCharacter::usage = "LintMissingOpenerIndicatorCharacter represents an error indicator in formatted output."

Format[LintMissingOpenerIndicatorCharacter, StandardForm] := "\[ErrorIndicator]"
Format[LintMissingOpenerIndicatorCharacter, OutputForm] := "^"


LintMissingCloserIndicatorCharacter::usage = "LintMissingCloserIndicatorCharacter represents an error indicator in formatted output."

Format[LintMissingCloserIndicatorCharacter, StandardForm] := "\[ErrorIndicator]"
Format[LintMissingCloserIndicatorCharacter, OutputForm] := "^"


LintErrorContinuationIndicatorCharacter::usage = "LintErrorContinuationIndicatorCharacter represents an error continuation indicator in formatted output."

Format[LintErrorContinuationIndicatorCharacter, StandardForm] := "\[ErrorIndicator]"
Format[LintErrorContinuationIndicatorCharacter, OutputForm] := "~"



LintContinuationCharacter::usage = "LintContinuationCharacter represents a continuation in formatted output."

Format[LintContinuationCharacter, StandardForm] := "\[Continuation]"
Format[LintContinuationCharacter, OutputForm] := "\\"



Format[LintTimesCharacter, StandardForm] := "\[Times]"
Format[LintTimesCharacter, OutputForm] := "x"

(*
LintSpaceTimesCharacter is used in LSP clients to display a space before the \[Times]

But when using CodeInspectImplicitTokensSummarize, it is actually more coherent to simply use "\[Times]"

So format LintSpaceTimesCharacter the same as LintTimesCharacter

This also keeps all items in grids to a single character size so that $LintedLintItemSize is more predictable
*)
Format[LintSpaceTimesCharacter, StandardForm] := "\[Times]"
Format[LintSpaceTimesCharacter, OutputForm] := "x"

Format[LintOneCharacter, StandardForm] := "1"
Format[LintOneCharacter, OutputForm] := "1"

Format[LintAllCharacter, StandardForm] := "A"
Format[LintAllCharacter, OutputForm] := "A"

Format[LintNullCharacter, StandardForm] := "N"
Format[LintNullCharacter, OutputForm] := "N"

Format[LintTimesOneCharacter, StandardForm] := "y"
Format[LintTimesOneCharacter, OutputForm] := "y"

Format[LintExpectedOperandCharacter, StandardForm] := "\[EmptySquare]"
Format[LintExpectedOperandCharacter, OutputForm] := "e"

Format[LintAllTimesCharacter, StandardForm] := "B"
Format[LintAllTimesCharacter, OutputForm] := "B"

Format[LintAllOneCharacter, StandardForm] := "D"
Format[LintAllOneCharacter, OutputForm] := "D"

Format[LintAllTimesOneCharacter, StandardForm] := "C"
Format[LintAllTimesOneCharacter, OutputForm] := "C"


Format[LintOpenOneCharacter, StandardForm] := "1"
Format[LintOpenOneCharacter, OutputForm] := "1"

Format[LintAllCloseCharacter, StandardForm] := "A"
Format[LintAllCloseCharacter, OutputForm] := "A"

Format[LintOpenOpenCharacter, StandardForm] := " "
Format[LintOpenOpenCharacter, OutputForm] := " "

Format[LintCloseCloseCharacter, StandardForm] := " "
Format[LintCloseCloseCharacter, OutputForm] := " "

Format[LintCloseTimesCharacter, StandardForm] := "\[Times]"
Format[LintCloseTimesCharacter, OutputForm] := "x"

Format[LintExpectedOperandCloseCharacter, StandardForm] := "\[EmptySquare]"
Format[LintExpectedOperandCloseCharacter, OutputForm] := "e"

Format[LintExpectedOperandTimesCharacter, StandardForm] := "\[EmptySquare]"
Format[LintExpectedOperandTimesCharacter, OutputForm] := "e"

Format[LintOpenExpectedOperandCharacter, StandardForm] := "\[EmptySquare]"
Format[LintOpenExpectedOperandCharacter, OutputForm] := "e"

Format[LintCloseTimesOneCharacter, StandardForm] := "y"
Format[LintCloseTimesOneCharacter, OutputForm] := "y"


LintEOFCharacter::usage = "LintEOFCharacter represents an EOF in formatted output."

Format[LintEOFCharacter, StandardForm] := " "
Format[LintEOFCharacter, OutputForm] := " "


Format[LintUnhandledCharacter, StandardForm] := " "
Format[LintUnhandledCharacter, OutputForm] := " "






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
$UseANSI =
Catch[
  Switch[$OperatingSystem,
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
  ]
]

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




LintBold[content_] := LintMarkup[content, "Program", FontWeight->$LintTextFontWeight]

LintPreserve[content_] := LintMarkup[content]






colorANSICode[GrayLevel[gray_]] :=
With[{code = ToString[232 + Round[23 * gray]]},
  "\[RawEscape][38;5;"<>code<>"m"
]

colorANSICode[RGBColor[r_, g_, b_]] :=
With[{code = ToString[16 + {36, 6, 1} . Round[5 * {r, g, b}]]},
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
weightANSICode["SemiBold"] = "\[RawEscape][1m"
weightANSICode["Medium"] = ""
weightANSICode[Automatic] = ""

variationsANSICode[{"Underline"->True}] = "\[RawEscape][4m"
variationsANSICode[Automatic] = ""

resetANSICode[] = "\[RawEscape][0m"











$prettyTooltipTemplateDisplayFunction =
  Function[
    TagBox[
    #1,
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



$suggestionIconTemplateDisplayFunction =
  Function[
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
            ]
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
          {#2, #3, {16., 16.}},
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





End[]

EndPackage[]
