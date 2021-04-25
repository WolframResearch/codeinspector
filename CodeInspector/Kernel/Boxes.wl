BeginPackage["CodeInspector`Boxes`"]

Begin["`Private`"]

Needs["CodeInspector`"]
Needs["CodeInspector`AbstractRules`"]
Needs["CodeInspector`AggregateRules`"]
Needs["CodeInspector`ConcreteRules`"]
Needs["CodeInspector`Summarize`"]
Needs["CodeInspector`SuppressedRegions`"]
Needs["CodeInspector`TokenRules`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


CodeInspect[nb_NotebookObject, opts:OptionsPattern[]] :=
  CodeInspect[NotebookGet[nb], opts]

CodeInspectSummarize[nb_NotebookObject, opts:OptionsPattern[]] :=
  CodeInspectSummarize[NotebookGet[nb], opts]


CodeInspect[cell_CellObject, opts:OptionsPattern[]] :=
  CodeInspect[NotebookRead[cell], opts]

CodeInspectSummarize[cell_CellObject, opts:OptionsPattern[]] :=
  CodeInspectSummarize[NotebookRead[cell], opts]


CodeInspect[nb_Notebook, opts:OptionsPattern[]] :=
Catch[
Module[{cst, suppressedRegions},

  cst = CodeConcreteParse[nb, FilterRules[{opts}, Options[CodeConcreteParse]]];

  If[FailureQ[cst],
    Throw[cst]
  ];

  suppressedRegions = SuppressedRegions[cst];

  (*
  IMPLEMENTATION DETAIL:
  it is important that CodeInspectCST is called on the CellNodes, so that
  the CellIndex property can be treated as inherited inside of CodeInspectCST and inserted
  into the InspectionObjects
  *)
  Flatten[If[FailureQ[#], {}, CodeInspectCST[#, FilterRules[{opts}, Options[CodeInspectCST]], "SuppressedRegions" -> suppressedRegions, "InheritedProperties" -> {CellIndex}]]& /@ cst[[2]]]
]]


CodeInspectSummarize[nbIn_Notebook, opts:OptionsPattern[]] :=
Module[{nb, lints, tagExclusions, severityExclusions, confidence, lintLimit},
  
  nb = nbIn;

  tagExclusions = OptionValue["TagExclusions"];
  severityExclusions = OptionValue["SeverityExclusions"];
  confidence = OptionValue[ConfidenceLevel];
  lintLimit = OptionValue["LintLimit"];

  lints = CodeInspect[nb, FilterRules[{opts}, Options[CodeInspect]]];

  (*
  TODO: use notebook title or thumbnail of notebook or something
  *)

  lints = filterLints[lints, tagExclusions, severityExclusions, confidence, lintLimit];


  If[lints == {},
    lints = {
      InspectedLineObject[{
        Column[{
          Text["Settings:"],
          ConfidenceLevel -> confidence,
          "LintLimit" -> lintLimit,
          "TagExclusions" -> tagExclusions,
          "SeverityExclusions" -> severityExclusions
        }]
      }],
      InspectedLineObject[{}],
      InspectedLineObject[{Text["No issues."]}]
    }
    ,
    lints = {
      InspectedLineObject[{
        Column[{
          Text["Settings:"],
          ConfidenceLevel -> confidence,
          "LintLimit" -> lintLimit,
          "TagExclusions" -> tagExclusions,
          "SeverityExclusions" -> severityExclusions
        }]
      }],
      InspectedLineObject[{}]
    } ~Join~ lints
  ];

  InspectedNotebookObject[Null, lints]
]


CodeInspect[c:Cell[BoxData[_], _, ___], opts:OptionsPattern[]] :=
Catch[
Module[{cst, suppressedRegions},

  cst = CodeConcreteParse[c, FilterRules[{opts}, Options[CodeConcreteParse]]];

  If[FailureQ[cst],
    Throw[cst]
  ];

  suppressedRegions = SuppressedRegions[cst];

  CodeInspectCST[cst, FilterRules[{opts}, Options[CodeInspectCST]], "SuppressedRegions" -> suppressedRegions]
]]

CodeInspect[Cell[___], opts:OptionsPattern[]] :=
  {}

CodeInspectSummarize[cIn_Cell, opts:OptionsPattern[]] :=
Module[{c, lints, tagExclusions, severityExclusions, confidence, lintLimit},
  
  c = cIn;

  tagExclusions = OptionValue["TagExclusions"];
  severityExclusions = OptionValue["SeverityExclusions"];
  confidence = OptionValue[ConfidenceLevel];
  lintLimit = OptionValue["LintLimit"];

  lints = CodeInspect[c, FilterRules[{opts}, Options[CodeInspect]]];

  lints = filterLints[lints, tagExclusions, severityExclusions, confidence, lintLimit];

  If[lints == {},
    lints = {
      InspectedLineObject[{
        Column[{
          Text["Settings:"],
          ConfidenceLevel -> confidence,
          "LintLimit" -> lintLimit,
          "TagExclusions" -> tagExclusions,
          "SeverityExclusions" -> severityExclusions
        }]
      }],
      InspectedLineObject[{}],
      InspectedLineObject[{Text["No issues."]}]
    }
    ,
    lints = {
      InspectedLineObject[{
        Column[{
          Text["Settings:"],
          ConfidenceLevel -> confidence,
          "LintLimit" -> lintLimit,
          "TagExclusions" -> tagExclusions,
          "SeverityExclusions" -> severityExclusions
        }]
      }],
      InspectedLineObject[{}]
    } ~Join~ lints
  ];

  (*
  TODO: use thumbnail of cell or something
  *)
  InspectedCellObject[Null, lints]
]


CodeInspect[b_RowBox, opts:OptionsPattern[]] :=
  CodeInspectBox[b, FilterRules[{opts}, Options[CodeInspectBox]]]

CodeInspectSummarize[b_RowBox, opts:OptionsPattern[]] :=
  CodeInspectBoxSummarize[b, FilterRules[{opts}, Options[CodeInspectBoxSummarize]]]



Options[CodeInspectBox] = {
  PerformanceGoal -> "Speed",
  "TokenRules" :> $DefaultTokenRules,
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules
  (*
  CodeConcreteParseBox has no options, so nothing to add here
  *)
}

CodeInspectBox[box_, opts:OptionsPattern[]] :=
Catch[
 Module[{cst, suppressedRegions},

  $ConcreteLintProgress = 0;
  $AggregateLintProgress = 0;
  $AbstractLintProgress = 0;
  $ConcreteLintTime = Quantity[0, "Seconds"];
  $AggregateLintTime = Quantity[0, "Seconds"];
  $AbstractLintTime = Quantity[0, "Seconds"];

  cst = CodeConcreteParseBox[box, FilterRules[{opts}, Options[CodeConcreteParseBox]]];

  If[FailureQ[cst],
    Throw[cst]
  ];

  suppressedRegions = SuppressedRegions[cst];

  (*
  session tokens such as % and Out are ok here
  *)
  Block[{CodeInspector`TokenRules`$ScanSessionTokens = False},
  CodeInspectCST[cst, FilterRules[{opts}, Options[CodeInspectCST]], "SuppressedRegions" -> suppressedRegions, "BatchMode" -> False, "KeepLowlevelScopingLints" -> False]
  ]
]]



CodeInspectBoxSummarize::usage = "CodeInspectBoxSummarize[box] returns a box inspection summary object."

Options[CodeInspectBoxSummarize] = {
  PerformanceGoal -> "Speed",
  "TokenRules" :> $DefaultTokenRules,
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  (*
  filtering
  *)
  "TagExclusions" :> $DefaultTagExclusions,
  "SeverityExclusions" :> $DefaultSeverityExclusions,
  ConfidenceLevel :> $DefaultConfidenceLevel,
  "LintLimit" :> $DefaultLintLimit
  (*
  CodeConcreteParseBox has no options, so nothing to add here
  *)
}

(*

There was a change in Mathematica 11.2 to allow 

foo[lints : {___Lint} : Automatic] := lints
foo[]  returns Automatic

Related bugs: 338218
*)

lintsInPat = If[$VersionNumber >= 11.2, {___InspectionObject}, _]

CodeInspectBoxSummarize[box_, lintsIn:lintsInPat:Automatic, opts:OptionsPattern[]] :=
Catch[
 Module[{lints, processedBox, cst, expandedLints,
  tagExclusions, severityExclusions, confidence, lintLimit},

 lints = lintsIn;

 tagExclusions = OptionValue["TagExclusions"];
 severityExclusions = OptionValue["SeverityExclusions"];
 confidence = OptionValue[ConfidenceLevel];
 lintLimit = OptionValue["LintLimit"];

 If[lints === Automatic,

    cst = CodeConcreteParseBox[box, FilterRules[{opts}, Options[CodeConcreteParseBox]]];

    If[FailureQ[cst],
      Throw[cst]
    ];

    suppressedRegions = SuppressedRegions[cst];

    lints = CodeInspectCST[cst, FilterRules[{opts}, Options[CodeInspectCST]], "SuppressedRegions" -> suppressedRegions];
  ];

  If[FailureQ[lints],
    Throw[lints]
  ];

  (*
  First, expand any AdditionalSources into their own "lints"
  *)
  expandedLints = Flatten[expandLint /@ lints];

  (*
  filterLints also sorts correctly
  *)
  expandedLints = filterLints[expandedLints, tagExclusions, severityExclusions, confidence, lintLimit];

  processedBox = box;
  Do[
    processedBox = replaceBox[processedBox, lint];
    ,
    {lint, expandedLints}
  ];

  (*
  Keep the original list of lints with "AdditionalSources"

  i.e. do not use expandedLints here
  *)

  (*
  Add a fake InspectedLineObject giving a listing of the settings used
  This can be easily removed if not wanted
  *)
  If[lints == {},
    lints = {
      InspectedLineObject[{
        Column[{
          Text["Settings:"],
          ConfidenceLevel -> confidence,
          "LintLimit" -> lintLimit,
          "TagExclusions" -> tagExclusions,
          "SeverityExclusions" -> severityExclusions
        }]
      }],
      InspectedLineObject[{}],
      InspectedLineObject[{Text["No issues."]}]
    }
    ,
    lints = {
      InspectedLineObject[{
        Column[{
          Text["Settings:"],
          ConfidenceLevel -> confidence,
          "LintLimit" -> lintLimit,
          "TagExclusions" -> tagExclusions,
          "SeverityExclusions" -> severityExclusions
        }]
      }],
      InspectedLineObject[{}]
    } ~Join~ lints
  ];

  InspectedBoxObject[processedBox, lints]
]]






InspectedBoxObject::usage = "InspectedBoxObject[box, lints] represents a formatted object of lints found in box."

Format[o:InspectedBoxObject[processedBoxIn_, lintsIn_], StandardForm] :=
Module[{lints, processedBox, col},

  lints = lintsIn;


  processedBox = processedBoxIn;

  processedBox = processedBox /. s_String :> StringReplace[s, $characterReplacementRules];

  
  If[TrueQ[CodeInspector`Format`$Attached],
    (*
    Attached, so delete any textual InspectedLineObjects
    *)
    lints = DeleteCases[lints, _InspectedLineObject]
  ];

  (*
  add formatting instructions
  *)
  lints = CodeInspector`Format`insertFormatInspectionObjectsAsPills /@ lints;

  If[MatchQ[processedBox, {___}],
    (*
    If the input is a list (meaning Cell[BoxData[{...}]]) then handle specially

    the top-level list is not a fully-general box

    Related bugs: 405218
    *)
    col = Column[(Row[{RawBoxes[#]}, ImageMargins -> {{0, 0}, {10, 10}}]& /@ processedBox) ~Join~ lints, Left, 0]
    ,
    col = Column[({Row[{RawBoxes[#]}, ImageMargins -> {{0, 0}, {10, 10}}]}&[processedBox]) ~Join~ lints, Left, 0]
  ];

  If[TrueQ[CodeInspector`Format`$Attached],
    (*
    $Attached is True, so probably running in CodeAssistance where InspectedBoxObjects are attached to the actual input
    *)
    Interpretation[
      (*
      no frame
      no background color

      looks better for duplicating the input and marking up in an AttachedCell underneath
      *)
      col
      ,
      o
    ]
    ,
    Interpretation[
      (*
      Framed
      Background color

      looks better when calling CodeInspectSummarize directly and you want a formatted object thing
      *)
      Framed[col, Background -> GrayLevel[0.97], RoundingRadius -> 5]
      ,
      o
    ]
  ]
]


Format[o:InspectedCellObject[Null, lintsIn_], StandardForm] :=
Module[{lints},

  lints = lintsIn;
  
  If[TrueQ[CodeInspector`Format`$Attached],
    (*
    Attached, so delete any textual InspectedLineObjects
    *)
    lints = DeleteCases[lints, _InspectedLineObject]
  ];

  (*
  add formatting instructions
  *)
  lints = CodeInspector`Format`insertFormatInspectionObjectsAsPills /@ lints;

  Interpretation[
    (*
    Framed
    Background color

    looks better when calling CodeInspectSummarize directly and you want a formatted object thing
    *)
    Framed[Column[{Row[{"Cell", "[", "\[Ellipsis]", "]"}, ImageMargins -> {{0, 0}, {10, 10}}]} ~Join~ lints, Left, 0], Background -> GrayLevel[0.97], RoundingRadius -> 5]
    ,
    o
  ]
]


Format[o:InspectedNotebookObject[Null, lintsIn_], StandardForm] :=
Module[{lints},

  lints = lintsIn;
  
  If[TrueQ[CodeInspector`Format`$Attached],
    (*
    Attached, so delete any textual InspectedLineObjects
    *)
    lints = DeleteCases[lints, _InspectedLineObject]
  ];

  (*
  add formatting instructions
  *)
  lints = CodeInspector`Format`insertFormatInspectionObjectsAsPills /@ lints;

  Interpretation[
    (*
    Framed
    Background color

    looks better when calling CodeInspectSummarize directly and you want a formatted object thing
    *)
    Framed[Column[{Row[{"Notebook", "[", "\[Ellipsis]", "]"}, ImageMargins -> {{0, 0}, {10, 10}}]} ~Join~ lints, Left, 0], Background -> GrayLevel[0.97], RoundingRadius -> 5]
    ,
    o
  ]
]


(*
Expand any AdditionalSources into their own lints
*)
expandLint[lint_] :=
  InspectionObject[lint[[1]], lint[[2]], lint[[3]],
    <|
      Source -> #,
      (*
      drop Source: already added
      drop "AdditionalSources": this already came from "AdditionalSources" !
      Keep all other keys, but we really need to keep e.g., ConfidenceLevel
      *)
      KeyDrop[lint[[4]], {Source, "AdditionalSources"}]
    |>
  ]& /@ {lint[[4, Key[Source]]]} ~Join~ Lookup[lint[[4]], "AdditionalSources", {}]



replaceBox[box_, lint_] :=
Module[{src, sevColor, processedBox, srcInter, extracted},

  src = lint[[4, Key[Source]]];
  sevColor = severityColor[{lint}];

  Switch[src,

    {___, Intra[___]},
      (*
      Intra
      *)
      srcInter = Most[src];

      (*
      srcInter could be {}, so make sure to accomodate that
      *)
      extracted = Extract[box, {srcInter}][[1]];

      processedBox = 
        ReplacePart[
          box, {srcInter} -> StyleBox[extracted, FontVariations -> {"Underlight" -> sevColor}]];

      processedBox
    ,
    After[_],
      (*
      After

      Just use the previous
      *)

      src = src[[1]];

      (*
      src could be {}, so make sure to accomodate that
      *)
      extracted = Extract[box, {src}][[1]];

      processedBox = 
        ReplacePart[
          box, {src} -> StyleBox[extracted, FontVariations -> {"Underlight" -> sevColor}]];

      processedBox
    ,
    Before[_],
      (*
      Before

      Just use the next
      *)

      src = src[[1]];

      (*
      src could be {}, so make sure to accomodate that
      *)
      extracted = Extract[box, {src}][[1]];

      processedBox = 
        ReplacePart[
          box, {src} -> StyleBox[extracted, FontVariations -> {"Underlight" -> sevColor}]];

      processedBox
    ,
    Span[_, _],
      (*
      Span
      *)

      (*
      FIXME: cannot use Extract with Span[{1}, {3}], so skip for now
      FIXME: cannot use ReplacePart with Span, so skip for now
      *)
      box
    ,
    _,
      (*
      There is no Intra | After | Before | Span in the position, so we can just use ReplacePart

      src could be {}, so make sure to accomodate that
      *)
      
      extracted = Extract[box, {src}][[1]];

      processedBox = 
        ReplacePart[
          box, {src} -> StyleBox[extracted, FontVariations -> {"Underlight" -> sevColor}]];

      processedBox
  ]
]




End[]

EndPackage[]
