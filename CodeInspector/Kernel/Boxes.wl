BeginPackage["CodeInspector`Boxes`"]

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeInspector`"]
Needs["CodeInspector`AbstractRules`"]
Needs["CodeInspector`AggregateRules`"]
Needs["CodeInspector`ConcreteRules`"]
Needs["CodeInspector`Summarize`"]
Needs["CodeInspector`SuppressedRegions`"]
Needs["CodeInspector`TokenRules`"]
Needs["CodeInspector`Utils`"]


CodeInspect[nb_NotebookObject, opts:OptionsPattern[]] :=
  CodeInspect[NotebookGet[nb]]

CodeInspectSummarize[nb_NotebookObject, opts:OptionsPattern[]] :=
  CodeInspectSummarize[NotebookGet[nb]]


CodeInspect[cell_CellObject, opts:OptionsPattern[]] :=
  CodeInspect[NotebookRead[cell]]

CodeInspectSummarize[cell_CellObject, opts:OptionsPattern[]] :=
  CodeInspectSummarize[NotebookRead[cell]]


CodeInspect[nb_Notebook, opts:OptionsPattern[]] :=
Module[{cst, suppressedRegions},

  cst = CodeConcreteParse[nb];
  suppressedRegions = SuppressedRegions[cst];
  (*
  IMPLEMENTATION DETAIL:
  it is important that CodeInspectCST is called on the CellNodes, so that
  the CellIndex property can be treated as inherited inside of CodeInspectCST and inserted
  into the InspectionObjects
  *)
  Flatten[If[FailureQ[#], {}, CodeInspectCST[#, opts, "SuppressedRegions" -> suppressedRegions, "InheritedProperties" -> {CellIndex}]]& /@ cst[[2]]]
]


CodeInspectSummarize[nbIn_Notebook, opts:OptionsPattern[]] :=
Module[{nb, lints, tagExclusions, severityExclusions, confidence, lintLimit},
  
  nb = nbIn;

  (*
  Support None for the various exclusion options
  *)
  tagExclusions = OptionValue["TagExclusions"];
  If[tagExclusions === None,
    tagExclusions = {}
  ];

  severityExclusions = OptionValue["SeverityExclusions"];
  If[severityExclusions === None,
    severityExclusions = {}
  ];

  confidence = OptionValue[ConfidenceLevel];

  lintLimit = OptionValue["LintLimit"];

  lints = CodeInspect[nb];

  (*
  TODO: use notebook title or thumbnail of notebook or something
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
      InspectedLineObject[{}],
      InspectedLineObject[{}]
    } ~Join~ lints
  ];

  InspectedNotebookObject[Null, lints]
]


CodeInspect[c:Cell[BoxData[_], _, ___], opts:OptionsPattern[]] :=
  CodeInspectCST[CodeConcreteParse[c], opts]

CodeInspect[Cell[___], opts:OptionsPattern[]] :=
  {}

CodeInspectSummarize[cIn_Cell, opts:OptionsPattern[]] :=
Module[{c, lints, tagExclusions, severityExclusions, confidence, lintLimit},
  
  c = cIn;

  (*
  Support None for the various exclusion options
  *)
  tagExclusions = OptionValue["TagExclusions"];
  If[tagExclusions === None,
    tagExclusions = {}
  ];

  severityExclusions = OptionValue["SeverityExclusions"];
  If[severityExclusions === None,
    severityExclusions = {}
  ];

  confidence = OptionValue[ConfidenceLevel];

  lintLimit = OptionValue["LintLimit"];

  lints = CodeInspect[c];

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
      InspectedLineObject[{}],
      InspectedLineObject[{}]
    } ~Join~ lints
  ];

  (*
  TODO: use thumbnail of cell or something
  *)
  InspectedCellObject[Null, lints]
]


CodeInspect[b_RowBox, opts:OptionsPattern[]] :=
  CodeInspectBox[b, opts]

CodeInspectSummarize[b_RowBox, opts:OptionsPattern[]] :=
  CodeInspectBoxSummarize[b, opts]



Options[CodeInspectBox] = {
  PerformanceGoal -> "Speed",
  "TokenRules" :> $DefaultTokenRules,
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules
}

CodeInspectBox[box_, OptionsPattern[]] :=
Catch[
 Module[{aggregateRules, abstractRules, cst, concreteRules, performanceGoal, suppressedRegions, tokenRules},

  performanceGoal = OptionValue[PerformanceGoal];
  tokenRules = OptionValue["TokenRules"];
  concreteRules = OptionValue["ConcreteRules"];
  aggregateRules = OptionValue["AggregateRules"];
  abstractRules = OptionValue["AbstractRules"];

  $ConcreteLintProgress = 0;
  $AggregateLintProgress = 0;
  $AbstractLintProgress = 0;
  $ConcreteLintTime = Quantity[0, "Seconds"];
  $AggregateLintTime = Quantity[0, "Seconds"];
  $AbstractLintTime = Quantity[0, "Seconds"];

  cst = CodeConcreteParseBox[box];

  If[FailureQ[cst],
    Throw[cst]
  ];

  suppressedRegions = SuppressedRegions[cst];

  CodeInspectCST[
    cst,
    PerformanceGoal -> performanceGoal,
    "TokenRules" -> tokenRules,
    "ConcreteRules" -> concreteRules,
    "AggregateRules" -> aggregateRules,
    "AbstractRules" -> abstractRules,
    "SuppressedRegions" -> suppressedRegions
  ]
]]



CodeInspectBoxSummarize::usage = "CodeInspectBoxSummarize[box] returns a box inspection summary object."

Options[CodeInspectBoxSummarize] = {
  PerformanceGoal -> "Speed",
  "TokenRules" :> $DefaultTokenRules,
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  CharacterEncoding -> "UTF-8",
  "TagExclusions" -> $DefaultTagExclusions,
  "SeverityExclusions" -> $DefaultSeverityExclusions,
  "LineNumberExclusions" -> <||>,
  "LineHashExclusions" -> {},
  ConfidenceLevel :> $DefaultConfidenceLevel,
  "LintLimit" :> $DefaultLintLimit
}

(*

There was a change in Mathematica 11.2 to allow 

foo[lints : {___Lint} : Automatic] := lints
foo[]  returns Automatic

Related bugs: 338218
*)

lintsInPat = If[$VersionNumber >= 11.2, {___InspectionObject}, _]

CodeInspectBoxSummarize[box_, lintsIn:lintsInPat:Automatic, OptionsPattern[]] :=
Catch[
 Module[{lints, tagExclusions, severityExclusions,
  confidence, lintLimit, performanceGoal, concreteRules, aggregateRules, abstractRules,
  processedBox, cst, expandedLints, tokenRules},

 lints = lintsIn;

 performanceGoal = OptionValue[PerformanceGoal];
 tokenRules = OptionValue["TokenRules"];
 concreteRules = OptionValue["ConcreteRules"];
 aggregateRules = OptionValue["AggregateRules"];
 abstractRules = OptionValue["AbstractRules"];

 (*
  Support None for the various exclusion options
 *)
 tagExclusions = OptionValue["TagExclusions"];
 If[tagExclusions === None,
  tagExclusions = {}
 ];

 severityExclusions = OptionValue["SeverityExclusions"];
 If[severityExclusions === None,
  severityExclusions = {}
 ];

 confidence = OptionValue[ConfidenceLevel];

 lintLimit = OptionValue["LintLimit"];

 If[lints === Automatic,

    cst = CodeConcreteParseBox[box];

    lints = CodeInspectCST[cst,
      PerformanceGoal -> performanceGoal,
      "TokenRules" -> tokenRules,
      "ConcreteRules" -> concreteRules,
      "AggregateRules" -> aggregateRules,
      "AbstractRules" -> abstractRules];
  ];

  If[FailureQ[lints],
    Throw[lints]
  ];

  (*
  First, expand any AdditionalSources into their own "lints"
  *)
  expandedLints = Flatten[expandLint /@ lints];

  (*
  Then sort

  given the srcs {{1, 3}, {1, 3, 1, 1}}

  it is important to process {1, 3, 1, 1} first because adding the StyleBox changes the shape of box, so must work from more-specific to less-specific positions

  For example, the boxes of this expression:
  f[%[[]]]

  which are:
  RowBox[{"f", "[", RowBox[{"%", "[", RowBox[{"[", "]"}], "]"}], "]"}]

  give lints with positions {1, 3} and {1, 3, 1, 1}
  *)
  expandedLints = ReverseSortBy[expandedLints, #[[4, Key[Source]]]&, lexOrderingForLists];

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
      InspectedLineObject[{}],
      InspectedLineObject[{}]
    } ~Join~ lints
  ];

  InspectedBoxObject[processedBox, lints]
]]






InspectedBoxObject::usage = "InspectedBoxObject[box, lints] represents a formatted object of lints found in box."

Format[o:InspectedBoxObject[processedBoxIn_, lintsIn_], StandardForm] :=
Module[{lints, processedBox},

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
      Column[{Row[{RawBoxes[processedBox]}, ImageMargins -> {{0, 0}, {10, 10}}]} ~Join~ lints, Left, 0]
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
      Framed[Column[{Row[{RawBoxes[processedBox]}, ImageMargins -> {{0, 0}, {10, 10}}]} ~Join~ lints, Left, 0], Background -> GrayLevel[0.97], RoundingRadius -> 5]
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
  InspectionObject[lint[[1]], lint[[2]], lint[[3]], <|Source -> #|>]& /@ {lint[[4, Key[Source]]]} ~Join~ Lookup[lint[[4]], "AdditionalSources", {}]



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
