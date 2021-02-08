BeginPackage["CodeInspector`Boxes`"]

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeInspector`"]
Needs["CodeInspector`AbstractRules`"]
Needs["CodeInspector`AggregateRules`"]
Needs["CodeInspector`ConcreteRules`"]
Needs["CodeInspector`Summarize`"]
Needs["CodeInspector`Utils`"]





CodeInspectBoxSummarize::usage = "CodeInspectBoxSummarize[box] returns a box inspection summary object."

Options[CodeInspectBoxSummarize] = {
  PerformanceGoal -> "Speed",
  "ConcreteRules" :> $DefaultConcreteRules,
  "AggregateRules" :> $DefaultAggregateRules,
  "AbstractRules" :> $DefaultAbstractRules,
  CharacterEncoding -> "UTF-8",
  "TagExclusions" -> $DefaultTagExclusions,
  "SeverityExclusions" -> $DefaultSeverityExclusions,
  "LineNumberExclusions" -> <||>,
  "LineHashExclusions" -> {},
  ConfidenceLevel :> $ConfidenceLevel
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
 Module[{lints, lineNumberExclusions, lineHashExclusions, tagExclusions, severityExclusions,
  confidence, performanceGoal, concreteRules, aggregateRules, abstractRules,
  processedBox, cst},

 lints = lintsIn;

 performanceGoal = OptionValue[PerformanceGoal];
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

 lineNumberExclusions = OptionValue["LineNumberExclusions"];
 If[lineNumberExclusions === None,
  lineNumberExclusions = {}
 ];

 lineHashExclusions = OptionValue["LineHashExclusions"];
 If[lineHashExclusions === None,
  lineHashExclusions = {}
 ];

 confidence = OptionValue[ConfidenceLevel];

 If[lints === Automatic,

    cst = CodeConcreteParseBox[box];

    lints = CodeInspectCST[cst,
      PerformanceGoal -> performanceGoal,
      "ConcreteRules" -> concreteRules,
      "AggregateRules" -> aggregateRules,
      "AbstractRules" -> abstractRules];
  ];

  If[FailureQ[lints],
    Throw[lints]
  ];

  processedBox = box;
  Do[
    processedBox = processBox[processedBox, lint];
    ,
    {lint, lints}
  ];

  InspectedBoxObject[processedBox, lints]
]]






InspectedBoxObject::usage = "InspectedBoxObject[box] represents a formatted object of lints found in box."

Format[InspectedBoxObject[processedBoxIn_, lints_], StandardForm] :=
Module[{processedBox},

  processedBox = processedBoxIn;

  processedBox = processedBox /. s_String :> StringReplace[s, $characterReplacementRules];

  Interpretation[
    Framed[Column[{Row[{RawBoxes[processedBox]}, ImageMargins -> {{0, 0}, {10, 10}}]} ~Join~ lints, Left, 0], Background -> GrayLevel[0.97], RoundingRadius -> 5]
    ,
    processedBox]
]






processBox[box_, lint_] :=
Module[{sevColor, srcs, processedBox},
  sevColor = severityColor[{lint}];
  srcs = {lint[[4, Key[Source] ]]} ~Join~ Lookup[lint[[4]], "AdditionalSources", {}];

  processedBox = box;
  Scan[(processedBox = replaceBox[processedBox, #, sevColor])&, srcs];

  processedBox
]



replaceBox[box_, srcIn_, sevColor_] :=
Module[{src, processedBox, srcInter, extracted},

  src = srcIn;

  Switch[src,

    {___, Intra[___]},
      (*
      Intra
      *)
      srcInter = Most[src];

      extracted = Extract[box, {srcInter}][[1]];

      processedBox = 
        ReplacePart[
          box, srcInter -> StyleBox[extracted, FontVariations -> {"Underlight" -> sevColor}]];

      processedBox
    ,
    After[_],
      (*
      After

      Just use the previous
      *)

      src = src[[1]];

      extracted = Extract[box, src];

      processedBox = 
        ReplacePart[
          box, src -> StyleBox[extracted, FontVariations -> {"Underlight" -> sevColor}]];

      processedBox
    ,
    _,
      (*
      There is no Intra | After in the position, so we can just use ReplacePart
      *)
      
      extracted = Extract[box, src];

      processedBox = 
        ReplacePart[
          box, src -> StyleBox[extracted, FontVariations -> {"Underlight" -> sevColor}]];

      processedBox
  ]
]




End[]

EndPackage[]
