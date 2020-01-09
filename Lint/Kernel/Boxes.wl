BeginPackage["Lint`Boxes`"]

Begin["`Private`"]

Needs["AST`"]
Needs["Lint`"]
Needs["Lint`AbstractRules`"]
Needs["Lint`AggregateRules`"]
Needs["Lint`ConcreteRules`"]
Needs["Lint`Utils`"]





Options[LintBoxReport] = {
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

LintBoxReport[box_, lintsIn:{___Lint}:Automatic, OptionsPattern[]] :=
Catch[
 Module[{lints, lineNumberExclusions, lineHashExclusions, tagExclusions, severityExclusions,
  confidence, performanceGoal, concreteRules, aggregateRules, abstractRules,
  processedBox},

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
    lints = LintBox[box,
      PerformanceGoal -> performanceGoal,
      "ConcreteRules" -> concreteRules,
      "AggregateRules" -> aggregateRules,
      "AbstractRules" -> abstractRules];
  ];

  processedBox = box;
  Do[
    processedBox = processBox[processedBox, lint];
    ,
    {lint, lints}
  ];

  LintedBox[processedBox, lints]
]]






LintedBox::usage = "LintedBox[box] represents a formatted object of lints found in box."

Format[LintedBox[processedBox_, lints_], StandardForm] :=
Module[{},

  Interpretation[
    Framed[Column[{Row[{RawBoxes[processedBox]}, ImageMargins -> {{0, 0}, {10, 10}}]} ~Join~ lints, Left, 0], Background -> GrayLevel[0.97], RoundingRadius -> 5]
    ,
    processedBox]
]






processBox[box_, lint_] :=
Module[{sevColor, poss, processedBox},
  sevColor = severityColor[{lint}];
  poss = {lint[[4, Key[Source] ]]} ~Join~ Lookup[lint[[4]], "AdditionalSources", {}];
  processedBox = box;
  Scan[(
    processedBox = 
      ReplacePart[
        processedBox, # -> StyleBox[Extract[processedBox, #], FontVariations -> {"Underlight" -> Red}]];
    )&
    ,
    poss
  ];
  processedBox
]




End[]

EndPackage[]
