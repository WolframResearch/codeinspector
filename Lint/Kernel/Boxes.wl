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

(*

There was a change in Mathematica 11.2 to allow 

foo[lints : {___Lint} : Automatic] := lints
foo[]  returns Automatic

Related bugs: 338218
*)

lintsInPat = If[$VersionNumber >= 11.2, {___Lint}, _]

LintBoxReport[box_, lintsIn:lintsInPat:Automatic, OptionsPattern[]] :=
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
Module[{sevColor, srcs, processedBox},
  sevColor = severityColor[{lint}];
  srcs = {lint[[4, Key[Source] ]]} ~Join~ Lookup[lint[[4]], "AdditionalSources", {}];

  processedBox = box;
  Scan[(processedBox = replaceBox[processedBox, #, sevColor])&, srcs];

  processedBox = processedBox /. s_String :> StringReplace[s, $characterReplacementRules];

  processedBox
]



replaceBox[box_, src_, sevColor_] :=
Catch[
Module[{processedBox, srcInter},

  If[!MatchQ[Last[src], Intra[___]],
      (*
      There is no Intra in the position, so we can just use ReplacePart
      *)
      
      extracted = Extract[box, src];

      processedBox = 
        ReplacePart[
          box, src -> StyleBox[extracted, FontVariations -> {"Underlight" -> sevColor}]];

      Throw[processedBox]
  ];

  (*
  There is Intra in the position
  *)
  srcInter = Most[src];

  extracted = Extract[box, {srcInter}][[1]];

  processedBox = 
    ReplacePart[
      box, srcInter -> StyleBox[extracted, FontVariations -> {"Underlight" -> sevColor}]];

  processedBox
]]




End[]

EndPackage[]
