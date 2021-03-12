BeginPackage["CodeInspector`TokenRules`"]

$DefaultTokenRules


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`Utils`"]

Needs["PacletManager`"] (* for PacletInformation *)


(*
TODO: when targeting 12.1 as a minimum, then use paclet["AssetLocation", "BadSymbols"]
*)
location = "Location" /. PacletInformation["CodeInspector"];

WolframLanguageSyntax`Generate`$badSymbols = Get[FileNameJoin[{location, "Resources", "Data", "BadSymbols.wl"}]]

badSymbolsStringPat = Alternatives @@ WolframLanguageSyntax`Generate`$badSymbols




$DefaultTokenRules = <|

LeafNode[Symbol, _, _] -> scanSymbols,

Nothing
|>



Attributes[scanSymbols] = {HoldRest}

scanSymbols[pos_List, cstIn_] :=
Module[{cst, node, data, str, issues, src},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  str = node[[2]];
  data = node[[3]];
  src = data[Source];

  issues = {};

  Which[
    StringMatchQ[str, badSymbolsStringPat],
      Switch[str,
        "Failed",
          AppendTo[issues,
            InspectionObject["BadSymbol", "``Failed`` does not exist in **System`** context.", "Error",
              <|
                Source -> src,
                ConfidenceLevel -> 0.75,
                CodeActions -> {
                  CodeAction["Replace with ``$Failed``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[$Failed]|>]
                },
                "Argument" -> str
              |>
            ]
          ]
        ,
        "Boolean",
          AppendTo[issues,
            InspectionObject["BadSymbol", "``Boolean`` does not exist in **System`** context.", "Error",
              <|
                Source -> src,
                ConfidenceLevel -> 0.75,
                CodeActions -> {
                  CodeAction["Replace with ``True|False``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[True|False]|>]
                },
                "Argument" -> str
              |>
            ]
          ]
        ,
        "Match",
          AppendTo[issues,
            InspectionObject["BadSymbol", "``Match`` does not exist in **System`** context.", "Error",
              <|
                Source -> src,
                ConfidenceLevel -> 0.75,
                CodeActions -> {
                  CodeAction["Replace with ``MatchQ``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[MatchQ]|>]
                },
                "Argument" -> str
              |>
            ]
          ]
        ,
        "UnSameQ",
          AppendTo[issues,
            InspectionObject["BadSymbol", "``UnSameQ`` does not exist in **System`** context.", "Error",
              <|
                Source -> src,
                ConfidenceLevel -> 0.95,
                CodeActions -> {
                  CodeAction["Replace with ``UnsameQ``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[UnsameQ]|>]
                },
                "Argument" -> str
              |>
            ]
          ]
        ,
        "StringMatch",
          AppendTo[issues,
            InspectionObject["BadSymbol", "``StringMatch`` does not exist in **System`** context.", "Error",
              <|
                Source -> src,
                ConfidenceLevel -> 0.95,
                CodeActions -> {
                  CodeAction["Replace with ``StringMatchQ``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[StringMatchQ]|>]
                },
                "Argument" -> str
              |>
            ]
          ]
        ,
        "OptionsQ",
          AppendTo[issues,
            InspectionObject["BadSymbol", "``OptionsQ`` does not exist in **System`** context.", "Error",
              <|
                Source -> src,
                ConfidenceLevel -> 0.95,
                CodeActions -> {
                  CodeAction["Replace with ``OptionQ``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[OptionQ]|>]
                },
                "Argument" -> str
              |>
            ]
          ]
        ,
        "OptionPattern",
          AppendTo[issues,
            InspectionObject["BadSymbol", "``OptionPattern`` does not exist in **System`** context.", "Error",
              <|
                Source -> src,
                ConfidenceLevel -> 0.95,
                CodeActions -> {
                  CodeAction["Replace with ``OptionsPattern``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[OptionsPattern]|>]
                },
                "Argument" -> str
              |>
            ]
          ]
        ,
        "InterpolationFunction",
          AppendTo[issues,
            InspectionObject["BadSymbol", "``InterpolationFunction`` does not exist in **System`** context.", "Error",
              <|
                Source -> src,
                ConfidenceLevel -> 0.95,
                CodeActions -> {
                  CodeAction["Replace with ``InterpolatingFunction``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[InterpolatingFunction]|>]
                },
                "Argument" -> str
              |>
            ]
          ]
        ,
        "InterpolationPolynomial",
          AppendTo[issues,
            InspectionObject["BadSymbol", "``InterpolationPolynomial`` does not exist in **System`** context.", "Error",
              <|
                Source -> src,
                ConfidenceLevel -> 0.95,
                CodeActions -> {
                  CodeAction["Replace with ``InterpolatingPolynomial``", ReplaceNode, <|Source->src, "ReplacementNode"->ToNode[InterpolatingPolynomial]|>]
                },
                "Argument" -> str
              |>
            ]
          ]
        ,
        _,
          (* everything else without suggestions *)
          AppendTo[issues,
            InspectionObject["BadSymbol", "``" <> str <> "`` does not exist in **System`** context.", "Error",
              <|
                Source -> src,
                ConfidenceLevel -> 0.75,
                "Argument" -> str
              |>
            ]
          ]
      ]
    ,
    True,
      Null
  ];

  issues
]



End[]


EndPackage[]
