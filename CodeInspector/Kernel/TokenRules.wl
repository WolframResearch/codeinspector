BeginPackage["CodeInspector`TokenRules`"]

$DefaultTokenRules


$ScanSessionTokens


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`Utils`"]

Needs["PacletManager`"] (* for PacletInformation *)



(*
A global to control special handling of session tokens:

special tokens such as:
%
%%
%45

and also symbols:
In
Out
etc.

*)
$ScanSessionTokens = True



(*
TODO: when targeting 12.1 as a minimum, then use paclet["AssetLocation", "BadSymbols"]
*)
location = "Location" /. PacletInformation["CodeInspector"];

WolframLanguageSyntax`Generate`$badSymbols = Get[FileNameJoin[{location, "Resources", "Data", "BadSymbols.wl"}]]
WolframLanguageSyntax`Generate`$sessionSymbols = Get[FileNameJoin[{location, "Resources", "Data", "SessionSymbols.wl"}]]
WolframLanguageSyntax`Generate`$undocumentedSymbols = Get[FileNameJoin[{location, "Resources", "Data", "UndocumentedSymbols.wl"}]]

badSymbolsStringPat = Alternatives @@ WolframLanguageSyntax`Generate`$badSymbols
sessionSymbolsStringPat = Alternatives @@ WolframLanguageSyntax`Generate`$sessionSymbols
undocumentedSymbolsStringPat = Alternatives @@ WolframLanguageSyntax`Generate`$undocumentedSymbols

allSymbolsPat = Alternatives @@ Join[
    WolframLanguageSyntax`Generate`$badSymbols,
    WolframLanguageSyntax`Generate`$sessionSymbols,
    WolframLanguageSyntax`Generate`$undocumentedSymbols
  ]


$badSymbolsWithSuggestions = <|
  "Failed" -> {0.75, "$Failed"},
  (* low confidence because people do define their own Match *)
  "Match" -> {0.5, "MatchQ"},
  "UnSameQ" -> {0.95, "UnsameQ"},
  (* low confidence because people do define their own StringMatch *)
  "StringMatch" -> {0.5, "StringMatchQ"},
  "OptionsQ" -> {0.95, "OptionQ"},
  "OptionPattern" -> {0.95, "OptionsPattern"},
  "InterpolationFunction" -> {0.95, "InterpolatingFunction"},
  "InterpolationPolynomial" -> {0.95, "InterpolatingPolynomial"},
  (* low confidence because people do define their own RealQ *)
  "RealQ" -> {0.5, "Developer`RealQ"},
  (* low confidence because people do define their own SymbolQ *)
  "SymbolQ" -> {0.5, "Developer`SymbolQ"},
  "DataSet" -> {0.95, "Dataset"},
  "UrlExecute" -> {0.95, "URLExecute"},
  "Cloudbase" -> {0.95, "CloudBase"},
  "ExpandFilename" -> {0.95, "ExpandFileName"},
  "$PathNameSeparator" -> {0.95, "$PathnameSeparator"},
  "$RegisteredUsername" -> {0.95, "$RegisteredUserName"}
|>

$badSymbolsNoSuggestions = <|
  (*
  low confidence because people do define their own Boolean
  could suggest True|False
  *)
  "Boolean" -> {0.5},
  (* low confidence because people do define their own FalseQ *)
  "FalseQ" -> {0.5},
  (* low confidence because people do define their own RationalQ *)
  "RationalQ" -> {0.5},
  (* low confidence because people do define their own ComplexQ *)
  "ComplexQ" -> {0.5}
|>

$undocumentedSymbolsWithSuggestions = <|
  "$UserName" -> {0.95, "$Username"}
|>



$DefaultTokenRules = <|

LeafNode[Symbol, allSymbolsPat, _] -> scanSymbols,

LeafNode[Token`Percent | Token`PercentPercent, _, _] | CompoundNode[Out, _, _] /; $ScanSessionTokens -> scanSessionTokens,

Nothing
|>



Attributes[scanSymbols] = {HoldRest}

scanSymbols[pos_List, cstIn_] :=
Module[{cst, node, data, str, issues, src, withSuggestion,
  noSuggestion},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  str = node[[2]];
  data = node[[3]];
  src = data[Source];

  issues = {};

  Which[
    StringMatchQ[str, badSymbolsStringPat],
      Which[
        !MissingQ[(withSuggestion = $badSymbolsWithSuggestions[str])],
          AppendTo[issues,
            InspectionObject["BadSymbol", "``" <> str <> "`` does not exist in **System`** context.", "Error",
              <|
                Source -> src,
                ConfidenceLevel -> withSuggestion[[1]],
                CodeActions -> {
                  CodeAction["Replace with ``" <> withSuggestion[[2]] <> "``", ReplaceNode, <|
                    Source -> src,
                    "ReplacementNode" -> LeafNode[Symbol, withSuggestion[[2]], <||>]
                  |>]
                },
                "Argument" -> str
              |>
            ]
          ]
        ,
        !MissingQ[(noSuggestion = $badSymbolsNoSuggestions[str])],
          AppendTo[issues,
            InspectionObject["BadSymbol", "``" <> str <> "`` does not exist in **System`** context.", "Error",
              <|
                Source -> src,
                ConfidenceLevel -> withSuggestion[[1]],
                "Argument" -> str
              |>
            ]
          ]
        ,
        True,
          (* everything else without suggestions or custom ConfidenceLevel*)
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
    $ScanSessionTokens && StringMatchQ[str, sessionSymbolsStringPat],
      AppendTo[issues,
        InspectionObject["SuspiciousSessionSymbol", "Suspicious use of session symbol " <> format[str] <> ".", "Warning",
          <|
            Source -> src,
            ConfidenceLevel -> 0.55
          |>
        ]
      ]
    ,
    StringMatchQ[str, undocumentedSymbolsStringPat],
      Which[
        !MissingQ[(withSuggestion = $undocumentedSymbolsWithSuggestions[str])],
          AppendTo[issues,
            InspectionObject["UndocumentedSymbol", "``" <> str <> "`` is undocumented.", "Remark",
              <|
                Source -> src,
                ConfidenceLevel -> withSuggestion[[1]],
                CodeActions -> {
                  CodeAction["Replace with ``" <> withSuggestion[[2]] <> "``", ReplaceNode, <|
                    Source -> src,
                    "ReplacementNode" -> LeafNode[Symbol, withSuggestion[[2]], <||>]
                  |>]
                },
                "Argument" -> str
              |>
            ]
          ]
        ,
        True,
          (*
          everything else without suggestions or custom ConfidenceLevel
          
          do not give any lints here

          undocumented symbols are generally ok
          *)
          Null
      ]
    ,
    True,
      Null
  ];

  issues
]



Attributes[scanSessionTokens] = {HoldRest}

scanSessionTokens[pos_List, cstIn_] :=
Module[{cst, node, data, str, issues, src},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  str = node[[2]];
  data = node[[3]];
  src = data[Source];

  issues = {};

  AppendTo[issues,
    InspectionObject["SuspiciousSessionToken", "Suspicious use of session token.", "Error",
      <|
        Source -> src,
        ConfidenceLevel -> 0.95
      |>
    ]
  ];

  issues
]


End[]


EndPackage[]
