BeginPackage["CodeInspector`MessageStack`"]

codeWithMessageStackInspectAST


Begin["`Private`"]

Needs["CodeInspector`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


codeWithMessageStackInspectAST[ast_, {HoldForm[Message[MessageName[sym_, tag_], args___]], rest___}] :=
Catch[
Module[{case, name, tryGeneral, namePat, poss, issues},

  tryGeneral = False;

  Which[
    KeyExistsQ[WolframLanguageSyntax`Generate`$analyzableMessagePositions, HoldForm[MessageName[sym, tag]]],
      poss = WolframLanguageSyntax`Generate`$analyzableMessagePositions[HoldForm[MessageName[sym, tag]]];
    ,
    KeyExistsQ[WolframLanguageSyntax`Generate`$analyzableMessagePositions, HoldForm[MessageName[General, tag]]],
      tryGeneral = True;
      poss = WolframLanguageSyntax`Generate`$analyzableMessagePositions[HoldForm[MessageName[General, tag]]];
    ,
    True,
      Throw[{}]
  ];

  name = SymbolName[Unevaluated[sym]];

  Which[
    name == "Import",
      namePat = "Import" | "ImportString"
    ,
    True,
      namePat = name
  ];


  issues = {};

  Function[{pos},

    Switch[pos,
      _Integer,
        case = FirstCase[ast, CallNode[LeafNode[Symbol, namePat, _], children_, _] :> children, $Failed, Infinity];
        If[FailureQ[case],
          Throw[{}]
        ];
        If[Length[case] < pos,
          Throw[{}]
        ];
        case = case[[pos]];
        AppendTo[issues, InspectionObject[name, ToString[StringForm[If[tryGeneral, MessageName[General, tag], MessageName[sym, tag]], args]], "Error", <|Source -> case[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]]
      ,
      _Symbol,
        case = FirstCase[ast, CallNode[LeafNode[Symbol, namePat, _], children_, _] :> children, $Failed, Infinity];
        If[FailureQ[case],
          Throw[{}]
        ];
        case = FirstCase[case, CallNode[LeafNode[Symbol, "Rule", _], {LeafNode[Symbol, ToString[pos], _], value_}, _] :> value, $Failed, Infinity];
        If[FailureQ[case],
          Throw[{}]
        ];
        AppendTo[issues, InspectionObject[name, ToString[StringForm[If[tryGeneral, MessageName[General, tag], MessageName[sym, tag]], args]], "Error", <|Source -> case[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]]
    ];

  ] /@ poss;

  issues
]]



End[]

EndPackage[]
