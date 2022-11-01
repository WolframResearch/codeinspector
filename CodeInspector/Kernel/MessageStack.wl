BeginPackage["CodeInspector`MessageStack`"]

codeWithMessageStackInspectAST


Begin["`Private`"]

Needs["CodeInspector`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Concretify`"]
Needs["CodeParser`Utils`"]


codeWithMessageStackInspectAST[ast_, {HoldForm[Message[MessageName[Part, "partw"], arg1_, arg2_]], rest___}] :=
Catch[
Module[{partCase, children, issues, replacementNode, f, r},

  partCase = FirstCase[ast, CallNode[LeafNode[Symbol, "Part", _], _, _], $Failed, Infinity];
  If[FailureQ[partCase],
    Throw[{}]
  ];

  children = partCase /. CallNode[LeafNode[Symbol, "Part", _], children_, _] :> children;
  Which[
    Length[children] < 2,
      Throw[{}]
    ,
    Length[children] > 2,

      f = children[[1]];
      r = children[[2;;]];

      replacementNode = Concretify[ContainerNode[Null, {CallNode[LeafNode[Symbol, "Indexed", <||>], {f, CallNode[LeafNode[Symbol, "List", <||>], r, <||>]}, <||>]}, <||>]][[2, 1]];
    ,
    True,
      replacementNode = Concretify[ContainerNode[Null, {CallNode[LeafNode[Symbol, "Indexed", <||>], children, <||>]}, <||>]][[2, 1]];
  ];

  issues = {};

  AppendTo[issues,
    (*
    remember to do ToString[..., StandardForm]
    bug 429482
    *)
    InspectionObject["Part", ToString[StringForm[MessageName[General, "partw"], arg1, arg2], StandardForm], "Error", <|
      Source -> children[[2, 3, Key[Source]]],
      ConfidenceLevel -> 0.95,
      CodeActions -> {
        CodeAction["Replace with ``Indexed``", ReplaceNode, <|
          "ReplacementNode" -> replacementNode, Source -> partCase[[3, Key[Source]]]
        |>]
      }
    |>]
  ];

  issues
]]


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

    Module[{posReplaced},

      posReplaced = pos /. s_String /; StringMatchQ[s, RegularExpression["`\\d+`"]] :> ReleaseHold[{args}[[ToExpression[StringTake[s, {2, -2}]]]]];
      
      Switch[posReplaced,
        _Integer,
          case = FirstCase[ast, CallNode[LeafNode[Symbol, namePat, _], children_, _] :> children, $Failed, Infinity];
          If[FailureQ[case],
            Throw[{}]
          ];
          If[Length[case] < posReplaced,
            Throw[{}]
          ];
          case = case[[posReplaced]];
          AppendTo[issues,
            (*
            remember to do ToString[..., StandardForm]
            bug 429482
            *)
            InspectionObject[name, ToString[StringForm[If[tryGeneral, MessageName[General, tag], MessageName[sym, tag]], args], StandardForm], "Error", <|Source -> case[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]
          ]
        ,
        _Symbol,
          case = FirstCase[ast, CallNode[LeafNode[Symbol, namePat, _], children_, _] :> children, $Failed, Infinity];
          If[FailureQ[case],
            Throw[{}]
          ];
          case = FirstCase[case, CallNode[LeafNode[Symbol, "Rule", _], {LeafNode[Symbol, ToString[posReplaced], _], value_}, _] :> value, $Failed, Infinity];
          If[FailureQ[case],
            Throw[{}]
          ];
          AppendTo[issues,
            (*
            remember to do ToString[..., StandardForm]
            bug 429482
            *)
            InspectionObject[name, ToString[StringForm[If[tryGeneral, MessageName[General, tag], MessageName[sym, tag]], args], StandardForm], "Error", <|Source -> case[[3, Key[Source]]], ConfidenceLevel -> 0.95|>]
          ]
        ,
        _,
          Message[codeWithMessageStackInspectAST::unhandled, posReplaced //InputForm];
          Null
      ];
    ]

  ] /@ poss;

  issues
]]


End[]

EndPackage[]
