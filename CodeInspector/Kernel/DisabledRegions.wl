BeginPackage["CodeInspector`DisabledRegions`"]

DisabledRegions

Begin["`Private`"]

Needs["CodeParser`"]


(*

Please use this syntax:

(* CodeInspect::Push *)
(* CodeInspect::Disable::DuplicateClausesIf *)

If[a, b, b]

(* CodeInspect::Pop *)


*)


codeInspectBeginPat = LeafNode[Token`Comment, "(* ::CodeInspect::Push:: *)" | "(* CodeInspect::Begin *)" | "(* CodeInspect::Push *)", _]

codeInspectEndPat = LeafNode[Token`Comment, "(* ::CodeInspect::Pop:: *)" | "(* CodeInspect::End *)" | "(* CodeInspect::Pop *)", _]

codeInspectDisablePat =
  LeafNode[
    Token`Comment,
    str_String /;
      StringMatchQ[str,
        ("(* ::CodeInspect::Disable::" ~~ LetterCharacter... ~~ ":: *)") |
        ("(* CodeInspect::Disable::" ~~ LetterCharacter... ~~ " *)") |
        ("(* ::CodeInspect::Disable::" ~~ LetterCharacter... ~~ "::" ~~ LetterCharacter... ~~ ":: *)") |
        ("(* CodeInspect::Disable::" ~~ LetterCharacter... ~~ "::" ~~ LetterCharacter... ~~ " *)")
      ],
    _
  ]



DisabledRegions[cstIn_] :=
Catch[
Module[{cst, codeInspectBeginPatNodePoss, disabledRegions, siblingsPos, siblings, endFound, candidate, endPos, disableds},

  cst = cstIn;

  codeInspectBeginPatNodePoss = Position[cst, codeInspectBeginPat];

  disabledRegions = {};
  Do[
    siblingsPos = Most[beginPos];
    siblings = Extract[cst, {siblingsPos}][[1]];
    endFound = False;
    disableds = {};
    Do[
      candidate = siblings[[pos]];
      Switch[candidate,
        codeInspectEndPat,
          endPos = Most[beginPos] ~Join~ {pos};
          endFound = True;
          Break[]
        ,
        codeInspectDisablePat,
          disableds = disableds ~Join~
            StringCases[candidate[[2]], {
              "(* ::CodeInspect::Disable::" ~~ d:LetterCharacter... ~~ ":: *)" :> {d},
              "(* CodeInspect::Disable::" ~~ d:LetterCharacter... ~~ " *)" :> {d},
              "(* ::CodeInspect::Disable::" ~~ d:LetterCharacter... ~~ "::" ~~ a:LetterCharacter... ~~ ":: *)" :> {d, a},
              "(* CodeInspect::Disable::" ~~ d:LetterCharacter... ~~ "::" ~~ a:LetterCharacter... ~~ " *)" :> {d, a}
            }]
      ]
      ,
      {pos, Last[beginPos]+1, Length[siblings]}
    ];
    If[endFound,
      AppendTo[disabledRegions, {rangeStart[Extract[cst, beginPos][[3, Key[Source]]]], rangeEnd[Extract[cst, endPos][[3, Key[Source]]]], disableds}]
      ,
      Message[DisabledRegions::missingpop, cst[[3]]];
      Throw[{}]
    ]
    ,
    {beginPos, codeInspectBeginPatNodePoss}
  ];

  disabledRegions
]]


(*
For LineColumn convention, return the start or end span
*)
rangeStart[{s:{_Integer, _Integer}, {_Integer, _Integer}}] :=
  s

rangeEnd[{{_Integer, _Integer}, e:{_Integer, _Integer}}] :=
  e


(*
For SourceCharacterIndex convention, return the start or end span
*)
rangeStart[{s:_Integer, _Integer}] :=
  s

rangeEnd[{_Integer, e:_Integer}] :=
  e


(*
For other conventions, just return the src

We do not know what to do
*)
rangeStart[src_] :=
  src

rangeEnd[src_] :=
  src


End[]

EndPackage[]
