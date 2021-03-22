BeginPackage["CodeInspector`DisabledRegions`"]

DisabledRegions

Begin["`Private`"]

Needs["CodeParser`"]


codeInspectPushPat = LeafNode[Token`Comment, "(* " <> "::CodeInspect::Push::" <> " *)", _]
codeInspectPopPat = LeafNode[Token`Comment, "(* " <> "::CodeInspect::Pop::" <> " *)", _]

codeInspectDisablePat = LeafNode[Token`Comment, str_String /; StringMatchQ[str, "(* " ~~ "::CodeInspect::Disable::" ~~ LetterCharacter... ~~ "::" ~~ " *)"], _]



DisabledRegions[cstIn_] :=
Catch[
Module[{cst, codeInspectPushPatNodePoss, disabledRegions, siblingsPos, siblings, popFound, candidate, popPos, disableds},

  cst = cstIn;

  codeInspectPushPatNodePoss = Position[cst, codeInspectPushPat];

  disabledRegions = {};
  Do[
    siblingsPos = Most[pushPos];
    siblings = Extract[cst, {siblingsPos}][[1]];
    popFound = False;
    disableds = {};
    Do[
      candidate = siblings[[pos]];
      Switch[candidate,
        codeInspectPopPat,
          popPos = Most[pushPos] ~Join~ {pos};
          popFound = True;
          Break[]
        ,
        codeInspectDisablePat,
          disableds = disableds ~Join~ StringCases[candidate[[2]], "(* " ~~ "::CodeInspect::Disable::" ~~ d:LetterCharacter... ~~ "::" ~~ " *)" :> d]
      ]
      ,
      {pos, Last[pushPos]+1, Length[siblings]}
    ];
    If[popFound,
      AppendTo[disabledRegions, {rangeStart[Extract[cst, pushPos][[3, Key[Source]]]], rangeEnd[Extract[cst, popPos][[3, Key[Source]]]], disableds}]
      ,
      Message[DisabledRegions::missingpop];
      Throw[{}]
    ]
    ,
    {pushPos, codeInspectPushPatNodePoss}
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
