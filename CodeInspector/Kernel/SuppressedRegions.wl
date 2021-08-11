BeginPackage["CodeInspector`SuppressedRegions`"]

SuppressedRegions

suppressedRegion

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*

Example:

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::DuplicateClauses::If:: *)

If[a, b, b]

(* :!CodeAnalysis::EndBlock:: *)


*)


codeInspectBeginPat =
  (* in-the-clear parsed from text *)
  LeafNode[Token`Comment, "(* :!CodeAnalysis::BeginBlock:: *)", _] |
  (* in-the-clear parsed from boxes *)
  CellNode[Cell, {
    GroupNode[Comment, {
      LeafNode[Token`Boxes`OpenParenStar, _, _],
      LeafNode[String, " ", _],
      BoxNode[RowBox, {{
        LeafNode[String, ":", _],
        BoxNode[RowBox, {{
            LeafNode[String, "!", _],
            BoxNode[RowBox, {{
              LeafNode[String, "CodeAnalysis", _],
              LeafNode[String, "::", _],
              LeafNode[String, "BeginBlock", _],
              LeafNode[String, "::", _]}}, _]}}, _]}}, _],
      LeafNode[String, " ", _],
      LeafNode[Token`Boxes`StarCloseParen, _, _]}, _]}, _]

codeInspectEndPat =
  (* in-the-clear parsed from text *)
  LeafNode[Token`Comment, "(* :!CodeAnalysis::EndBlock:: *)", _] |
  (* in-the-clear parsed from boxes *)
  CellNode[Cell, {
    GroupNode[Comment, {
      LeafNode[Token`Boxes`OpenParenStar, _, _],
      LeafNode[String, " ", _],
      BoxNode[RowBox, {{
        LeafNode[String, ":", _],
        BoxNode[RowBox, {{
            LeafNode[String, "!", _],
            BoxNode[RowBox, {{
              LeafNode[String, "CodeAnalysis", _],
              LeafNode[String, "::", _],
              LeafNode[String, "EndBlock", _],
              LeafNode[String, "::", _]}}, _]}}, _]}}, _],
      LeafNode[String, " ", _],
      LeafNode[Token`Boxes`StarCloseParen, _, _]}, _]}, _]

codeInspectPackagePat =
  (* in-the-blind parsed from text *)
  LeafNode[Token`Comment, str_String /; StringMatchQ[str, "(* ::Package::\"Tags\"" ~~ ___ ~~ ":: *)"], _] |
  (* in-the-blind parsed from boxes *)
  CellNode[Cell, {
    GroupNode[Comment, {
      LeafNode[Token`Boxes`OpenParenStar, _, _],
      LeafNode[String, " ", _],
      BoxNode[RowBox, {{
        BoxNode[RowBox, {{
          LeafNode[String, "::", _],
          LeafNode[String, "Package", _],
          LeafNode[String, "::", _],
          LeafNode[String, "\"Tags\"", _]}}, _],
      LeafNode[String, "->", _], _}}, _],
      LeafNode[String, " ", _],
      LeafNode[Token`Boxes`StarCloseParen, _, _]}, _]}, _]

codeInspectCellPat =
  (* in-the-blind parsed from text *)
  LeafNode[Token`Comment, str_String /; StringMatchQ[str, "(* ::Code::Initialization::\"Tags\"" ~~ ___ ~~ ":: *)"], _] |
  (* in-the-blind parsed from boxes *)
  CellNode[Cell, {
    GroupNode[Comment, {
      LeafNode[Token`Boxes`OpenParenStar, _, _],
      LeafNode[String, " ", _],
      BoxNode[RowBox, {{
        BoxNode[RowBox, {{
          LeafNode[String, "::", _],
          LeafNode[String, "Code", _],
          LeafNode[String, "::", _],
          LeafNode[String, "Initialization", _],
          LeafNode[String, "::", _],
          LeafNode[String, "\"Tags\"", _]}}, _],
        LeafNode[String, "->", _], _}}, _],
      LeafNode[String, " ", _],
      LeafNode[Token`Boxes`StarCloseParen, _, _]}, _]}, _]

codeInspectSuppressPat =
  (* in-the-clear with no arg parsed from text *)
  LeafNode[
    Token`Comment,
    str_String /;
      StringMatchQ[str,
        ("(* :!CodeAnalysis::Disable::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ ":: *)")
      ],
    _
  ] |
  (* in-the-clear with 1 arg parsed from text *)
  LeafNode[
    Token`Comment,
    str_String /;
      StringMatchQ[str,
        ("(* :!CodeAnalysis::Disable::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ ":: *)")
      ],
    _
  ] |
  (* in-the-clear with no arg parsed from boxes *)
  CellNode[Cell, {
    GroupNode[Comment, {
      LeafNode[Token`Boxes`OpenParenStar, _, _],
      LeafNode[String, " ", _],
      BoxNode[RowBox, {{
        LeafNode[String, ":", _],
        BoxNode[RowBox, {{
          LeafNode[String, "!", _],
          BoxNode[RowBox, {{
            LeafNode[String, "CodeAnalysis", _],
            LeafNode[String, "::", _],
            LeafNode[String, "Disable", _],
            LeafNode[String, "::", _],
            LeafNode[String, _, _],
            LeafNode[String, "::", _]}}, _]}}, _]}}, _],
      LeafNode[String, " ", _],
      LeafNode[Token`Boxes`StarCloseParen, _, _]}, _]}, _] |
  (* in-the-clear with 1 arg parsed from boxes *)
  CellNode[Cell, {
      GroupNode[Comment, {
        LeafNode[Token`Boxes`OpenParenStar, _, _],
        LeafNode[String, " ", _],
        BoxNode[RowBox, {{
          LeafNode[String, ":", _],
          BoxNode[RowBox, {{
            LeafNode[String, "!", _],
              BoxNode[RowBox, {{
                LeafNode[String, "CodeAnalysis", _],
                LeafNode[String, "::", _],
                LeafNode[String, "Disable", _],
                LeafNode[String, "::", _],
                LeafNode[String, _, _],
                LeafNode[String, "::", _],
                LeafNode[String, _, _],
                LeafNode[String, "::", _]}}, _]}}, _]}}, _],
        LeafNode[String, " ", _],
        LeafNode[Token`Boxes`StarCloseParen, _, _]}, _]}, _]





SuppressedRegions[cstIn_] :=
Catch[
Module[{cst, codeInspectBeginPatNodePoss, suppressedRegions, siblingsPos, siblings, endFound, candidate, endPos, suppresseds, beginPos},

  cst = cstIn;

  suppressedRegions = {};

  If[empty[cst[[2]]],
    Throw[suppressedRegions]
  ];

  (*
  ::Package:: affecting the entire file
    
  (* ::Package::"Tags"-><|"DuplicateClauses" -> <|"If" -> <|Enabled -> False|>|>|>:: *)

  If[a,b,b]

  *)
  candidate = cst[[2, 1]];
  If[MatchQ[candidate, codeInspectPackagePat],
    beginPos = {2, 1};
    endPos = {2, Length[cst[[2]]]};
    suppresseds = suppressedsFromCandidate[candidate];
    AppendTo[suppressedRegions,
      suppressedRegion[rangeStart[Extract[cst, beginPos][[3]]], rangeEnd[Extract[cst, endPos][[3]]], suppresseds, <|"Toplevel" -> True|>]
    ]
  ];

  (*
  ::Code::Initialization:: affecting the next child
  *)
  Do[
    candidate = cst[[2, i]];
    If[MatchQ[candidate, codeInspectCellPat] && MatchQ[cst[[2, i + 1]], LeafNode[Token`Newline, _, _]],
      suppresseds = suppressedsFromCandidate[candidate];
      AppendTo[suppressedRegions,
        suppressedRegion[rangeStart[Extract[cst, {2, i + 2}][[3]]], rangeEnd[Extract[cst, {2, i + 2}][[3]]], suppresseds, <|"Toplevel" -> True|>]
      ]
    ]
    ,
    {i, 1, Length[cst[[2]]] - 2}
  ];

  codeInspectBeginPatNodePoss = Position[cst, codeInspectBeginPat];

  If[$Debug,
    Print["codeInspectBeginPatNodePoss: ", codeInspectBeginPatNodePoss]
  ];

  Do[
    siblingsPos = Most[beginPos];
    siblings = Extract[cst, {siblingsPos}][[1]];
    endFound = False;
    suppresseds = {};
    Do[
      candidate = siblings[[pos]];
      Switch[candidate,
        codeInspectEndPat,
          endPos = Most[beginPos] ~Join~ {pos};
          endFound = True;
          Break[]
        ,
        codeInspectSuppressPat,
          suppresseds = suppresseds ~Join~ suppressedsFromCandidate[candidate]
      ]
      ,
      {pos, Last[beginPos]+1, Length[siblings]}
    ];
    If[endFound,
      AppendTo[suppressedRegions,
        suppressedRegion[rangeStart[Extract[cst, beginPos][[3]]], rangeEnd[Extract[cst, endPos][[3]]], suppresseds, <|"Toplevel" -> MatchQ[beginPos, {_, _}]|>]
      ]
      ,
      Message[SuppressedRegions::missingpop];
      Throw[{}]
    ]
    ,
    {beginPos, codeInspectBeginPatNodePoss}
  ];

  suppressedRegions
]]


(*
in-the-clear with no arg parsed from text
*)
suppressedsFromCandidate[
  LeafNode[
    Token`Comment,
    str_String /;
      StringMatchQ[str,
        ("(* :!CodeAnalysis::Disable::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ ":: *)")
      ],
    _
  ]
] :=
  StringCases[str, {
      "(* :!CodeAnalysis::Disable::" ~~ d:(LetterCharacter ~~ LetterCharacter...) ~~ ":: *)" :> {d}
  }]

(*
in-the-clear with 1 arg parsed from text
*)
suppressedsFromCandidate[
  LeafNode[
    Token`Comment,
    str_String /;
      StringMatchQ[str,
        ("(* :!CodeAnalysis::Disable::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ ":: *)")
      ],
    _
  ]
] :=
  StringCases[str, {
      "(* :!CodeAnalysis::Disable::" ~~ d:(LetterCharacter ~~ LetterCharacter...) ~~ "::" ~~ a:(LetterCharacter ~~ LetterCharacter...) ~~ ":: *)" :> {d, a}
  }]

(*
in-the-clear with no arg parsed from boxes
*)
suppressedsFromCandidate[
  CellNode[Cell, {
    GroupNode[Comment, {
      LeafNode[Token`Boxes`OpenParenStar, _, _],
      LeafNode[String, " ", _],
      BoxNode[RowBox, {{
        LeafNode[String, ":", _],
        BoxNode[RowBox, {{
          LeafNode[String, "!", _],
          BoxNode[RowBox, {{
            LeafNode[String, "CodeAnalysis", _],
            LeafNode[String, "::", _],
            LeafNode[String, "Disable", _],
            LeafNode[String, "::", _],
            LeafNode[String, d_, _],
            LeafNode[String, "::", _]}}, _]}}, _]}}, _],
      LeafNode[String, " ", _],
      LeafNode[Token`Boxes`StarCloseParen, _, _]}, _]}, _]
] := {{d}}

(*
in-the-clear with 1 arg parsed from boxes
*)
suppressedsFromCandidate[
  CellNode[Cell, {
    GroupNode[Comment, {
      LeafNode[Token`Boxes`OpenParenStar, _, _],
      LeafNode[String, " ", _],
      BoxNode[RowBox, {{
        LeafNode[String, ":", _],
        BoxNode[RowBox, {{
          LeafNode[String, "!", _],
            BoxNode[RowBox, {{
              LeafNode[String, "CodeAnalysis", _],
              LeafNode[String, "::", _],
              LeafNode[String, "Disable", _],
              LeafNode[String, "::", _],
              LeafNode[String, d_, _],
              LeafNode[String, "::", _],
              LeafNode[String, a_, _],
              LeafNode[String, "::", _]}}, _]}}, _]}}, _],
      LeafNode[String, " ", _],
      LeafNode[Token`Boxes`StarCloseParen, _, _]}, _]}, _]
] := {{d, a}}

(* in-the-blind parsed from text *)
suppressedsFromCandidate[
  LeafNode[Token`Comment, str_String /; StringMatchQ[str, "(* ::Package::\"Tags\"->" ~~ ___ ~~ ":: *)"], _]
] :=
Module[{assocStr},
  assocStr = StringCases[str, "(* ::Package::\"Tags\"->" ~~ a___ ~~ ":: *)" :> a][[1]];
  suppressedsFromCandidateAssocStr[assocStr]
]

(* in-the-blind parsed from text *)
suppressedsFromCandidate[
  LeafNode[Token`Comment, str_String /; StringMatchQ[str, "(* ::Code::Initialization::\"Tags\"->" ~~ ___ ~~ ":: *)"], _]
] :=
Module[{assocStr},
  assocStr = StringCases[str, "(* ::Code::Initialization::\"Tags\"->" ~~ a___ ~~ ":: *)" :> a][[1]];
  suppressedsFromCandidateAssocStr[assocStr]
]


(* in-the-blind parsed from text *)
suppressedsFromCandidateAssocStr[assocStr_String] :=
Catch[
Reap[
Module[{assocNode, rules1, tag, body1, rules2, body2, rules3, argument},
  assocNode = CodeParse[assocStr];
  If[!MatchQ[assocNode, ContainerNode[String, {CallNode[LeafNode[Symbol, "Association", _], _, _]}, _]],
    (*
    invalid
    *)
    Throw[{}]
  ];
  rules1 = assocNode[[2, 1, 2]];
  Do[
    If[!MatchQ[rule1, CallNode[LeafNode[Symbol, "Rule", _], {LeafNode[String, _, _], _}, _]],
      (*
      invalid
      *)
      Throw[{}]
    ];
    tag = FromNode[rule1[[2, 1]]];
    body1 = rule1[[2, 2]];
    If[!MatchQ[body1, CallNode[LeafNode[Symbol, "Association", _], _, _]],
      (*
      invalid
      *)
      Throw[{}]
    ];
    rules2 = body1[[2]];
    Do[
      Switch[rule2,
        CallNode[LeafNode[Symbol, "Rule", _], {LeafNode[Symbol, "Enabled", _], LeafNode[Symbol, "False", _]}, _],
          Sow[{tag}]
        ,
        CallNode[LeafNode[Symbol, "Rule", _], {LeafNode[String, _, _], _}, _],
          argument = FromNode[rule2[[2, 1]]];
          body2 = rule2[[2, 2]];
          If[!MatchQ[body2, CallNode[LeafNode[Symbol, "Association", _], _, _]],
            (*
            invalid
            *)
            Throw[{}]
          ];
          rules3 = body2[[2]];
          Do[
            Switch[rule3,
              CallNode[LeafNode[Symbol, "Rule", _], {LeafNode[Symbol, "Enabled", _], LeafNode[Symbol, "False", _]}, _],
                Sow[{tag, argument}]
              ,
              _,
                (*
                invalid
                *)
                Throw[{}]
            ]
            ,
            {rule3, rules3}
          ]
        ,
        _,
          (*
          invalid
          *)
          Throw[{}]
      ]
      ,
      {rule2, rules2}
    ]
    ,
    {rule1, rules1}
  ]
] (* Module *)
][[2, 1]] (* Reap *)
] (* Catch *)




(*
Check for CellIndex before checking for Source
*)
rangeStart[KeyValuePattern[CellIndex -> index_]] :=
  <|CellIndex -> index|>

rangeEnd[KeyValuePattern[CellIndex -> index_]] :=
  <|CellIndex -> index|>


(*
For LineColumn convention, return the start or end span
*)
rangeStart[KeyValuePattern[Source -> {s:{_Integer, _Integer}, {_Integer, _Integer}}]] :=
  <|Source -> s|>

rangeEnd[KeyValuePattern[Source -> {{_Integer, _Integer}, e:{_Integer, _Integer}}]] :=
  <|Source -> e|>


(*
For SourceCharacterIndex convention, return the start or end span
*)
rangeStart[KeyValuePattern[Source -> {s:_Integer, _Integer}]] :=
  <|Source -> s|>

rangeEnd[KeyValuePattern[Source -> {_Integer, e:_Integer}]] :=
  <|Source -> e|>


(*
For other conventions, just return the src

We do not know what to do
*)
rangeStart[KeyValuePattern[Source -> src_]] :=
  <|Source -> src|>

rangeEnd[KeyValuePattern[Source -> src_]] :=
  <|Source -> src|>


End[]

EndPackage[]
