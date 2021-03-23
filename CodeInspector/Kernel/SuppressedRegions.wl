BeginPackage["CodeInspector`SuppressedRegions`"]

SuppressedRegions

suppressedRegion

Begin["`Private`"]

Needs["CodeParser`"]


(*

Please use this syntax:

(* CodeInspect::Push *)
(* CodeInspect::Suppress::DuplicateClauses::If *)

If[a, b, b]

(* CodeInspect::Pop *)


*)


codeInspectBeginPat =
  (* in-the-clear parsed from text *)
  LeafNode[Token`Comment, "(* CodeInspect::Push *)", _] |
  (* in-the-clear parsed from boxes *)
  CellNode[Cell, {
      GroupNode[Comment, {
        LeafNode[Token`Boxes`OpenParenStar, "(*", _],
        LeafNode[String, " ", _],
        BoxNode[RowBox, {{
            LeafNode[String, "CodeInspect", _],
            LeafNode[String, "::", _],
            LeafNode[String, "Push", _]
          }}, _],
        LeafNode[String, " ", _], LeafNode[Token`Boxes`StarCloseParen, "*)", _]
      }, _]
    }, _] |
  (* in-the-blind parsed from text *)
  (*
  yes, the <> is needed
  Related bugs: 407007
  *)
  LeafNode[Token`Comment, "(* " <> "::CodeInspect::Push:: *)", _] |
  (* in-the-blind parsed from boxes *)
  CellNode[Cell, {
      GroupNode[Comment, {
        LeafNode[Token`Boxes`OpenParenStar, "(*", _],
        LeafNode[String, " ", _],
        BoxNode[RowBox, {{
          LeafNode[String, "::", _],
          LeafNode[String, "CodeInspect", _],
          LeafNode[String, "::", _],
          LeafNode[String, "Push", _],
          LeafNode[String, "::", _]}}, _],
        LeafNode[String, " ", _],
        LeafNode[Token`Boxes`StarCloseParen, "*)", _]}, _]}, _]

codeInspectEndPat =
  (* in-the-clear parsed from text *)
  LeafNode[Token`Comment, "(* CodeInspect::Pop *)", _] |
  (* in-the-clear parsed from boxes *)
  CellNode[Cell, {
      GroupNode[Comment, {
          LeafNode[Token`Boxes`OpenParenStar, "(*", _],
          LeafNode[String, " ", _],
          BoxNode[RowBox, {{
              LeafNode[String, "CodeInspect", _],
              LeafNode[String, "::", _],
              LeafNode[String, "Pop", _]
            }}, _],
          LeafNode[String, " ", _], LeafNode[Token`Boxes`StarCloseParen, "*)", _]
        }, _]
    }, _] |
  (* in-the-blind parsed from text *)
  (*
  yes, the <> is needed
  Related bugs: 407007
  *)
  LeafNode[Token`Comment, "(* " <> "::CodeInspect::Pop:: *)", _] |
  (* in-the-blind parsed from boxes *)
  CellNode[Cell, {
      GroupNode[Comment, {
        LeafNode[Token`Boxes`OpenParenStar, "(*", _],
        LeafNode[String, " ", _],
        BoxNode[RowBox, {{
          LeafNode[String, "::", _],
          LeafNode[String, "CodeInspect", _],
          LeafNode[String, "::", _],
          LeafNode[String, "Pop", _],
          LeafNode[String, "::", _]}}, _],
        LeafNode[String, " ", _],
        LeafNode[Token`Boxes`StarCloseParen, "*)", _]}, _]}, _]

codeInspectSuppressPat =
  (* in-the-clear with no arg parsed from text *)
  LeafNode[
    Token`Comment,
    str_String /;
      StringMatchQ[str,
        ("(* CodeInspect::" ~~ ("Suppress" | "Disable") ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ " *)")
      ],
    _
  ] |
  (* in-the-clear with 1 arg parsed from text *)
  LeafNode[
    Token`Comment,
    str_String /;
      StringMatchQ[str,
        ("(* CodeInspect::" ~~ ("Suppress" | "Disable") ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ " *)")
      ],
    _
  ] |
  (* in-the-clear with no arg parsed from boxes *)
  CellNode[Cell, {
      GroupNode[Comment, {
        LeafNode[Token`Boxes`OpenParenStar, "(*", _],
        LeafNode[String, " ", _],
        BoxNode[RowBox, {{
          LeafNode[String, "CodeInspect", _],
          LeafNode[String, "::", _],
          LeafNode[String, "Suppress" | "Disable", _],
          LeafNode[String, "::", _],
          LeafNode[String, _, _]}}, _],
          LeafNode[String, " ", _],
          LeafNode[Token`Boxes`StarCloseParen, "*)", _]}, _]
    }, _] |
  (* in-the-clear with 1 arg parsed from boxes *)
  CellNode[Cell, {
      GroupNode[Comment, {
        LeafNode[Token`Boxes`OpenParenStar, "(*", _],
        LeafNode[String, " ", _],
        BoxNode[RowBox, {{
          LeafNode[String, "CodeInspect", _],
          LeafNode[String, "::", _],
          LeafNode[String, "Suppress" | "Disable", _],
          LeafNode[String, "::", _],
          LeafNode[String, _, _],
          LeafNode[String, "::", _],
          LeafNode[String, _, _]}}, _],
        LeafNode[String, " ", _],
        LeafNode[Token`Boxes`StarCloseParen, "*)", _]}, _]
    }, _] |
  (* in-the-blind with no arg parsed from text *)
  LeafNode[
    Token`Comment,
    str_String /;
      StringMatchQ[str,
        (*
        yes, the <> is needed
        Related bugs: 407007
        *)
        ("(* " <> "::CodeInspect::" ~~ ("Suppress" | "Disable") ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ ":: *)")
      ],
    _
  ] |
  (* in-the-blind with 1 arg parsed from text *)
  LeafNode[
    Token`Comment,
    str_String /;
      StringMatchQ[str,
        (*
        yes, the <> is needed
        Related bugs: 407007
        *)
        ("(* " <> "::CodeInspect::" ~~ ("Suppress" | "Disable") ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ ":: *)")
      ],
    _
  ] |
  (* in-the-blind with no arg parsed from boxes *)
  CellNode[Cell, {
      GroupNode[Comment, {
        LeafNode[Token`Boxes`OpenParenStar, "(*", _],
        LeafNode[String, " ", _],
        BoxNode[RowBox, {{
          LeafNode[String, "::", _],
          LeafNode[String, "CodeInspect", _],
          LeafNode[String, "::", _],
          LeafNode[String, "Suppress" | "Disable", _],
          LeafNode[String, _, _],
          LeafNode[String, "::", _]}}, _],
        LeafNode[String, " ", _],
        LeafNode[Token`Boxes`StarCloseParen, "*)", _]}, _]}, _] |
  (* in-the-blind with 1 arg parsed from boxes *)
  CellNode[Cell, {
      GroupNode[Comment, {
        LeafNode[Token`Boxes`OpenParenStar, "(*", _],
        LeafNode[String, " ", _],
        BoxNode[RowBox, {{
          LeafNode[String, "::", _],
          LeafNode[String, "CodeInspect", _],
          LeafNode[String, "::", _],
          LeafNode[String, "Suppress" | "Disable", _],
          LeafNode[String, "::", _],
          LeafNode[String, _, _],
          LeafNode[String, "::", _],
          LeafNode[String, _, _],
          LeafNode[String, "::", _]}}, _],
        LeafNode[String, " ", _],
        LeafNode[Token`Boxes`StarCloseParen, "*)", _]}, _]}, _]



SuppressedRegions[cstIn_] :=
Catch[
Module[{cst, codeInspectBeginPatNodePoss, suppressedRegions, siblingsPos, siblings, endFound, candidate, endPos, suppresseds},

  cst = cstIn;

  codeInspectBeginPatNodePoss = Position[cst, codeInspectBeginPat];

  If[$Debug,
    Print["codeInspectBeginPatNodePoss: ", codeInspectBeginPatNodePoss]
  ];

  suppressedRegions = {};
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
      Message[SuppressedRegions::missingpop, cst[[3]]];
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
        ("(* CodeInspect::" ~~ ("Suppress" | "Disable") ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ " *)")
      ],
    _
  ]
] :=
  StringCases[str, {
      "(* CodeInspect::" ~~ ("Suppress" | "Disable") ~~ "::" ~~ d:(LetterCharacter ~~ LetterCharacter...) ~~ " *)" :> {d}
  }]

(*
in-the-clear with 1 arg parsed from text
*)
suppressedsFromCandidate[
  LeafNode[
    Token`Comment,
    str_String /;
      StringMatchQ[str,
        ("(* CodeInspect::" ~~ ("Suppress" | "Disable") ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ " *)")
      ],
    _
  ]
] :=
  StringCases[str, {
      "(* CodeInspect::" ~~ ("Suppress" | "Disable") ~~ "::" ~~ d:(LetterCharacter ~~ LetterCharacter...) ~~ "::" ~~ a:(LetterCharacter ~~ LetterCharacter...) ~~ " *)" :> {d, a}
  }]

(*
in-the-clear with no arg parsed from boxes
*)
suppressedsFromCandidate[
  CellNode[Cell, {
    GroupNode[Comment, {
      LeafNode[Token`Boxes`OpenParenStar, "(*", _],
      LeafNode[String, " ", _],
      BoxNode[RowBox, {{
        LeafNode[String, "CodeInspect", _],
        LeafNode[String, "::", _],
        LeafNode[String, "Suppress" | "Disable", _],
        LeafNode[String, "::", _],
        LeafNode[String, d_, _]}}, _],
        LeafNode[String, " ", _],
        LeafNode[Token`Boxes`StarCloseParen, "*)", _]
    }
    ,
    _]
  }, _]
] := {d}

(*
in-the-clear with 1 arg parsed from boxes
*)
suppressedsFromCandidate[
  CellNode[Cell, {
    GroupNode[Comment, {
        LeafNode[Token`Boxes`OpenParenStar, "(*", _],
        LeafNode[String, " ", _],
        BoxNode[RowBox, {{
          LeafNode[String, "CodeInspect", _],
          LeafNode[String, "::", _],
          LeafNode[String, "Suppress" | "Disable", _],
          LeafNode[String, "::", _],
          LeafNode[String, d_, _],
          LeafNode[String, "::", _],
          LeafNode[String, a_, _]}}, _],
        LeafNode[String, " ", _],
        LeafNode[Token`Boxes`StarCloseParen, "*)", _]
      }
      ,
      _]
  }, _]
] := {d, a}



(*
in-the-blind with no arg parsed from text
*)
suppressedsFromCandidate[
  LeafNode[
    Token`Comment,
    str_String /;
      StringMatchQ[str,
        (*
        yes, the <> is needed
        Related bugs: 407007
        *)
        ("(* " <> "::CodeInspect::" ~~ ("Suppress" | "Disable") ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ ":: *)")
      ],
    _
  ]
] :=
  StringCases[str, {
      (*
      yes, the <> is needed
      Related bugs: 407007
      *)
      "(* " <> "::CodeInspect::" ~~ ("Suppress" | "Disable") ~~ "::" ~~ d:(LetterCharacter ~~ LetterCharacter...) ~~ ":: *)" :> {d}
  }]

(*
in-the-blind with 1 arg parsed from text
*)
suppressedsFromCandidate[
  LeafNode[
    Token`Comment,
    str_String /;
      StringMatchQ[str,
        (*
        yes, the <> is needed
        Related bugs: 407007
        *)
        ("(* " <> "::CodeInspect::" ~~ ("Suppress" | "Disable") ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ "::" ~~ (LetterCharacter ~~ LetterCharacter...) ~~ ":: *)")
      ],
    _
  ]
] :=
  StringCases[str, {
      (*
      yes, the <> is needed
      Related bugs: 407007
      *)
      "(* " <> "::CodeInspect::" ~~ ("Suppress" | "Disable") ~~ "::" ~~ d:(LetterCharacter ~~ LetterCharacter...) ~~ "::" ~~ a:(LetterCharacter ~~ LetterCharacter...) ~~ ":: *)" :> {d, a}
  }]

(*
in-the-blind with no arg parsed from boxes
*)
suppressedsFromCandidate[
  CellNode[Cell, {
    GroupNode[Comment, {
      LeafNode[Token`Boxes`OpenParenStar, "(*", _],
      LeafNode[String, " ", _],
      BoxNode[RowBox, {{
        LeafNode[String, "::", _],
        LeafNode[String, "CodeInspect", _],
        LeafNode[String, "::", _],
        LeafNode[String, "Suppress" | "Disable", _],
        LeafNode[String, d_, _],
        LeafNode[String, "::", _]}}, _],
      LeafNode[String, " ", _],
      LeafNode[Token`Boxes`StarCloseParen, "*)", _]}, _]}, _]
] := {d}

(*
in-the-blind with 1 arg parsed from boxes
*)
suppressedsFromCandidate[
  CellNode[Cell, {
    GroupNode[Comment, {
      LeafNode[Token`Boxes`OpenParenStar, "(*", _],
      LeafNode[String, " ", _],
      BoxNode[RowBox, {{
        LeafNode[String, "::", _],
        LeafNode[String, "CodeInspect", _],
        LeafNode[String, "::", _],
        LeafNode[String, "Suppress" | "Disable", _],
        LeafNode[String, "::", _],
        LeafNode[String, d_, _],
        LeafNode[String, "::", _],
        LeafNode[String, a_, _],
        LeafNode[String, "::", _]}}, _],
      LeafNode[String, " ", _],
      LeafNode[Token`Boxes`StarCloseParen, "*)", _]}, _]}, _]
] := {d, a}




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
