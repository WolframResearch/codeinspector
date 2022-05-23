BeginPackage["CodeInspector`ImplicitTokens`"]

CodeInspectImplicitTokens

CodeInspectImplicitTokensSummarize


CodeInspectImplicitTokensCST

CodeInspectImplicitTokensCSTSummarize


CodeInspectImplicitTokensAgg


$ImplicitTokensLimit


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Abstract`"]
Needs["CodeParser`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Summarize`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`Utils`"]



(*
How many implicit tokens to keep?
*)
$ImplicitTokensLimit = 100

(*
How many lines to include above and below each lint
*)
$EnvironBuffer = 0



CodeInspectImplicitTokens::usage = "CodeInspectImplicitTokens[code] returns a list of implicit tokens in code."

Options[CodeInspectImplicitTokens] = {
  PerformanceGoal -> "Speed",
  "TabWidth" -> 1,
  "AllowedImplicitTokens" -> {"*", ",", ";;", "?"},
  SourceConvention -> "LineColumn"
}


$fileByteCountMinLimit = 0*^6
$fileByteCountMaxLimit = 3*^6



CodeInspectImplicitTokens[File[file_String], opts:OptionsPattern[]] :=
Catch[
Module[{full, performanceGoal, cst},

  performanceGoal = OptionValue[PerformanceGoal];

  full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <| "FileName" -> file |>]]
  ];

  If[performanceGoal == "Speed",
    If[FileByteCount[full] > $fileByteCountMaxLimit,
      Throw[Failure["FileTooLarge", <| "FileName" -> full, "FileSize" -> FileSize[full] |>]]
    ];
    If[FileByteCount[full] < $fileByteCountMinLimit,
      Throw[Failure["FileTooSmall", <| "FileName" -> full, "FileSize" -> FileSize[full] |>]]
    ];
  ];

  cst = CodeConcreteParse[File[full], FilterRules[{opts}, Options[CodeConcreteParse]]];

  CodeInspectImplicitTokensCST[cst, FilterRules[{opts}, Options[CodeInspectImplicitTokensCST]]]
]]

CodeInspectImplicitTokens[string_String, opts:OptionsPattern[]] :=
Catch[
Module[{cst},

  cst = CodeConcreteParse[string, FilterRules[{opts}, Options[CodeConcreteParse]]];

  CodeInspectImplicitTokensCST[cst, FilterRules[{opts}, Options[CodeInspectImplicitTokensCST]]]
]]



Options[CodeInspectImplicitTokensCST] = {
  "AllowedImplicitTokens" -> {"*", ",", ";;", "?"}
}

CodeInspectImplicitTokensCST[cst_, opts:OptionsPattern[]] :=
Catch[
Module[{agg},

  If[FailureQ[cst],
    Throw[cst]
  ];

  agg = Aggregate[cst];

  CodeInspectImplicitTokensAgg[agg, FilterRules[{opts}, Options[CodeInspectImplicitTokensAgg]]]
]]


Options[CodeInspectImplicitTokensAgg] = {
  "AllowedImplicitTokens" -> {"*", ",", ";;", "?"}
}

CodeInspectImplicitTokensAgg[agg_, OptionsPattern[]] :=
Catch[
Module[{times, spans, commaNulls, compoundExpressionNulls, ops, allowed},

  allowed = OptionValue["AllowedImplicitTokens"];

  If[allowed === All,
    allowed = {"*", ",", ";;", ";", "?"}
  ];

  If[FailureQ[agg],
    Throw[agg]
  ];

  times = {};
  spans = {};
  commaNulls = {};
  compoundExpressionNulls = {};
  ops = {};

  If[MemberQ[allowed, "*"],
    times = implicitTimes[agg];
  ];

  If[MemberQ[allowed, ";;"],
    spans = implicitSpans[agg];
  ];

  If[MemberQ[allowed, ","],
    commaNulls = implicitCommaNulls[agg];
  ];

  If[MemberQ[allowed, ";"],
    compoundExpressionNulls = implicitCompoundExpressionNulls[agg];
  ];

  If[MemberQ[allowed, "?"],
    ops = expectedOperands[agg];
  ];

  Join[times, spans, commaNulls, compoundExpressionNulls, ops]
]]




CodeInspectImplicitTokensSummarize::usage = "CodeInspectImplicitTokensSummarize[code] returns an inspection summary object."

Options[CodeInspectImplicitTokensSummarize] = {
  "TabWidth" -> 1
}

CodeInspectImplicitTokensSummarize[File[file_String], implicitTokensIn:_List:Automatic, opts:OptionsPattern[]] :=
Catch[
Module[{implicitTokens, full, lines, lintedLines, bytes, str, tabWidth},

  implicitTokens = implicitTokensIn;

  tabWidth = OptionValue["TabWidth"];

  full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <| "FileName" -> file |>]]
  ];

  If[FileByteCount[full] == 0,
    Throw[Failure["EmptyFile", <| "FileName" -> full |>]]
  ];

  If[implicitTokens === Automatic,
    implicitTokens = CodeInspectImplicitTokens[File[full], FilterRules[{opts}, Options[CodeInspectImplicitTokens]]];
  ];

  (*
  Was:
  bytes = Import[full, "Byte"];

  but this is slow
  *)
  bytes = ReadByteArray[full];

  str = SafeString[bytes];

  If[FailureQ[str],
    Throw[str]
  ];

  If[MissingQ[str],
    Throw[str]
  ];

  lines = StringSplit[str, {"\r\n", "\n", "\r"}, All];

  lines = replaceTabs[#, 1, "!", tabWidth]& /@ lines;

  lintedLines = implicitTokensLinesReport[lines, implicitTokens];

  InspectedFileObject[full, lintedLines]
]]





CodeInspectImplicitTokensSummarize[string_String, implicitTokensIn:_List:Automatic, opts:OptionsPattern[]] :=
Catch[
Module[{implicitTokens, lines, lintedLines, tabWidth},

  implicitTokens = implicitTokensIn;

  tabWidth = OptionValue["TabWidth"];

  If[StringLength[string] == 0,
    Throw[Failure["EmptyString", <||>]]
  ];

  If[implicitTokens === Automatic,
    implicitTokens = CodeInspectImplicitTokens[string, FilterRules[{opts}, Options[CodeInspectImplicitTokens]]];
  ];

  lines = StringSplit[string, {"\r\n", "\n", "\r"}, All];

  lines = replaceTabs[#, 1, "!", tabWidth]& /@ lines;

  lintedLines = implicitTokensLinesReport[lines, implicitTokens];

  InspectedStringObject[string, lintedLines]
]]




Options[CodeInspectImplicitTokensCSTSummarize] = {
  "TabWidth" -> 1
}

(*
precondition:
Source convention for implicitTokens is "LineColumn"

*)
CodeInspectImplicitTokensCSTSummarize[cst_, implicitTokensIn:_List:Automatic, opts:OptionsPattern[]] :=
Catch[
Module[{implicitTokens, lines, lintedLines, string, tabWidth},

  If[FailureQ[cst],
    Throw[cst]
  ];

  implicitTokens = implicitTokensIn;

  tabWidth = OptionValue["TabWidth"];

  If[implicitTokens === Automatic,
    implicitTokens = CodeInspectImplicitTokensCST[cst, FilterRules[{opts}, Options[CodeInspectImplicitTokensCST]]];
  ];

  string = ToSourceCharacterString[cst];

  lines = StringSplit[string, {"\r\n", "\n", "\r"}, All];

  lines = replaceTabs[#, 1, "!", tabWidth]& /@ lines;

  lintedLines = implicitTokensLinesReport[lines, implicitTokens];

  InspectedStringObject[string, lintedLines]
]]




implicitTimes[agg_] :=
Catch[
Module[{times},

  times = Cases[agg, InfixNode[Times, nodes_ /; !FreeQ[nodes, LeafNode[Token`Fake`ImplicitTimes, _, _], 1], _], {0, Infinity}];

  times
]]

implicitSpans[agg_] :=
Catch[
Module[{spans},

  spans = Cases[agg, (BinaryNode|TernaryNode)[Span, nodes_ /; !FreeQ[nodes, LeafNode[Token`Fake`ImplicitOne | Token`Fake`ImplicitAll, _, _], 1], _], {0, Infinity}];

  spans
]]

implicitCommaNulls[agg_] :=
Catch[
Module[{nulls},

  nulls = Cases[agg, InfixNode[Comma, nodes_ /; !FreeQ[nodes, ErrorNode[Token`Error`PrefixImplicitNull | Token`Error`InfixImplicitNull, _, _], 1], _], {0, Infinity}];

  nulls
]]

implicitCompoundExpressionNulls[agg_] :=
Catch[
Module[{nulls},

  nulls = Cases[agg, InfixNode[CompoundExpression, nodes_ /; !FreeQ[nodes, LeafNode[Token`Fake`ImplicitNull, _, _], 1], _], {0, Infinity}];

  nulls
]]

expectedOperands[aggIn_] :=
Catch[
Module[{ops, poss, agg},

  agg = aggIn;

  poss = Position[agg, ErrorNode[Token`Error`ExpectedOperand, _, _]];

  ops = Extract[agg, (#[[;; -3]])]& /@ poss;

  ops
]]




(* how many (, ), or \[Times] to insert per line *)
$markupLimit = 100


$color = severityColor[{InspectionObject["ImplicitTimes", "ImplicitTimes", "ImplicitTimes", <||>]}];


modify[lineIn_String, charInfos:{{_, _, _}...}, lineNumber_] :=
Module[{line, cols, inserters, under, rules},

  cols = Cases[charInfos, {char_, lineNumber, col_} :> {char, col}];

  line = lineIn;

  inserters = (#[[2]] -> #[[1]])& /@ cols;

  If[$Debug,
    Print["lineNumber: ", lineNumber];
    Print["inserters: ", inserters];
  ];

  rules = Merge[inserters, (LintMarkup[#, FontWeight -> CodeInspector`Format`$LintGridFontWeight, FontColor -> $color])& @* mergeCharacters];

  rules = Normal[rules];
  
  under = Table[" ", {StringLength[line]}];

  (*
  extend line to be able to insert \[Times] after the line, when ImplicitTimes spans lines
  *)
  under = Join[under, {" "}];

  under = ReplacePart[under, rules];

  (*
  to match Listify
  *)
  under = Join[{" "}, under];

  under
]

mergeCharacters[{"("}] = "("
mergeCharacters[{")"}] = ")"
mergeCharacters[{LintAllCharacter}] = LintAllCharacter
mergeCharacters[{LintNullCharacter}] = LintNullCharacter
mergeCharacters[{LintOneCharacter}] = LintOneCharacter
mergeCharacters[{LintTimesCharacter}] = LintTimesCharacter
mergeCharacters[{LintSpaceTimesCharacter}] = LintSpaceTimesCharacter
mergeCharacters[{LintExpectedOperandCharacter}] = LintExpectedOperandCharacter

mergeCharacters[{"(", "("}] = LintOpenOpenCharacter
mergeCharacters[{"(", LintOneCharacter}] = LintOpenOneCharacter
mergeCharacters[{")", ")"}] = LintCloseCloseCharacter
mergeCharacters[{")", LintAllCharacter}] = LintAllCloseCharacter
mergeCharacters[{")", LintTimesCharacter | LintSpaceTimesCharacter}] = LintCloseTimesCharacter
mergeCharacters[{")", LintExpectedOperandCharacter}] = LintExpectedOperandCloseCharacter
mergeCharacters[{LintAllCharacter, LintTimesCharacter | LintSpaceTimesCharacter}] = LintAllTimesCharacter
mergeCharacters[{LintAllCharacter, LintOneCharacter}] = LintAllOneCharacter
mergeCharacters[{LintOneCharacter, LintTimesCharacter | LintSpaceTimesCharacter}] = LintTimesOneCharacter
mergeCharacters[{LintExpectedOperandCharacter, LintTimesCharacter | LintSpaceTimesCharacter}] = LintExpectedOperandTimesCharacter
mergeCharacters[{"(", LintExpectedOperandCharacter}] = LintOpenExpectedOperandCharacter

mergeCharacters[{")", LintOneCharacter, LintTimesCharacter | LintSpaceTimesCharacter}] = LintCloseTimesOneCharacter
mergeCharacters[{LintAllCharacter, LintOneCharacter, LintTimesCharacter | LintSpaceTimesCharacter}] = LintAllTimesOneCharacter


(*
Anything else that is unhandled is ignored

An arbitrary number of open parens can be required to merge

e.g., with a&a&a&a&a&a&a&a&a&a&a&a&a&

This is valid syntax that requires 12 open parens in the same location

Obviously, we cannot have a dedicated symbol for all combinations
*)
mergeCharacters[unhandled_] := (
  If[$Debug,
    Message[mergeCharacters::unhandled, unhandled]
  ];
  LintUnhandledCharacter
)

(*
return {line, col} for all \[Times] symbols
*)
processPar[{left_, LeafNode[Token`Fake`ImplicitTimes, _, _], right_}] :=
Module[{leftSource, rightSource},

  leftSource = left[[3, Key[Source]]];
  rightSource = right[[3, Key[Source]]];
  (*
  same line

  this is symbolically represented as the best placement between left and right at this stage

  Actual tokenization needs to occur later to figure out the actual column
  *)
  BestImplicitTimesPlacement[{leftSource[[2]], rightSource[[1]]}]
]

(*
other tokens in InfixNode[Time, ] such as LeafNode[Token`Star]
*)
processPar[_] := Nothing


(*
nodes is something like { 1, ImplicitTimes, 2 } and we just want {1, 2} pairs

we will disregard the ImplicitTimes tokens that come in, because they may not have Source that looks good
*)
processChildren[nodes_List] :=
Module[{pars},

  If[$Debug,
    Print["processChildren nodes: ", nodes];
  ];

  pars = Partition[nodes, 3, 2];
  processPar /@ pars
]

(*
precondition:
Source convention for implicitTokens is "LineColumn"

*)
implicitTokensLinesReport[linesIn:{___String}, implicitTokensIn:_List] :=
Catch[
Module[{implicitTokens, sources, starts, ends, infixs, lines,
  linesToModify, times, ones, alls, nulls, charInfos, charInfoPoss,
  ops, maxLineNumberLength, environLinesTentative, environLines},

  If[implicitTokensIn === {},
    Throw[{}]
  ];

  implicitTokens = implicitTokensIn;

  lines = linesIn;

  lines = StringTake[#, UpTo[$LineTruncationLimit]]& /@ lines;

  times = Cases[implicitTokens, InfixNode[Times, nodes_ /; !FreeQ[nodes, LeafNode[Token`Fake`ImplicitTimes, _, _], 1], _]];
  sources = #[Source]& /@ times[[All, 3]];

  starts = {"(", #[[1]], #[[2]]}& /@ sources[[All, 1]];
  ends = {")", #[[1]], #[[2]]}& /@ sources[[All, 2]];


  ones = Union[Cases[implicitTokens, LeafNode[Token`Fake`ImplicitOne, _, _], {0, Infinity}]];
  alls = Union[Cases[implicitTokens, LeafNode[Token`Fake`ImplicitAll, _, _], {0, Infinity}]];
  nulls = Union[Cases[implicitTokens, LeafNode[Token`Fake`ImplicitNull, _, _] | ErrorNode[Token`Error`PrefixImplicitNull | Token`Error`InfixImplicitNull, _, _], {0, Infinity}]];
  ops = Union[Cases[implicitTokens, ErrorNode[Token`Error`ExpectedOperand, _, _], {0, Infinity}]];

  times = processChildren /@ times[[All, 2]];

  times = Flatten[times, 1];

  (*
  resolve BestImplicitTimesPlacement with actual columns now
  *)
  times = Map[resolveInfix[#, lines]&, times];

  If[$Debug,
    Print["times after resolveInfix: ", times];
  ];

  (*
  Fix 418799: push implicit 1 and All past any leading or trailing whitespace for better appearance
  *)
  ones = Map[pushOutBefore[#, lines]&, ones];
  alls = Map[pushOutAfter[#, lines]&, alls];

  (*
  Source convention for implicitTokens is "LineColumn"
  *)
  nulls = {LintNullCharacter, #[[3, Key[Source], 1, 1]], #[[3, Key[Source], 1, 2]]}& /@ nulls;
  ops = {LintExpectedOperandCharacter, #[[3, Key[Source], 1, 1]], #[[3, Key[Source], 1, 2]]}& /@ ops;

  (*
  each element of infixs is:
  {LineCharacter, line, col}
  *)
  infixs = times ~Join~ ones ~Join~ alls ~Join~ nulls ~Join~ ops;

  charInfos = starts ~Join~ ends ~Join~ infixs;

  If[$Debug,
    Print["charInfos: ", charInfos]
  ];

  (*
  Make sure to sort using Union before taking
  *)
  charInfoPoss = Union[charInfos[[All, 2;;3]]];

  charInfoPoss = Take[charInfoPoss, UpTo[$ImplicitTokensLimit]];

  (*
  Source convention for implicitTokens is "LineColumn", so just sort by the structure itself
  *)
  charInfos = SortBy[charInfos, #[[2;;3]]&];

  charInfos = Cases[charInfos, {_, line_, col_} /; MemberQ[charInfoPoss, {line, col}]];

  linesToModify = Union[charInfos[[All, 2]]];

  environLinesTentative = Clip[(# + {-$EnvironBuffer, $EnvironBuffer}), {1, Length[lines]}]& /@ linesToModify;
  environLinesTentative = Range @@@ environLinesTentative;

  environLines = MapThread[Complement, {environLinesTentative, List /@ linesToModify}];

  linesToModify = MapThread[Union, {List /@ linesToModify, environLines}];

  linesToModify = Union[Flatten[linesToModify]];
  environLines = Union[Flatten[environLines]];

  maxLineNumberLength = Max[IntegerLength /@ linesToModify];

  Table[
    With[{environ = MemberQ[environLines, i]},

      InspectedLineObject[lines[[i]], i, {ListifyLine[lines[[i]], <||>, "EndOfFile" -> (i == Length[lines])],
        modify[lines[[i]], charInfos, i]},
        {}, "MaxLineNumberLength" -> maxLineNumberLength, "Environ" -> environ]
    ]
    ,
    {i, linesToModify}
  ]
]]



(*
precondition:
Source convention for implicitTokens is "LineColumn"

BestImplicitTimesPlacement[span_] is something like {{startLine_, startCol_}, {endLine_, endCol_}}

returns {LintTimesCharacter, line, column}
*)
resolveInfix[BestImplicitTimesPlacement[span_], lines:{___String}] :=
Module[{lineNumber, line, tokens, goalLine, goalCol, spaces, spaceRanges, candidates, edges, offset, intersection,
  mean, commentsOrMBWhitespace, gaps, excludes, goals},

  If[$Debug,
    Print["resolveInfix: ", {BestImplicitTimesPlacement[span], lines}];
  ];

  Which[
    span[[1, 1]] != span[[2, 1]],
      (* different lines, so place \[Times] at end of first line *)
      {LintTimesCharacter, span[[1, 1]], StringLength[lines[[span[[1, 1]]]]] + 1}
    ,
    span[[1, 2]] == span[[2, 2]],
      (* contiguous *)
      {LintTimesCharacter, span[[1, 1]], span[[1, 2]]}
    ,
    span[[1, 2]] + 1 == span[[2, 2]],
      (*
      optimization case to avoid calling TokenizeString: only 1 space between
      Use LintSpaceTimesCharacter here to display a space before the \[Times] for better appearance
      *)
      {LintSpaceTimesCharacter, span[[1, 1]], span[[1, 2]]}
    ,
    True,
      (* do actual work to figure out best placement *)
      goalLine = span[[1, 1]];
      mean = N[Mean[{span[[1, 2]], span[[2, 2]]}]];
      lineNumber = span[[1, 1]];
      line = lines[[lineNumber]];

      (* only tokenize the characters in-between *)
      line = StringTake[line, {span[[1, 2]], span[[2, 2]]-1}];

      If[$Debug,
        Print["line: ", line];
      ];

      tokens = CodeTokenize[line];

      If[$Debug,
        Print["tokens: ", tokens];
      ];

      offset = span[[1, 2]];
      
      If[$Debug,
        Print["offset: ", offset];
      ];

      (*
      any space is a candidate

      ADDENDUM: only testing actual Whitespace with StringLength of 1

      Something like:
      a\[ThinSpace]b

      should NOT get a \[Times] character in the middle of ThinSpace!
      *)
      spaces = Cases[tokens, LeafNode[Whitespace, s_ /; StringLength[s] == 1, _]];
      spaceRanges = offset - 1 + Flatten[Range @@ #[[3, Key[Source], All, 2]]& /@ spaces];
      
      If[$Debug,
        Print["spaceRanges: ", spaceRanges];
      ];

      (*
      the gaps on either side of a comment are only candidates if they are not next to a space (because the space
      itself is preferred) 
      *)
      commentsOrMBWhitespace = Cases[tokens, LeafNode[Token`Comment, _, _] | LeafNode[Whitespace, s_ /; StringLength[s] > 1, _]];
      gaps = Union[Flatten[#[[3, Key[Source], All, 2]]& /@ commentsOrMBWhitespace]];

      excludes = SequenceCases[tokens, {LeafNode[Whitespace, s_ /; StringLength[s] == 1, _], c:LeafNode[Token`Comment, _, _] | LeafNode[Whitespace, s_ /; StringLength[s] > 1, _]} :> c];
      gaps = Complement[gaps, #[[3, Key[Source], 1, 2]]& /@ excludes];

      excludes = SequenceCases[tokens, {c:LeafNode[Token`Comment, _, _] | LeafNode[Whitespace, s_ /; StringLength[s] > 1, _], LeafNode[Whitespace, s_ /; StringLength[s] == 1, _]} :> c];
      gaps = Complement[gaps, #[[3, Key[Source], 2, 2]]& /@ excludes];

      gaps = offset - 1 + gaps;

      If[$Debug,
        Print["gaps: ", gaps];
      ];

      edges = offset - 1 + {(*0, *)StringLength[line]+1};

      If[$Debug,
        Print["edges: ", edges];
      ];

      candidates = Union[spaceRanges ~Join~ gaps ~Join~ edges];

      If[$Debug,
        Print["candidates: ", candidates];
      ];

      (*
      Which candidates are closest to mean?
      *)
      goals = MinimalBy[candidates, Abs[# - mean]&];

      If[$Debug,
        Print["goals: ", goals];
      ];

      Which[
        Length[goals] == 1,
          (* only 1 possibility *)
          goalCol = goals[[1]];
        ,
        intersection = Intersection[goals, spaceRanges];
        intersection =!= {},
          (* always prefer spaces over gaps or edges *)
          goalCol = intersection[[1]];
        ,
        True,
          (* only comments; arbitrarily choose one *)
          goalCol = goals[[1]];
      ];

      {LintTimesCharacter, goalLine, goalCol}
  ]
]

resolveInfix[infix_, lines:{___String}] :=
  {LintTimesCharacter} ~Join~ infix


pushOutBefore[one:LeafNode[Token`Fake`ImplicitOne, _, data_], lines:{___String}] :=
Module[{origLineNumber, origColumnNumber, lineNumber, columnNumber,
  line, hitSomething},

  origLineNumber = data[[Key[Source], 1, 1]];
  origColumnNumber = data[[Key[Source], 1, 2]];

  lineNumber = origLineNumber;
  columnNumber = origColumnNumber;
  line = lines[[lineNumber]];

  hitSomething = False;

  While[True,
    If[columnNumber-1 == 0,
      Break[]
    ];
    (*
    this test is just to ensure that StringTake will succeed

    this test should never be true, but there are cases in the parser
    where the lines in the reported Source do not match the lines of the
    actual source, so that line is not actually a line with a ;;

    it is VERY BAD if this gets stuck in an infinite loop, so just do extra check
    *)
    If[StringLength[line] < columnNumber-1,
      Break[]
    ];
    If[StringTake[line, {columnNumber-1}] != " ",
      hitSomething = True;
      Break[]
    ];
    columnNumber--;
  ];

  If[hitSomething,
    {LintOneCharacter, lineNumber, columnNumber}
    ,
    (*
    did not hit anything, so just use original position
    *)
    {LintOneCharacter, origLineNumber, origColumnNumber}
  ]
]


pushOutAfter[all:LeafNode[Token`Fake`ImplicitAll, _, data_], lines:{___String}] :=
Module[{origLineNumber, origColumnNumber, lineNumber, columnNumber,
  line, hitSomething},

  origLineNumber = data[[Key[Source], 1, 1]];
  origColumnNumber = data[[Key[Source], 1, 2]];

  lineNumber = origLineNumber;
  columnNumber = origColumnNumber;
  line = lines[[lineNumber]];

  hitSomething = False;

  While[True,
    If[columnNumber-1 == StringLength[line],
      Break[]
    ];
    (*
    see similar comment above
    *)
    If[StringLength[line] < columnNumber,
      Break[]
    ];
    If[StringTake[line, {columnNumber}] != " ",
      hitSomething = True;
      Break[]
    ];
    columnNumber++;
  ];

  If[hitSomething,
    {LintAllCharacter, lineNumber, columnNumber}
    ,
    (*
    did not hit anything, so just use original position
    *)
    {LintAllCharacter, origLineNumber, origColumnNumber}
  ]
]


End[]

EndPackage[]
