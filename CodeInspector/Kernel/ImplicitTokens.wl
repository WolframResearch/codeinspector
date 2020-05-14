BeginPackage["CodeInspector`ImplicitTokens`"]

CodeInspectImplicitTokens

CodeInspectImplicitTokensSummarize


CodeInspectImplicitTokensCST

CodeInspectImplicitTokensCSTSummarize



$ImplicitTokensLimit


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Abstract`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Summarize`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`Utils`"]



(*
How many implicit tokens to keep?
*)
$ImplicitTokensLimit = 20



CodeInspectImplicitTokens::usage = "CodeInspectImplicitTokens[code] returns a list of implicit tokens in code."

Options[CodeInspectImplicitTokens] = {
  PerformanceGoal -> "Speed",
  "TabWidth" -> ("TabWidth" /. Options[CodeConcreteParse])
}


$fileByteCountMinLimit = 0*^6
$fileByteCountMaxLimit = 3*^6



CodeInspectImplicitTokens[File[file_String], opts:OptionsPattern[]] :=
Catch[
 Module[{full, performanceGoal, cst},

 performanceGoal = OptionValue[PerformanceGoal];

   full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <|"FileName"->file|>]]
  ];

   If[performanceGoal == "Speed",
    If[FileByteCount[full] > $fileByteCountMaxLimit,
     Throw[Failure["FileTooLarge", <|"FileName"->full, "FileSize"->FileSize[full]|>]]
     ];
    If[FileByteCount[full] < $fileByteCountMinLimit,
     Throw[Failure["FileTooSmall", <|"FileName"->full, "FileSize"->FileSize[full]|>]]
     ];
    ];

    cst = CodeConcreteParse[File[full], FilterRules[{opts}, Options[CodeConcreteParse]]];

    CodeInspectImplicitTokensCST[cst]
]]





CodeInspectImplicitTokens[string_String, opts:OptionsPattern[]] :=
Catch[
Module[{cst},

  cst = CodeConcreteParse[string, FilterRules[{opts}, Options[CodeConcreteParse]]];

  CodeInspectImplicitTokensCST[cst]
]]




CodeInspectImplicitTokensCST[cst_] :=
Catch[
Module[{times, agg, spans, nulls},

  If[FailureQ[cst],
    Throw[cst]
  ];

  agg = Aggregate[cst];

  times = implicitTimes[agg];
  spans = implicitSpans[agg];
  nulls = implicitNulls[agg];

  Join[times, spans, nulls]
]]




CodeInspectImplicitTokensSummarize::usage = "CodeInspectImplicitTokensSummarize[code] returns an inspection summary object."

Options[CodeInspectImplicitTokensSummarize] = {
  "TabWidth" -> ("TabWidth" /. Options[CodeConcreteParse])
}

CodeInspectImplicitTokensSummarize[File[file_String], implicitTokensIn:_List:Automatic, opts:OptionsPattern[]] :=
Catch[
Module[{implicitTokens, full, lines, lintedLines, bytes, str, tabWidth},

  implicitTokens = implicitTokensIn;

  tabWidth = OptionValue["TabWidth"];

  full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <|"FileName"->file|>]]
  ];

  If[FileByteCount[full] == 0,
    Throw[Failure["EmptyFile", <|"FileName"->full|>]]
  ];

  If[implicitTokens === Automatic,
    implicitTokens = CodeInspectImplicitTokens[File[full], FilterRules[{opts}, Options[CodeInspectImplicitTokens]]];
  ];

  bytes = Import[full, "Byte"];

  str = SafeString[bytes];

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
  "TabWidth" -> ("TabWidth" /. Options[CodeConcreteParse])
}

CodeInspectImplicitTokensCSTSummarize[cst_, implicitTokensIn:_List:Automatic, OptionsPattern[]] :=
Catch[
Module[{implicitTokens, lines, lintedLines, string, tabWidth},

  If[FailureQ[cst],
    Throw[cst]
  ];

  implicitTokens = implicitTokensIn;

  tabWidth = OptionValue["TabWidth"];

  If[implicitTokens === Automatic,
    implicitTokens = CodeInspectImplicitTokensCST[cst];
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

implicitNulls[agg_] :=
Catch[
Module[{nulls},

  nulls = Cases[agg, InfixNode[Comma | CompoundExpression, nodes_ /; !FreeQ[nodes, LeafNode[Token`Fake`ImplicitNull, _, _], 1], _], {0, Infinity}];

  nulls
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

  rules = Merge[inserters, (LintMarkup[#, FontWeight->Bold, FontSize->Larger, FontColor->$color])& @* mergeCharacters];

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

mergeCharacters[{LintTimesCharacter}] = LintTimesCharacter
mergeCharacters[{LintOneCharacter}] = LintOneCharacter
mergeCharacters[{LintAllCharacter}] = LintAllCharacter
mergeCharacters[{LintNullCharacter}] = LintNullCharacter
mergeCharacters[{"("}] = "("
mergeCharacters[{")"}] = ")"

mergeCharacters[{LintTimesCharacter, LintOneCharacter}] = LintTimesOneCharacter
mergeCharacters[{LintTimesCharacter, LintAllCharacter}] = LintAllTimesCharacter
mergeCharacters[{LintAllCharacter, LintTimesCharacter}] = LintAllTimesCharacter
mergeCharacters[{LintTimesCharacter, LintOneCharacter, LintAllCharacter}] = LintAllTimesOneCharacter
mergeCharacters[{LintAllCharacter, LintOneCharacter, LintTimesCharacter}] = LintAllTimesOneCharacter

mergeCharacters[{"(", LintOneCharacter}] = LintOpenOneCharacter
mergeCharacters[{")", LintAllCharacter}] = LintAllCloseCharacter
mergeCharacters[{")", LintNullCharacter}] = LintNullCloseCharacter
mergeCharacters[{"(", "("}] = LintOpenOpenCharacter
mergeCharacters[{")", ")"}] = LintCloseCloseCharacter
mergeCharacters[{")", LintTimesCharacter, LintOneCharacter}] = LintCloseTimesOneCharacter
mergeCharacters[{")", LintOneCharacter, LintTimesCharacter}] = LintCloseTimesOneCharacter

mergeCharacters[args___] := Failure["InternalUnhandled", <|"Function"->mergeCharacters, "Arguments"->{args}|>]


(*
return {line, col} for all \[Times] symbols
*)
processPar[{left_, LeafNode[Token`Fake`ImplicitTimes, _, _], right_}] :=
 Module[{leftSource, rightSource},

  leftSource = left[[3, Key[Source] ]];
  rightSource = right[[3, Key[Source] ]];
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

implicitTokensLinesReport[linesIn:{___String}, implicitTokensIn:_List] :=
Catch[
 Module[{implicitTokens, sources, starts, ends, infixs, lines, linesToModify, times, ones, alls, nulls, charInfos, charInfoPoss},

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
   nulls = Union[Cases[implicitTokens, LeafNode[Token`Fake`ImplicitNull, _, _], {0, Infinity}]];

   times = processChildren /@ times[[All, 2]];

   times = Flatten[times, 1];

   (*
    resolve BestImplicitTimesPlacement with actual columns now
   *)
   times = Map[resolveInfix[#, lines]&, times];

   ones = {LintOneCharacter, #[[3, Key[Source], 1, 1]], #[[3, Key[Source], 1, 2]]}& /@ ones;
   alls = {LintAllCharacter, #[[3, Key[Source], 1, 1]], #[[3, Key[Source], 1, 2]]}& /@ alls;
   nulls = {LintNullCharacter, #[[3, Key[Source], 1, 1]], #[[3, Key[Source], 1, 2]]}& /@ nulls;

  infixs = times ~Join~ ones ~Join~ alls ~Join~ nulls;

   charInfos = starts ~Join~ ends ~Join~ infixs;

   If[$Debug,
    Print["charInfos: ", charInfos]
   ];

   (*
   Make sure to sort using Union before taking
   *)
   charInfoPoss = Union[charInfos[[All, 2;;3]]];

   charInfoPoss = Take[charInfoPoss, UpTo[$ImplicitTokensLimit]];

    
    charInfos = SortBy[charInfos, #[[2;;3]]&];

    charInfos = Cases[charInfos, {_, line_, col_} /; MemberQ[charInfoPoss, {line, col}]];

   linesToModify = Union[charInfos[[All, 2]]];

   Table[

     InspectedLineObject[lines[[i]], i, {ListifyLine[lines[[i]], <||>, "EndOfFile" -> (i == Length[lines])],
                                  modify[lines[[i]], charInfos, i]},
                                  {}]
    ,
    {i, linesToModify}
    ]
]]



(*

BestImplicitTimesPlacement[span_] is something like {{startLine_, startCol_}, {endLine_, endCol_}}
*)
resolveInfix[BestImplicitTimesPlacement[span_], lines:{___String}] :=
Module[{lineNumber, line, tokens, goalLine, goalCol, spaces, spaceRanges, candidates, edges, offset, intersection,
	mean, comments, gaps, excludes, goals},

  If[$Debug,
    Print["resolveInfix: ", {BestImplicitTimesPlacement[span], lines}];
  ];

  Which[
    span[[1, 1]] != span[[2, 1]],
      (* different lines, so place \[Times] at end of first line *)
      {LintTimesCharacter, span[[1, 1]], StringLength[lines[[span[[1, 1]] ]] ] + 1}
    ,
    span[[1, 2]] == span[[2, 2]],
      (* contiguous *)
      {LintTimesCharacter, span[[1, 1]], span[[1, 2]]}
    ,
    span[[1, 2]] + 1 == span[[2, 2]],
      (* optimization case to avoid calling TokenizeString: only 1 space between *)
      {LintTimesCharacter, span[[1, 1]], span[[1, 2]]}
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
      *)
      spaces = Cases[tokens, LeafNode[Whitespace, _, _]];
      spaceRanges = offset - 1 + Flatten[Range @@ #[[3, Key[Source], All, 2]]& /@ spaces];
      
      If[$Debug,
        Print["spaceRanges: ", spaceRanges];
      ];

      (*
      the gaps on either side of a comment are only candidates if they are not next to a space (because the space
      itself is preferred) 
      *)
      comments = Cases[tokens, LeafNode[Token`Comment, _, _]];
      gaps = #[[3, Key[Source], 1, 2]]& /@ comments;
      excludes = SequenceCases[tokens, {LeafNode[Whitespace, _, _], c:LeafNode[Token`Comment, _, _]} :> c];
      gaps = offset - 1 + Complement[gaps, excludes];

      If[$Debug,
        Print["gaps: ", gaps];
      ];

      edges = offset - 1 + {0, StringLength[line]+1};

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
        intersection =!= {}
        ,
        (* always prefer spaces over gaps or edges *)
        goalCol = intersection[[1]];
        ,
        True,
        (* only comments; arbitrarily choose one *)
        goalCol = goals[[1]];
      ];

      {LintTimesCharacter, goalLine, goalCol}
  ]]

resolveInfix[infix_, lines:{___String}] :=
  {LintTimesCharacter} ~Join~ infix



End[]

EndPackage[]
