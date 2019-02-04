BeginPackage["Lint`ImplicitTimes`"]

ImplicitTimesFile::usage = "ImplicitTimesFile[file, options] returns a list of implicit times in file."

ImplicitTimesString::usage = "ImplicitTimesString[string, options] returns a list of implicit times in string."



ImplicitTimesFileReport::usage = "ImplicitTimesFileReport[file, implicitTimes, options] returns a list of LintedLines in file."

ImplicitTimesStringReport::usage = "ImplicitTimesStringReport[string, implicitTimes, options] returns a list of LintedLines in string."




Begin["`Private`"]

Needs["AST`"]
Needs["Lint`"]
Needs["Lint`Report`"]
Needs["Lint`Format`"]
Needs["Lint`Utils`"]


Options[ImplicitTimesFile] = {
  PerformanceGoal -> "Speed"
}

ImplicitTimesFile[file_, OptionsPattern[]] :=
Catch[
 Module[{full, lines, times, performanceGoal, cst},

 performanceGoal = OptionValue[PerformanceGoal];

   full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <|"FileName"->file|>]]
  ];

   If[performanceGoal == "Speed",
    If[FileByteCount[full] > 1*^6,
     Throw[Failure["FileTooLarge", <|"FileName"->full, "FileSize"->FileSize[full]|>]]
     ];
    ];

    cst = ConcreteParseFile[full];

    If[FailureQ[cst],
      Throw[cst]
    ];

    times = implicitTimes[cst];

   times
]]


Options[ImplicitTimesFile] = {
  PerformanceGoal -> "Speed"
}

ImplicitTimesString[string_, OptionsPattern[]] :=
Catch[
 Module[{times, cst},

   cst = ConcreteParseString[string];

   If[FailureQ[cst],
    Throw[cst]
  ];

    times = implicitTimes[cst];

   times
]]













Options[ImplicitTimesFileReport] = {
  "LineNumberExclusions" -> <||>,
  "LineHashExclusions" -> {}
}



(*
cannot have
ImplicitTimesFileReport[file_String, opts:OptionsPattern[]]

because ImplicitTimesFileReport[file, {}] leads to infinite recursion
*)

ImplicitTimesFileReport[file_String, implicitTimes:{___InfixNode}, OptionsPattern[]] :=
Catch[
 Module[{full, lines, lineNumberExclusions, lineHashExclusions},

 lineNumberExclusions = OptionValue["LineNumberExclusions"];
 lineHashExclusions = OptionValue["LineHashExclusions"];

   full = FindFile[file];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <|"FileName"->file|>]]
  ];

   If[FileByteCount[full] == 0,
   Throw[Failure["EmptyFile", <|"FileName"->full|>]]
   ];
   (*
    bug 163988
    Use CharacterEncoding -> "ASCII" to guarantee that newlines are preserved
    *)
   lines = Import[full, {"Text", "Lines"}, CharacterEncoding -> "ASCII"];

   implicitTimesLinesReport[lines, implicitTimes, lineNumberExclusions, lineHashExclusions]
]]



Options[ImplicitTimesStringReport] = {
  "LineNumberExclusions" -> <||>,
  "LineHashExclusions" -> {}
}

ImplicitTimesStringReport[string_String, implicitTimes:{___InfixNode}, OptionsPattern[]] :=
Catch[
 Module[{lines, lineNumberExclusions, lineHashExclusions},

 lineNumberExclusions = OptionValue["LineNumberExclusions"];
 lineHashExclusions = OptionValue["LineHashExclusions"];

 If[StringLength[string] == 0,
  Throw[Failure["EmptyString", <||>]]
 ];
    (*
    bug 163988
    Use CharacterEncoding -> "ASCII" to guarantee that newlines are preserved
    *)
   lines = ImportString[string, {"Text", "Lines"}, CharacterEncoding -> "ASCII"];

   implicitTimesLinesReport[lines, implicitTimes, lineNumberExclusions, lineHashExclusions]
]]





implicitTimes[cst_] :=
Catch[
Module[{implicitTimes, sources, starts, ends, infixs},

  implicitTimes = Cases[cst, InfixNode[ImplicitTimes, nodes_, opts_], {0, Infinity}];

   implicitTimes
]]








$columnLimit = 500
$lineLimit = 20

(* how many (, ), or \[Times] to insert per line *)
$markupLimit = 100


modify[lineIn_String, {starts_, ends_, infixs_}, lineNumber_] :=
 Module[{line, startCols, endCols, infixCols, startInserters, endInserters, infixInserters, under,
  rules, underLength},

  startCols = Cases[starts, {lineNumber, col_} :> col];
  endCols = Cases[ends, {lineNumber, col_} :> col + 1];
  infixCols = Cases[infixs, {lineNumber, col_} :> col];

  line = lineIn;

  startInserters = AssociationMap[LintedCharacter["(", {Lint["ImplicitTimes", "ImplicitTimes", "ImplicitTimes", <||>]}, FontWeight->Bold, FontSize->Larger]&, startCols];
  endInserters = AssociationMap[LintedCharacter[")", {Lint["ImplicitTimes", "ImplicitTimes", "ImplicitTimes", <||>]}, FontWeight->Bold, FontSize->Larger]&, endCols];
  infixInserters = AssociationMap[LintedCharacter[LintTimes, {Lint["ImplicitTimes", "ImplicitTimes", "ImplicitTimes", <||>]}, FontWeight->Bold, FontSize->Larger]&, infixCols];

  If[Intersection[Keys[startInserters], Keys[endInserters]] =!= {},
    Throw["intersection start and end", "Unhandled"]
  ];

  If[Intersection[Keys[startInserters], Keys[infixInserters]] =!= {},
    Throw["intersection start and infix", "Unhandled"]
  ];

  If[Intersection[Keys[endInserters], Keys[infixInserters]] =!= {},
    Throw["intersection end and infix", "Unhandled"]
  ];

  rules = Join[startInserters, endInserters, infixInserters];
  rules = Normal[rules];

  underLength = StringLength[line];
  (*
  extend line to be able to insert \[Times] after the line, when ImplicitTimes spans lines
  *)
  underLength = underLength + 1;
  under = Table[" ", {underLength}];

  under = ReplacePart[under, rules];

  (*
  to match Listify
  *)
  under = Join[{" "}, under];

  under
  ]

(*
return {line, col} for all \[Times] symbols
*)
processPair[{left_, right_}] :=
 Module[{leftSource, rightSource},

  leftSource = left[[3]][Source];
  rightSource = right[[3]][Source];
   (*
   same line
   
   this is symbolically represented as the best placement between left and right at this stage

   Actual tokenization needs to occur later to figure out actual the column
   *)
   BestImplicitTimesPlacement[{leftSource[[2]], rightSource[[1]]}]
   ]

processChildren[nodes_List] :=
 Module[{pars},
  pars = Partition[nodes, 2, 1];
  processPair /@ pars
  ]

implicitTimesLinesReport[linesIn_List, implicitTimesIn:{___InfixNode}, lineNumberExclusionsIn_Association, lineHashExclusionsIn_List] :=
Catch[
 Module[{implicitTimes, sources, starts, ends, infixsIn, infixs, lines, hashes,
  lineNumberExclusions, lineHashExclusions, implicitTimesExcludedByLineNumber, tmp},

    If[implicitTimes === {},
      Throw[{}]
    ];

    implicitTimes = implicitTimesIn;

    lines = linesIn;
    hashes = (IntegerString[Hash[#], 16, 16])& /@ lines;
    lines = StringTake[#, UpTo[$columnLimit]]& /@ lines;

    lineNumberExclusions = lineNumberExclusionsIn;

    lineNumberExclusions = expandLineNumberExclusions[lineNumberExclusions];

    lineHashExclusions = lineHashExclusionsIn;

    (* Association of lineNumber -> All *)
    tmp = Association[Table[If[MemberQ[lineHashExclusions, hashes[[i]]], i -> All, Nothing], {i, 1, Length[lines]}]];
    lineNumberExclusions = lineNumberExclusions ~Join~ tmp;


    (*
    implicitTimes that match the line numbers and tags in lineNumberExclusions
    *)
    implicitTimesExcludedByLineNumber = Catenate[KeyValueMap[Function[{line, tags},
        If[tags === All,
          Cases[implicitTimes, InfixNode[_, _, KeyValuePattern[Source -> {{line1_ /; line1 == line, _}, {_, _}}]]]
          ,
          (* warn? *)
          {}
        ]
      ],
      lineNumberExclusions]];

    implicitTimes = Complement[implicitTimes, implicitTimesExcludedByLineNumber];


    sources = #[Source]& /@ implicitTimes[[All, 3]];

   starts = sources[[All, 1]];
   ends = sources[[All, 2]];

   infixs = processChildren /@ implicitTimes[[All, 2]];

   infixs = Flatten[infixs, 1];

   (*
    resolve BestImplicitTimesPlacement with actual columns now
   *)
   infixs = Map[resolveInfix[#, lines]&, infixs];








   linesToModify = Union[starts[[All, 1]] ~Join~ ends[[All, 1]] ~Join~ infixs[[All, 1]]];

   Table[

     LintedLine[lines[[i]], i, hashes[[i]], {ListifyLine[lines[[i]], <||>, "EndOfFile" -> (i == Length[lines])],
                                  modify[lines[[i]], {starts, ends, infixs}, i]},
                                  {}]
    ,
    {i, linesToModify}
    ]
]]




resolveInfix[BestImplicitTimesPlacement[span_], lines_] :=
Module[{lineNumber, line, tokens, goalLine, goalCol, tokenSource, spaces, spaceRanges, candidates, edges, offset, intersection},

  Which[
    span[[1, 1]] != span[[2, 1]],
      (* different lines, so place \[Times] at end of first line *)
      {span[[1, 1]], StringLength[lines[[span[[1, 1]]]]] + 1}
    ,
    span[[1, 2]] + 1 == span[[2, 2]],
      (* contiguous *)
      {span[[1, 1]], span[[1, 2]] + 1}
    ,
    span[[1, 2]] + 1 == span[[2, 2]] - 1,
      (* optimization case to avoid calling TokenizeString: only 1 space between *)
      {span[[1, 1]], span[[1, 2]] + 1}
    ,
    True,
      (* do actual work to figure out best placement *)
      goalLine = span[[1, 1]];
      mean = N[Mean[{span[[1, 2]], span[[2, 2]]}]];
      lineNumber = span[[1, 1]];
      line = lines[[lineNumber]];

      (* only tokenize the characters in-between *)
      line = StringTake[line, {span[[1,2]]+1, span[[2,2]]-1}];

      tokens = TokenizeString[line];
      tokens = Most[tokens];

      offset = span[[1, 2]];
      
      (*
      any space is a candidate
      *)
      spaces = Cases[tokens, Token[Token`Space, _, _]];
      spaceRanges = offset + Flatten[Range @@ #[[3]][Source][[All, 2]]& /@ spaces];
      
      (*
      the gaps on either side of a comment are only candidates if they are not next to a space (because the space
      itself is preferred) 
      *)
      comments = Cases[tokens, Token[Token`Comment, _, _]];
      gaps = #[[3]][Source][[1, 2]]& /@ comments;
      excludes = SequenceCases[tokens, {Token[Token`Space, _, _], c:Token[Token`Comment, _, _]} :> c];
      gaps = offset + Complement[gaps, excludes];

      edges = offset + {1, StringLength[line]+1};

      candidates = Union[spaceRanges ~Join~ gaps ~Join~ edges];

      (*
      Which candidates are closest to mean?
      *)
      goals = MinimalBy[candidates, Abs[# - mean]&];

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

      {goalLine, goalCol}
  ]]

resolveInfix[infix_, lines_] :=
  infix



End[]

EndPackage[]