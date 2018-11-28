BeginPackage["Lint`ImplicitMultiplication`"]


ImplicitMultiplicationFile::usage = "ImplicitMultiplicationFile[file] returns data for all implicit multiplications in file."

ImplicitMultiplicationString::usage = "ImplicitMultiplicationString[string] returns data for all implicit multiplications in string."



ImplicitMultiplicationFileReport::usage = "ImplicitMultiplicationFileReport[file, data] prints a report of all the implicit multiplications in file."

ImplicitMultiplicationStringReport::usage = "ImplicitMultiplicationStringReport[string, data] prints a report of all the implicit multiplications in string."




Begin["`Private`"]

Needs["AST`"]




ImplicitMultiplicationFile[file_String] :=
Catch[
Module[{ast},

  If[FileType[file] =!= File,
   Throw[Failure["NotAFile", <|"FileName"->file|>]]
   ];

  If[FileByteCount[file] > 1*^6,
   Throw[Failure["FileTooLarge", <|"FileName"->file, "FileSize"->FileSize[file]|>]]
   ];

  ast = ParseFile[file];

  implicitMultiplication[ast]
]]

ImplicitMultiplicationString[string_String] :=
Catch[
Module[{ast},

  ast = ParseString[string];

  implicitMultiplication[ast]
]]





implicitMultiplication[ast_?FailureQ] :=
  ast

implicitMultiplication[ast_] :=
Catch[
Module[{spaceTimes, sources, starts, ends, infixs},

  spaceTimes = Cases[ast, InfixNode[InfixImplicitTimes, nodes_, opts_], {0, Infinity}, Heads->True];
  If[spaceTimes === {},
    Throw[Null]
  ];
   
   sources = Cases[spaceTimes, InfixNode[InfixImplicitTimes, nodes_, opts_] :> opts[Source]];

   starts = sources[[All, 1]];
   ends = sources[[All, 2]];

   infixs = 
    Flatten[Cases[spaceTimes, 
      InfixNode[InfixImplicitTimes, nodes_, opts_] :> processNodes[nodes]], 1];

  {sources, starts, ends, infixs}
]]








$columnLimit = 500;
$lineLimit = 20;

insert[str_, col_, line_] :=
 Module[{},
  Which[
   col > $columnLimit,
   line
   ,
   str == "\[Times]" && col == StringLength[line] + 1,
   StringJoin[line, 
    ToString[Style[str, Red, Bold, Larger], StandardForm]]
   ,
   str == "\[Times]" && StringTake[line, {col}] == " ",
   StringReplacePart[line, 
    ToString[Style[str, Red, Bold, Larger], StandardForm], {col, col}]
   ,
   True,
   StringInsert[line, 
    ToString[Style[str, Red, Bold, Larger], StandardForm], col]
   ]
  ]

modify[lines_, {starts_, ends_, infixs_}, lineNumber_] :=
 Module[{startCols, endCols, infixCols, line, startInserters, endInserters, infixInserters},
  startCols = Cases[starts, {lineNumber, col_} :> col];
  endCols = Cases[ends, {lineNumber, col_} :> col + 1];
  infixCols = Cases[infixs, {lineNumber, col_} :> col];
  line = lines[[lineNumber]];

  startInserters = 
   AssociationMap[Function[{line}, insert["(", #1, line]]&, startCols];
  endInserters = 
   AssociationMap[Function[{line}, insert[")", #1, line]]&, endCols];
  infixInserters = 
   AssociationMap[Function[{line}, insert["\[Times]", #1, line]]&, infixCols];

  Scan[(line = #[line])&, 
   Reverse[Take[
     Values[KeySort[
       Join[startInserters, endInserters, infixInserters]]], 
     UpTo[10]]]];

  line
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
   BestImplicitMultiplicationTimesPlacement[{leftSource[[2]], rightSource[[1]]}]
   ]

processNodes[nodes_List] :=
 Module[{pars},
  pars = Partition[nodes, 2, 1];
  processPair /@ pars
  ]

ImplicitMultiplicationFileReport[file_, data_] :=
 Module[{lines},

   lines = Import[file, {"Text", "Lines"}, CharacterEncoding -> "ASCII"];

   implicitMultiplicationLinesReport[lines, data]
  ]

ImplicitMultiplicationStringReport[string_, data_] :=
 Module[{lines},

   lines = ImportString[string, {"Text", "Lines"}, CharacterEncoding -> "ASCII"];

   implicitMultiplicationLinesReport[lines, data]
   ]

implicitMultiplicationLinesReport[linesIn_, data_] :=
Catch[
 Module[{sources, starts, ends, infixsIn, infixs, lines},

    If[FailureQ[data],
      Throw[data]
    ];

    If[data === Null,
      Throw[data]
    ];

    lines = linesIn;

    {sources, starts, ends, infixsIn} = data;

   lines = StringTake[#, UpTo[$columnLimit]]& /@ lines;

   (*
    resolve BestImplicitMultiplicationTimesPlacement with actual columns now
   *)
   infixs = infixsIn;
   infixs = Map[resolveInfix[#, lines]&, infixs];

   linesToModify = Union[starts[[All, 1]] ~Join~ ends[[All, 1]] ~Join~ infixs[[All, 1]]];

   linesToDisplay = 
    Union[Take[Sort[Flatten[Range @@@ sources[[All, All, 1]]]], UpTo[$lineLimit]]];
   
    If[$Debug,
      Print["linesToDisplay: ", linesToDisplay]
    ];

   Do[
    If[MemberQ[linesToDisplay, i],
     lines[[i]] = modify[lines, {starts, ends, infixs}, i];
     ]
    ,
    {i, linesToModify}
    ];

   Do[
    Print[Style[Row[{"line ", l, ": ", lines[[l]]}]]]
    ,
    {l, linesToDisplay}
    ];
]]




resolveInfix[BestImplicitMultiplicationTimesPlacement[span_], lines_] :=
Module[{lineNumber, tokens, goalLine, goalCol, tokenSource},

  If[$Debug,
    Print["resolveInfix: ", BestImplicitMultiplicationTimesPlacement[span]];
  ];

  Which[
    span[[1, 1]] =!= span[[2, 1]],
      (* different lines *)
      {span[[1, 1]], span[[1, 2]] + 1}
    ,
    span[[1, 2]] + 1 === span[[2, 2]],
      (* contiguous *)
      {span[[1, 1]], span[[1, 2]] + 1}
    ,
    True,
      goalLine = span[[1, 1]];
      goalCol = Ceiling[Mean[{span[[1, 2]], span[[2, 2]]}]];
      lineNumber = span[[1, 1]];
      line = lines[[lineNumber]];
      tokens = TokenizeString[line];
      Do[
        tokenSource = token[[3]][Source];
        Which[
          token[[1]] === "Token`SPACE",
          Continue[]
          ,
          (* if we have reached to where the goalCol is occurring before a token, then use that goalCol *)
          goalCol <= tokenSource[[1, 2]],
          Break[]
          ,
          (* goalCol is inside of a token, so move the goal to after this token *)
          goalCol <= tokenSource[[2, 2]],
          goalCol = tokenSource[[2, 2]] + 1;
          Break[];
        ]
        ,
        {token, tokens}
      ];
      {goalLine, goalCol}
  ]
]

resolveInfix[infix_, lines_] :=
  infix



End[]

EndPackage[]

