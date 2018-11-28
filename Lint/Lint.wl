BeginPackage["Lint`"]

LintFile::usage = "LintFile[file] returns a list of lints in file."

LintString::usage = "LingString[string] returns a list of lints in string."


LintFileReport::usage = "LintFileReport[file, lints] prints a report of all the lints in file."

LintStringReport::usage = "LintStringReport[string, lints] prints a report of all the lints in string."



Lint::usage = "Lint[description, severity, data] is a problem found in source code."


Begin["`Private`"]

Needs["AST`"]
Needs["Lint`Run`"]


$lineWidth = 500;



severityToColor[sev_] :=
Switch[sev,
    "Remark", Blue,
    "Warning", Orange,
    "Error", Red,
    "Fatal", Red
  ]



insert[str_, sev_, col_, underline_] :=
 Module[{replaced, taken, color},

  color = severityToColor[sev];

  (*XXXPrint["insert: ",{StringLength[underline],col}]*)
  Which[
   col > $lineWidth,
   underline
   ,
   col > Length[underline],
   underline
   ,
   col < 1,
   underline
   ,
   True,
   ReplacePart[underline, 
    Tooltip[Style["\[ErrorIndicator]", color], sev <> "\n" <> str], col]
   ]
  ]

(*
I originally wanted to do this:
Style[Underscript[line, under], ScriptSizeMultipliers->1]

But there is no guarantee that the 2 lines will be aligned
So brute-force it with Grid so that it looks good
*)
decorate[line_, under_] :=
 Module[{},
  If[Length[line] =!= Length[under], 
   Print["line and under not same length: ", {line, under}]; 
   Throw[1, 1]];
  Grid[{line, under}, Spacings -> {0, 0}]
  ]

modify[lines_, lints_, lineNumber_] :=
 Module[{warningsCols, line, under, warningInserters, locations, 
   spans, cases, sorted, end, start, startMsg, endMsg, startSev, endSev,
   entireMiddleLines},
  locations = {};
  end = False;
  start = False;

  (*
  setup spans
  *)
  spans = 
   Flatten[Cases[lints, 
     Lint[msg_, 
       sev_, <|Source -> {{lineNumber, col1_}, {lineNumber, col2_}}|>] :> 
      Table[{msg, sev, i, "SameLine"}, {i, col1, col2}]], 1];

  (*
  setup spans and locations
  *)
  Scan[Switch[#,
      Lint[_, _, <|Source -> {{lineNumber, _}, {lineNumber, _}}|>],
      Null
      ,
      Lint[_, _, <|Source -> {{lineNumber, _}, _}|>],
      end = True;
      endMsg = #[[1]];
      endSev = #[[2]];
      Do[
       AppendTo[spans, {#[[1]], #[[2]], i, "AfterLine"}]
       ,
       {i, #[[3]][Source][[1, 2]] + 1, StringLength[lines[[lineNumber]]]}
       ];
      AppendTo[locations, {#[[1]], #[[2]], #[[3]][Source][[1, 2]], "EndOfLine"}];
      ,
      Lint[_, _, <|Source -> {_, {lineNumber, _}}|>],
      start = True;
      startMsg = #[[1]];
      startSev = #[[2]];
      Do[
       AppendTo[spans, {#[[1]], #[[2]], i, "BeforeLine"}]
       ,
       {i, 1, #[[3]][Source][[2, 2]] - 1}
       ];
      AppendTo[locations, {#[[1]], #[[2]], #[[3]][Source][[2, 2]], "StartOfLine"}];
      ,
      _, Null
    ];&
    ,
    lints
  ];

  (*
  setup entireMiddleLines
  *)
  entireMiddleLines = 
   Flatten[Cases[
     lints, (Lint[msg_, sev_, <|Source -> {{lineNumber1_, _}, {lineNumber2_, _}}|>] /; (lineNumber1 < lineNumber < lineNumber2)) :> 
      Table[{msg, sev, i, "EntireLine"}, {i, 1, StringLength[lines[[lineNumber]]]}]], 1];

  If[entireMiddleLines =!= {},
   start = True;
   startMsg = entireMiddleLines[[1]][[1]];
   startSev = entireMiddleLines[[1]][[2]];
   end = True;
   endMsg = entireMiddleLines[[1]][[1]];
   endSev = entireMiddleLines[[1]][[2]];
   ];

  cases = Union[locations ~Join~ spans ~Join~ entireMiddleLines];

  warningInserters = 
   AssociationMap[
    With[{x1 = #[[1]], x2 = #[[2]], x3 = #[[3]]}, 
      Function[{line}, insert[x1, x2, x3, line]]
    ]&, cases];
  
  If[$Debug,
    Print[warningInserters]
  ];

  sorted = 
   Take[Values[
     KeySortBy[
      warningInserters, -Which[#[[3]] === 
          "BeforeLine", -20000 + #[[2]], #[[3]] === "AfterLine", 
         20000 + #[[2]], #[[3]] === "StartOfLine", -10000, #[[3]] === 
          "EndOfLine", 10000, True, #[[2]]] &]], UpTo[Infinity]];

  line = lines[[lineNumber]];
  line = Characters[line];
  (*line=line<>" ";*)

  (*
  We want everything to be 1 character wide
  *)
  line = ReplaceAll[line, "\t" -> " "];
  
  under = Table["\[SpaceIndicator]", {StringLength[line]}];
  Scan[(under = #[under]) &, sorted];
  If[start,
   line = Join[{" "}, line];
   under = Join[{Tooltip[Style["\[Continuation]", severityToColor[startSev]], startSev<>"\n"<>startMsg]}, under];
   ,
   line = Join[{" "}, line];
   under = Join[{" "}, under];
   ];
  If[end,
   line = Join[line, {" "}];
   under = Join[under, {Tooltip[Style["\[Continuation]", severityToColor[endSev]], endSev<>"\n"<>endMsg]}];
   ,
   line = Join[line, {" "}];
   under = Join[under, {" "}];
   ];

  decorate[line, under]
  ]



Options[LintFileReport] = {
"FileNamePrefixPattern" -> ""
}


LintFileReport[file_String, lints:{___Lint}, OptionsPattern[]] :=
 Module[{lines},

  lines = Import[file, {"Text", "Lines"}, CharacterEncoding -> "ASCII"];

  lintLinesReport[lines, lints]
]


LintStringReport[string_String, lintsIn:{___Lint}] :=
 Module[{lines},

  lines = ImportString[string, {"Text", "Lines"}, CharacterEncoding -> "ASCII"];

  lintLinesReport[lines, lints]
]



lintLinesReport[linesIn_, lintsIn_] :=
Module[{lints, lines},
  
  lints = lintsIn;

  lints = Cases[lints, Lint[_, _, _]];
  
  If[lints == {},
    Return[]
  ];

  lines = linesIn;

   lines = Append[lines, ""];
   lines = StringTake[#, UpTo[$lineWidth]] & /@ lines;
   sources = Cases[lints, Lint[_, _, opts_] :> opts[Source]];
   
    If[$Debug,
      Print[{"sources", sources}]
    ];

   warningsLines = sources[[All, All, 1]];
   linesToModify = 
    Union[Take[Sort[Flatten[Range @@@ warningsLines]], UpTo[20]]];
   Do[
    lines[[i]] = modify[lines, lints, i]
    ,
    {i, linesToModify}
    ];
   
   Do[
    Print[Style[Row[{"line ", l, ": ", lines[[l]]}]]]
    ,
    {l, linesToModify}
    ]

]




End[]

EndPackage[]


