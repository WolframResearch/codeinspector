BeginPackage["CodeInspector`TokenRules`"]

$DefaultTokenRules


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`Utils`"]


$DefaultTokenRules = <|

LeafNode[Symbol, _, _] -> scanSymbols,

Nothing
|>



Attributes[scanSymbols] = {HoldRest}

scanSymbols[pos_List, cstIn_] :=
Module[{cst, node, data},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  data = node[[3]];

  {}
]



End[]


EndPackage[]
