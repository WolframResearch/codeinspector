BeginPackage["Lint`Folds`"]


removeIgnoredNodes


Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]




removeIgnoredNodes[Null, _SourceMemberQFunction] := Null

removeIgnoredNodes[l_LeafNode, _SourceMemberQFunction] := l

(* optimization *)
removeIgnoredNodes[node_, SourceMemberQFunction[{}]] :=
  node

removeIgnoredNodes[node_[tag_, childrenIn_, data_], ignoredNodesSrcMemberFunc_SourceMemberQFunction] :=
Module[{children},

  children = childrenIn;

  children = DeleteCases[children, n_ /; ignoredNodesSrcMemberFunc[n[[3]][Source]]];

  node[tag, removeIgnoredNodes /@ children, data]
]



End[]

EndPackage[]
