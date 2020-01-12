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

removeIgnoredNodes[node_[tag_, childrenIn_, dataIn_], ignoredNodesSrcMemberFunc_SourceMemberQFunction] :=
Module[{children, data, syntaxIssues, abstractSyntaxIssues},

  children = childrenIn;
  data = dataIn;

  children = DeleteCases[children, n_ /; ignoredNodesSrcMemberFunc[n[[3, Key[Source] ]] ]];
  children = removeIgnoredNodes[#, ignoredNodesSrcMemberFunc]& /@ children;
  
  syntaxIssues = Lookup[data, SyntaxIssues, {}];
  If[syntaxIssues != {},
    syntaxIssues = DeleteCases[syntaxIssues, n_ /; ignoredNodesSrcMemberFunc[n[[4, Key[Source] ]] ]];
    data[SyntaxIssues] = syntaxIssues;
  ];

  abstractSyntaxIssues = Lookup[data, AbstractSyntaxIssues, {}];
  If[abstractSyntaxIssues != {},
    abstractSyntaxIssues = DeleteCases[abstractSyntaxIssues, n_ /; ignoredNodesSrcMemberFunc[n[[4, Key[Source] ]] ]];
    data[AbstractSyntaxIssues] = abstractSyntaxIssues;
  ];

  node[tag, children, data]
]



End[]

EndPackage[]
