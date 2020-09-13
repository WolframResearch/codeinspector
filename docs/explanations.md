
# Explanations for various lints

## Why am I seeing "Unexpected character: \\:0001." ?

```
In[20]:= CodeInspect["\"\\:0001\""]

Out[20]= {
CodeInspector`InspectionObject[
 "UnexpectedCharacter", "Unexpected character: ``\\:0001``.", 
  "Warning", 
Association[
  CodeParser`Source -> {{1, 2}, {1, 8}}, ConfidenceLevel -> 0.95]]}
```

It's an ASCII control character that is not in common use.

With high confidence, if an ASCII control character appears in code, it is the result of some bad copy-paste error originating in the FE, or a typo from the user.

Note: The lint is not 100% confident.


## Why am I seeing "\*Q function in symbolic solver. Did you mean to do this?" ?

email "Suggestion for linter" 11/4/2019

Related bugs: 358571, 364894, 381950

Users love using the “numeric" \*Q functions in the symbolic solvers, and they just don’t work. From an example today (bug 381950):

```
In[71]:= FindInstance[EvenQ[p], p, Primes]
Out[71]= {}
```

To which the user says “Why doesn’t FindInstance find 2! It’s prime and Even!”

The short answer is that EvenQ gets evaluated on the symbolic p and outputs False before FindInstance even starts, giving the user:

```
FindInstance[False, p, Primes]
```

This affects
- Refine (Refine[EvenQ[2 p], Element[p, Integers]])
- Reduce
- Solve
- FindInstance (FindInstance[EvenQ@p, p, Primes])
- Assuming (Assuming[p \\[Element] Integers, EvenQ[2 p]])

etc…










