
# Explanations

## why is "\:0001" marked with a warning?

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


The lint is not 100% confident.




