
# Suppressed Regions and Suppressed Tags

## Suppressed Regions

It is convenient to have a way to disable certain lints within a region of code


```
(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::PatternRule:: *)

f[a_ -> a]

(* :!CodeAnalysis::EndBlock:: *)
```



### Arguments

Some lints require an additional argument:

```
(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::DuplicateClauses::If:: *)

If[a, b, b]

(* :!CodeAnalysis::EndBlock:: *)
```
