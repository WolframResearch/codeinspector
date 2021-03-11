
# Disabled Regions

It is convenient to have a way to disable certain lints within a region of code


```
(* CodeInspect::Push *)
(* CodeInspect::Disable::PatternRule *)

f[a_ -> a]

(* CodeInspect::Pop *)
```



## Arguments

Some lints require an additional argument:

```
(* CodeInspect::Push *)
(* CodeInspect::Disable::DuplicateClauses::If *)

If[a, b, b]

(* CodeInspect::Pop *)
```
