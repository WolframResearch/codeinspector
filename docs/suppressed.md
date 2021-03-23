
# Suppressed Regions and Suppressed Tags

## Suppressed Regions

It is convenient to have a way to disable certain lints within a region of code


```
(* CodeInspect::Push *)
(* CodeInspect::Suppress::PatternRule *)

f[a_ -> a]

(* CodeInspect::Pop *)
```



### Arguments

Some lints require an additional argument:

```
(* CodeInspect::Push *)
(* CodeInspect::Suppress::DuplicateClauses::If *)

If[a, b, b]

(* CodeInspect::Pop *)
```


### Inside of functions

The comment annotations do not have to be only at top-level.

They can appear inside of functions (as long as they are in the same piece of syntax, e.g., CompoundExpression)

```
example with comments in CompoundExpression
```





## Suppressed Tags

CurrentValue[XX]






