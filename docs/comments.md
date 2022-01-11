
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






## in-the-clear

authored by humans in .m files

from mon mar 29 sw codetools meeting

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::PatternTest:: *)

(* :!CodeAnalysis::EndBlock:: *)




(* :!CodeFormatting::BeginBlock:: *)
(* :!CodeFormatting::LineWidth::120:: *)

(* :!CodeFormatting::EndBlock:: *)




(* :!CodeInstrumenting::BeginBlock:: *)
(* :!CodeInstrumenting::Ignore:: *)

(* :!CodeInstrumenting::EndBlock:: *)






## in-the-blind

managed by the FE

CodeInspector can consume it


actual:
(* ::Package::"Tags"->Association["tag1" -> Association[Enabled -> True], "tag2" -> Association[Enabled -> False]]:: *)

If[a, b, b]




actual:

(* ::Code::Initialization::"Tags"->Association["tag1" -> Association[Enabled -> True], "tag2" -> Association[Enabled -> False]]:: *)
If[a, b, b]


(* ::Code::Initialization::"Tags"*) only applies to next cell!






(* set *)
CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions",
   "CodeInspect", "Tags", "tag1", Enabled}, True] = False
(* query *)
AbsoluteCurrentValue[$FrontEnd, {CodeAssistOptions,
  "CodeToolsOptions", "CodeInspect", "Tags", "tag1", Enabled}, True]

Why?  Partly because you don't want to be responsible for managing inheritance yourself, and this will easily navigate the inheritance issues in the new world of Association inheritance.  But also partly because it's more flexible.

It allows for things like the following without ambiguity:
* Suppress at the global level, but reenable for a specific notebook.

CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions",
   "CodeInspect", "Tags", "tag1", Enabled}] = False
CurrentValue[_NotebookObject, {CodeAssistOptions, "CodeToolsOptions",
   "CodeInspect", "Tags", "tag1", Enabled}] = True

* Configure a tag with settings other than on/off.  For example, if you have a tag that might have some experimental support to it, or some way of customizing how deep its scans are, whether it uses slower methods for more comprehensive checks, whether it offers fixes which might frequently proven to be questionable or of low quality, etc.
CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions",
   "CodeInspect", "Tags", "tag1", "OfferLowQualityFix"}] = True

If you use this method, you only ever have to make one query.  You query AbsoluteCurrentValue on the CellObject, with a third argument that provides the default value if the setting simply doesn't exist, and let the FE do the rest of the work for you.  The FE is really *very* good at this (association-level inheritance is just a slight spin on the general inheritance mechanism the FE has used for 15 years...it is quite a battle-hardened system).

                                                -John











