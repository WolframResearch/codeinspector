
## 0.7 - 9 Jan, 2019

Separate Concrete Syntax Trees and Abstract Syntax Trees.

Turn on console colors on Windows where possible.

Added "SeverityExclusions" option with default of {"Remarks"}.

Added several new lints for Module, With, Block.

Added DuplicateClauses lints for Which and Switch.


## 0.8 - 4 Feb, 2019

Some work on documentation notebooks.

Add "ConcreteRules" and "AbstractRules" options for user-provided rules.

Add several new rules concerning Blanks, Optionals, Patterns, nonexistent Symbols, stray semicolons, PatternTest, 


## 0.9 - 1 Apr, 2019

Make DuplicateNamedPattern a warning.

Add symbol Failed to BadSymbols.

Separate SyntaxIssues and AbstractSyntaxIssues.

Allow File[] wrappers to work.

Add Interpretation to Lint objects.

Add UnusedVariables scan.

Various bug fixes.


## 0.10 - 6 May, 2019

Added new rules:
"ImplicitTimesBlanks" for detecting occurrenes of ____ (that is, 4 _ characters)
"AssociationCall" etc. for detecting occurrences a?Association (when AssociationQ was meant)
"SuspiciousPrivateContext" for detecting Begin["Private\`"]

Add DynamicModule scoping rules.

Handling nested With syntax:
With[{a = 1}, {b = a}, b]

"UnusedBlockVariables" has a separate tag, can be enabled separately.

"SelfAssignment" for detecting a = a or a := a.

Provide some selectors for Lint and LintedLine objects.

Updated build scripts.


## 0.11 - 10 Jun, 2019

Added new rules.

Various bug fixes.


## 0.12 - 5 Aug, 2019

Added new rules:
ContiguousImplicitTimesBlanks
ImplicitTimesStrings
AssociationArguments
SessionSymbol
WhichSet
IfSet
TrueCall

Add BadSymbol for Boolean, RealQ, and FalseQ

Add scans for And, Or, and Alternatives

Limit number of Lints when reporting.

Limit lint width of description.

Various bug fixes.


## 0.13 - 16 Sep, 2019

Add scan for List of Rules

Add scan for Fail, RationalQ, ComplexQ, SymbolQ

Add scan for a?Function[args]

Add scans for undocumented symbols.

Add "AdditionalSources" for Lints

Add scans for obsolete symbols.


## 0.14 - 28 Oct, 2019

Remove scan for %, it is redundant

Add scan for a ~f[x]~ b

Allow shadowing lints to be dropped

Work on making lints prettier

Work on CodeActions

Distinguish between SyntaxIssues and FormatIssues

Add ConfidenceLevel to issues

Round of editing the messages for issues

Retrofit a lot of CodeActions

Experiment with new style for lints

Scan # for not containing Function

Add all undocumented symbols.

Add experimental symbols.

Add Match to bad symbols

More work on messages

Properly convert SyntaxErrorNodes into Lints

Update CodeActions in a lot of scans

Work on OutputForm some

If a Lint has CodeActions, it is not considered to be shadow-able

Add External.wl

Add "Open in editor" item when linting files

Punctuation.

Replace ASCII control characters with � when displaying

Sort lints before applying $LintLimit, so that the first ones kept

Apply a $LintsPerLine limit

Treat different groups of duplicated keys as different Lints

Add scan for prefix Plus


### Fixes

Fix fallout from rearranging Blanks into LeafNodes

Fix adding 1 too many fake lines

Fix for LineCol style only


## 0.15 - 15 Jan, 2020

Add Creator field

Add scan for \*Q functions inside of symbolic solvers.

Add scan for Rule___

Add LintBytes and LintBytesReport

Add empty list rule to Module, DynamicModule and Block.

Report leaking Module variables that appear on RHS of RuleDelayed in String functions

Require using File[] wrapper

Don't include a_b syntax with reporting suspicous implicit Times between blanks

Make the scanning for undocumented, obsolete, or experimental System symbols optional

Add LintedLine::truncation message for lines that are too long

Add LintBoxReport

Scan for calls on different lines


### Fixes

Fix problem reported by Francisco with scanning With with bad arguments

Fix OutputForm of LintedLine.


## 0.15.1 - 23 Jan, 2020

Fix the problem of not reporting GroupMissingCloserNode warnings.

Prior to version 12, handling of \|xxxxxx notation was not correct.

Allow lint Report functions to work with default arguments in earlier versions.


## 1.0 - 24 Mar, 2020

Add more cases for warning about implicit Times

handle BeginStaticAnalysisIgnore[] / EndStaticAnalysisIgnore[] ending with ;

It might be possible for 2 lints to have ConfidenceLevel and Severity so that neither shadows the other

Rename Lint -> CodeInspector

Add scan for using Rule here a_ -> a+1 instead of RuleDelayed

Show a ^ for the first character of each AdditionalSource

Add rule scanning for ImageSize -> ImageDimensions[]

Add a basic scan for arg_ :> arg_

Add a check suggested by Szabolcs:
warn about optional argument here: foo[a_, b_:Automatic, OptionsPattern[]]
calling foo[1, "Bar" -> 2] makes "Bar" -> 2 get bound to b, and not to OptionsPattern[]

Put a limit on number of implicit Times x returned


## 1.1 - 30 Sep, 2020

### Scans and suggestions

Scan for UnSameQ, which is very similar to System\`UnsameQ

Catch the case of x_List binding arguments that are intended for OptionsPattern[]

CodeInspectSummarize[
"f[x_List : {}, OptionsPattern[]] := x"
]

Allow code like this to pass:
SyntaxInformation[f] = {"ArgumentsPattern" -> {\_., OptionsPattern[]}}

scan for StringMatch

Add scan for 3 (or more) args to MessageName

Add BackwardsCompaibility issues and tests for bug 390755 changes

Duplicate keys in list of rules in Graph is ok

Suggest inserting * with implicit Times across lines


### API changes

Display other implicit tokens, such as implicit 1, implicit All, and implicit Null

Add "TabWidth" option to CodeInspectSummarize.

This will format tabs in the display correctly. The previous behavior simply formatted tabs as a single space.

Add "TabWidth" option to CodeInspect to be passed through to CodeConcreteParse.

Also remove "LineNumberExclusions" and "LineHashExclusions" options form CodeInspectSummarize. These were not used very much and were interefering with tab work.

Add a definition for ScriptForm format

Add BracketMismatches functionality.

This is similar to the Implicit Tokens functionality. These functions will identify and display cases of mismatched brackets.

Allow lints themselves to be summarized

Since we have an explicit lint that we want to summarize, then make sure that "TagExclusions" and ConfidenceLevel do not interfere with summarizing

Add environ lines, which are lines above and below lints that provide more context

Put in some work to only report first token of potentially large expressions

Rename some tags

Allow opening files in external editor with only a file name, or only file name and line number

Only report the opener of a GroupMissingOpener

The contents can be arbitrarily complex

Handle the new UnterminatedGroupNode

When filtering lints, sort by severity, then sort by Source

add proper InspectedBytesObject

In non-interactive cases, take CodeActions and print their suggestions

Add implicit tokens for ExpectedOperands


### Performance improvements

Use ReadByteArray instead of Import, for speed

Disable shadow filtering for now

The filtering takes quadratic time


### Error handling

Tighten up error reporting when given bad input


### Fixes

Fix implicit tokens in a\ [ThinSpace]b

Fix errors when linting code such as. Module[{a@}, b]

Fix spaces in messages


### Miscellaneous

A massive number of minor bug fixes, typos fixes, and reorganizations, tweaks and cleanup


## 1.1.1 - 8 Dec, 2020

Included in Mathematica 12.2

Fix symbols in wrong context


## 1.2 - 25 Mar, 2021

Add StringTrim to functions to scan for leaking Module variables

Go through and replace all "Did you mean...?" with CodeActions

Add support for Eclipse as an editor

Give much lower confidence to {a -> 1, a-> 2} when it appears in Graph, NetGraph, WordCloud

Add option "AllowedImplicitTokens" to CodeInspectImplicitTokens

This can be a list of special character strings representing the various implicit tokens to allow.

Add OptionPattern to bad symbols

Handle FormatIssues that may come from abstracting

Update BlankPredicate scan with whitelisted symbols

Add rule catching implicit Times in Set:

a = 1; b = 2 c = 3;

forgotten ; leads to parsing as (2 c) = 3

Add "Editor" option to CodeInspect

Add a note about handling Q-functions that take more than 1 arg

It is unclear what to do with them. If \_FreeQ appears, it may still be incorrect code, but the correct code is not \_?FreeQ

Update default lint limit to 100 and wire it in as an option

Add experimental support for disabled regions comment syntax

Example:

(* ::CodeInspect::Push:: \*)
(* ::CodeInspect::Disable::DuplicateClausesIf:: \*)

If[a, b, b]

(* ::CodeInspect::Pop:: \*)

Add a higher confidence to Times appearing in variable declarations (probably a missing comma)

If MessageName has a 3rd argument, then check against recognized languages

Bring in heuristic for PatternBlankOptional

If the pattern name contains "pat", then assume it is used as a pattern

Allow a?(b|c) to be flagged

Parameterize some lints with an "Argument" and allow the disabled regions syntax to take an additional argument that specifies this

Allow inspecting and summarizing of Cells, Notebooks, CellObjects, and NotebookObjects

Add support for suppressed regions to Notebooks

add "InheritedProperties" option to allow CellIndex to be passed down

Use InheritedProperties to allow suppressed regions to work at top-level in notebooks

Fill in in-the-blind suppressed region comment syntax for comment cells

increase $ImplicitTokensLimit to 100


## 1.3 - 30 Aug, 2021

Notes on compatibility have been added to docs/compatibility.md

Fix text for scoping errors
Previously, was saying e.g. "Module variable: foo error"
Now says "Module variable error: foo"

Options are now correctly passed between various inspector and parser functions

Technically, CodeInspector does depend on CodeFormatter, so provide versions checks and update instructions

Add scan for implicit Times pseudo-calls, e.g. Sin(x)

Add the first box-specific rule for having  between GridBox and surrounding parens

Allow MapIndexed[a -> b &, c] to work

Allow Module[{pat}] to not give warning

Add MapThread to recognize a -> b &

Add ReplaceList and StringReplaceList to list of functions for lower confidence of DuplicateKeys

Add Scoping to $DefaultSeverityExclusions

Allow TargetExclusions of "tag" and {"tag", "argument"} syntax


### New CodeInspector UI

CodeInspector UI functionality that powers the new menu item Evaluation > Analyze Notebook

attached cell

notebook toolbar

suppress issues at the cell, notebook, or global level

see suggested changes and apply them

Code Analysis Options palette

Comment syntax for issues suppression

A new comment syntax for selectively disabling linter issues


### Fixes

410408: Code linter treats OptionsPattern[] inconsistently with OptionsPattern[Name]


### Known Issues

12.3 does not have the Evaluation > Code Analysis Options... menu item; this will be addressed in a later paclet update.

413279: Stray Print statement messes up CodeInspector UI

412967: Quote characters appear in analysis pod when using Documentation stylesheet

412965: First::normal messages when closing nb after Analysis

412932: symbol leak and slow down in CodeInspectorUI

412559: Closing notebook during analysis gives messages
