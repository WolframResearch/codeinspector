Lint docs


## The severity levels

### Remark

Issues such as character encoding assumptions, formatting issues.

A character was saved as extended ASCII, and cannot be treated as UTF8.

Token is not on same line as operand.


### Warning

Most likely unintended uses of functions.


### Error

Recoverable errors in syntax.

Code that is never intentional.

`"\[Alpa]"`

`f[a,]`


### Fatal

Unrecoverable errors in syntax.

`1 + {}}`



So this brings up the interesting problem of how to categorize these problems.

I have settled on 2 dimensions (for now): Severity and ConfidenceLevel

if something is indeed a problem, then Severity should denote how severe it is. Will it ever impact users? Will it accidentally launch nuclear warheads?

ConfidenceLevel is supposed to denote the confidence that this is actually a problem. ConfidenceLevel -> 0.0 means no confidence at all, while ConfidenceLevel -> 1.0 means something like mismatched brackets in a function.

Note that even "obvious" problems don't necessarily have ConfidenceLevel -> 1.0
