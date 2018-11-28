
# Lint

The Lint tool is **HIGHLY EXPERIMENTAL**.

See the LintExamples.nb notebook for examples of using the Lint tool.


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



