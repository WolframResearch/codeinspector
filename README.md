# CodeInspector

CodeInspector is a paclet for finding problems in Wolfram Language code.

[Finding Bugs in the Wolfram Language from WTC 2019: Watch Video](https://www.wolfram.com/broadcast/video.php?v=2911)

[Finding Bugs in the Wolfram Language from WTC 2019: Download Presentation](https://files.wolframcdn.com/pub/www.wolfram.com/technology-conference/2019/Thursday/2019BrentonBostickFindingBugsInTheWL.nb)


## Installing

Install CodeInspector and dependencies from the public paclet server:
```
In[1]:= PacletUpdate["CodeParser", "UpdateSites" -> True]
			PacletUpdate["CodeInspector", "UpdateSites" -> True]

Out[1]= Paclet[CodeParser,1.0,<>]
Out[2]= Paclet[CodeInspector,1.0,<>]
```


## Setup

CodeInspector depends on the CodeParser paclet. Make sure that the paclets can be found on your system:
```
In[1]:= Needs["CodeParser`"]
			Needs["CodeInspector`"]
```


## Building

CodeInspector uses a Wolfram Language kernel to build a `.paclet` file.

CodeInspector uses CMake to generate build scripts.

Here is an example transcript using the default make generator to build CodeInspector:
```
cd codeparser
mkdir build
cd build
cmake ..
cmake --build . --target paclet
```

The result is a directory named `paclet` that contains the WL package source code and a built Lint `.paclet` file for installing.

You may see an error because the default path to `WolframKernel` may not be correct.

Here is the cmake command using supplied values for `WOLFRAMKERNEL`:
```
cmake -DWOLFRAMKERNEL=/path/to/WolframKernel ..
```

Here are typical values for the variables:
* `WOLFRAMKERNEL` `/Applications/Mathematica.app/Contents/MacOS/WolframKernel`

Here is the build directory layout after building Lint:

```
paclet/
  Lint/
    Kernel/
      Lint.wl
    PacletInfo.m
    ...
```

### Windows

It is recommended to specify `wolfram.exe` instead of `WolframKernel.exe`.

`WolframKernel.exe` opens a new window while it is running. But `wolfram.exe` runs inside the window that started it.


## The Severity levels

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



