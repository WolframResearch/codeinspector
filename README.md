# CodeInspector

CodeInspector is a package for finding and reporting problems in Wolfram Language code.
CodeInspector has a large collection of rules that can be used to inspect Wolfram Language source code files and can be customized to your preferences. 

In a stand-alone kernel:
```
Needs["CodeInspector`"]

CodeInspectSummarize["If[a,b,b]"]
```
```
Out[2]= If[a,b,b]

        line 1:  If[a,b,b] 
                      ^ ^  
                DuplicateClauses Error Both branches are the same.
```

In a front end:
![CodeInspectSummarize](docs/summarize.png)

[Static Analysis Tools in the Wolfram Language](https://blog.wolfram.com/2021/04/06/static-analysis-tools-in-the-wolfram-language/)

["CodeParser and CodeInspector" on community.wolfram.com](https://community.wolfram.com/groups/-/m/t/1931315)

[Finding Bugs in the Wolfram Language from WTC 2019: Watch Video (youtube)](https://www.youtube.com/watch?v=jMUVwLglt-c)

[Finding Bugs in the Wolfram Language from WTC 2019: Watch Video (wolfram.com)](https://www.wolfram.com/broadcast/video.php?v=2911)

[Finding Bugs in the Wolfram Language from WTC 2019: Download Presentation](https://files.wolframcdn.com/pub/www.wolfram.com/technology-conference/2019/Thursday/2019BrentonBostickFindingBugsInTheWL.nb)


## Setup

CodeInspector and its dependencies are included in Mathematica 12.2 and above.

For older versions, you can install from the paclet server.

The minimum version for CodeInspector is Mathematica 11.0.

CodeInspector depends on [CodeParser](https://github.com/WolframResearch/codeparser) and [CodeFormatter](https://github.com/WolframResearch/codeformatter).

Install CodeInspector and dependencies from the paclet server:
```
PacletInstall["CodeParser"]
PacletInstall["CodeFormatter"]
PacletInstall["CodeInspector"]
```

Make sure that the paclets can be found on your system:
```
Needs["CodeInspector`"]
```


## Using CodeInspector

After CodeParser and CodeInspector are installed, CodeInspector can be used.

Return the list of all problems found in a string of code:
```
Needs["CodeInspector`"]

CodeInspect["If[a,b,b]"]
```
```
Out[2]= {DuplicateClauses Error Both branches are the same.}
```

Summarize the problems found in a source code file:

![Collatz](docs/collatz.png)

The input to `CodeInspect` and `CodeInspectSummarize` may be a string, a `File`, or a list of bytes.
