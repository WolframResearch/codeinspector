# CodeInspector

`CodeInspector` is a package for finding and reporting problems in Wolfram Language code.

```
In[1]:= Needs["CodeInspector`"]

In[2]:= CodeInspectSummarize["If[a,b,b]"]

Out[2]= If[a,b,b]

        line 1:  If[a,b,b] 
                      ^ ^  
                DuplicateClauses Error Both branches are the same.

In[3]:= 
```

[Finding Bugs in the Wolfram Language from WTC 2019: Watch Video](https://www.wolfram.com/broadcast/video.php?v=2911)

[Finding Bugs in the Wolfram Language from WTC 2019: Download Presentation](https://files.wolframcdn.com/pub/www.wolfram.com/technology-conference/2019/Thursday/2019BrentonBostickFindingBugsInTheWL.nb)


## Setup

CodeInspector depends on the CodeParser paclet. Make sure that the paclets can be found on your system:
```
In[1]:= Needs["CodeParser`"]
      Needs["CodeInspector`"]
```

[CodeParser on github.com](https://github.com/xxx)
[CodeInspector on github.com](https://github.com/xxx)

Install CodeInspector and dependencies from the CodeTools paclet server:
```
In[1]:= PacletUpdate["CodeParser", "Site" -> "XXX", "UpdateSites" -> True]
			PacletUpdate["CodeInspector", "Site" -> "XXX", "UpdateSites" -> True]

Out[1]= PacletObject[CodeParser, 1.0, <>]
Out[2]= PacletObject[CodeInspector, 1.0, <>]
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

The result is a directory named `paclet` that contains the WL package source code and a built CodeInspector `.paclet` file for installing.

Specify `INSTALLATION_DIRECTORY` if you have Mathematica installed in a non-default location:
```
cmake -DINSTALLATION_DIRECTORY=/Applications/Mathematica111.app/Contents/ ..
cmake --build . --target paclet
```
