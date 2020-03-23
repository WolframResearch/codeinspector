# Building

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

Specify `MATHEMATICA_INSTALL_DIR` if you have Mathematica installed in a non-default location:
```
cmake -DMATHEMATICA_INSTALL_DIR=/Applications/Mathematica111.app/Contents/ ..
cmake --build . --target paclet
```
