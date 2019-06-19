
# Building Lint

Lint uses CMake to generate build scripts.

Here is an example transcript using the default make generator to build Lint:

```
cd lint
mkdir build
cd build
cmake -DWOLFRAMKERNEL=/path/to/WolframKernel ..
cmake --build . --target paclet
```

The result is a directory named paclet that contains the WL package source code and a built Lint .paclet file for installing.

Here is the build directory layout after building Lint:

```
paclet/
  Lint/
    Lint.wl
    PacletInfo.m
    ...
```



