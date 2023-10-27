## FiniteFieldSolve

#### Overview
FiniteFieldSolve is a Mathematica package for exactly solving large linear systems of equations over the rationals.  The package is distributed under GPLv3.

#### Installation
To install FiniteFieldSolve, download and run InstallScript.m.  This script will create the folder `$UserBaseDirectory/Applications/FiniteFieldSolve` and then it will download FiniteFieldSolve.m and RowReduceLink.cpp to this folder.

Running FiniteFieldSolve requires a C++ compiler.  Configuring the compiler depends on the operating system.
- *Windows* is not natively supported but FiniteFieldSolve has been tested under WSL2.
- *Linux* should have g++ installed by default but if it does not then it can be installed with the package manager.
- *Mac* does not ship with a compiler.  clang can be installed by running `xcode-select --install` in a terminal.

FiniteFieldSolve will automatically detect the operating system and defaults to using g++ on Linux and clang on Mac.

OpenMP is disabled by default.  If the compiler is properly configured with OpenMP, then it can be enabled by changing `IsOpenMPInstalled` from `False` to `True` in FiniteFieldSolve.m.

#### Usage

FiniteFieldSolve can be loaded in Mathematica with
```
<<FiniteFieldSolve`
```
like any other package.

The basic syntax for solving a system is `FiniteFieldSolve[{a==b, a==1}]` which will return `{a->1, b->1}`.

More examples can be found in Examples.m.