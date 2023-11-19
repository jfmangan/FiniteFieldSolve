## FiniteFieldSolve

#### Overview
FiniteFieldSolve is a Mathematica package for exactly solving large linear systems of equations over the rationals.  The package is distributed under GPLv3.  For more information see <https://arxiv.org/abs/2311.01671> by James Mangan.  If you encounter any issues, please feel free to reach out to me.

#### Installation
Running FiniteFieldSolve requires a C++ compiler.  Configuring the compiler depends on the operating system.
- *Windows* is not natively supported but FiniteFieldSolve has been tested under WSL2.
- *Linux* should have g++ installed by default but if it does not then it can be installed with the package manager.
- *Mac* does not ship with a compiler.  clang can be installed by running `xcode-select --install` in a terminal.

FiniteFieldSolve will automatically detect the operating system and defaults to using g++ on Linux and clang on Mac.

After setting up the compiler, FiniteFieldSolve can be installed by downloading and running InstallScript.m.  This script will create the folder `$UserBaseDirectory/Applications/FiniteFieldSolve` and then it will download FiniteFieldSolve.m and RowReduceLink.cpp to this folder.  This completes the basic installation procedure.

OpenMP is disabled by default.  If the compiler is properly configured with OpenMP, then it can be enabled by changing `IsOpenMPInstalled` from `False` to `True` in FiniteFieldSolve.m.

#### Usage

FiniteFieldSolve can be loaded in Mathematica with
```
<<FiniteFieldSolve`
```
like any other package.

The basic syntax for solving a system is `FiniteFieldSolve[{a==b, a==1}]` which will return `{a->1, b->1}`.

More examples can be found in Examples.m.

#### Citation information

If you do happen to use FiniteFieldSolve in your research, please cite it as
```
@article{Mangan:2023eeb,
    author = "Mangan, James",
    title = "{FiniteFieldSolve: Exactly Solving Large Linear Systems in High-Energy Theory}",
    eprint = "2311.01671",
    archivePrefix = "arXiv",
    primaryClass = "hep-th",
    month = "11",
    year = "2023"
}
```
