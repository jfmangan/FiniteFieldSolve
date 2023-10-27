(* ::Package:: *)

dir=FileNameJoin[{$UserBaseDirectory,"Applications","FiniteFieldSolve"}];
Print["FiniteFieldSolve will be installed to ", dir];
CreateDirectory[dir];

(*URL comes from viewing the 'raw' file on github*)
urlMMA="https://raw.githubusercontent.com/jfmangan/FiniteFieldSolve/main/FiniteFieldSolve.m";
urlCPP="https://raw.githubusercontent.com/jfmangan/FiniteFieldSolve/main/RowReduceLink.cpp";

URLDownload[urlMMA,FileNameJoin[{dir,"FiniteFieldSolve.m"}]];
URLDownload[urlCPP,FileNameJoin[{dir,"RowReduceLink.cpp"}]];

If[FindFile["FiniteFieldSolve`"]===$Failed,Print["FiniteFieldSolve.m could not be found"]; Quit[]; ,Print["Found the FiniteFieldSolve.m file"]];

os=$SystemID//ToLowerCase;
Which[StringContainsQ[os,"mac"],
	Print["Mac detected"];
	If[Run["which clang++"]===0, Print["clang++ detected"];, Print["clang++ was not found.  Install clang by running 'xcode-select --install' in a terminal.  For advanced use Homebrew and g++ are recommended."]; Quit[];];
	,
	StringContainsQ[os,"linux"],
	Print["Linux detected"];
	If[Run["which g++"]===0, Print["g++ detected"];, Print["g++ was not found.  Please install g++ with your package manager."]; Quit[];];
	,
	True,
	Print["Operating system not recognized.  Only Linux and Mac are supported."]; Quit[];
];

Check[<<FiniteFieldSolve`;,Print["There was an issue loading the package.  The URLs could be incorrect."]; Quit[];];
If[FiniteFieldSolve[{a==b, b==c}]=!={a->c,b->c},Print["Error running FiniteFieldSolve"]; Quit[];];

Print["It looks like FiniteFieldSolve was installed and works correctly."];
Print["OpenMP is disabled by default but can greatly improve performance.  Properly configuring the compiler is beyond the scope of this installer. If the compiler is configured correctly then OpenMP can be enabled by switching 'IsOpenMPInstalled' from False to True in FiniteFieldSolve.m."];



