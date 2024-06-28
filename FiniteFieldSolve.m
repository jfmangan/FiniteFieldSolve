(* ::Package:: *)

BeginPackage["FiniteFieldSolve`"]


FiniteFieldSolve::usage = "FiniteFieldSolve[equations, (optional: OptionsList)] solves a List of linear equations.  FiniteFieldSolve will automatically detect the variables in the equations.  The equations can contain '==' but if they do not contain '==' then it is assumed that the equation is equal to zero.  Note that 'equations' will likely take up more memory than the SparseArray representing them, so it is recommended to directly call FiniteFieldSolveMatrix for advanced usage.  OptionsList is an optional parameter taking the form {string1, string2...}.  Including 'verbose' in OptionsList will print more output.  By default FiniteFieldSolve will use the first prime it solves over to determine the linearly independent rows and only use those rows when solving over subsequent primes.  To disable this behavior include 'KeepLinearDepRows' in OptionsList.  By default FiniteFieldSolve will use the first prime to detect if solving the equations sets any variables to zero.  These variables will not be solved for in the future and the ensuing linearly dependent rows will be removed by row reducing over the second prime.  To disable this behavior include 'KeepZeroVariables' in OptionsList.  Row reduction will default to 16-bit primes which are generally faster and use less memory.  To use 32-bit primes include '32bit' in OptionsList.  Add 'ClearDenominators' to OptionsList in order to clear all of the denominators in the equations.  This is especially useful for dense equations generated numerically.  By default the equations are sorted by their density (after calling CoefficientArrrays on 'equations') so that more complicated linearly dependent rows are discarded.  To disable this feature include 'NoRowSorting' in OptionsList.  By default, the function statically allocates a contiguous block of memory for the matrix to improve performance.  This may not be possible for sufficiently large matrices, in which case you can switch to dynamically allocating the matrix by adding 'dynamic' to OptionsList.";

FiniteFieldSolveMatrix::usage = "FiniteFieldSolveMatrix[matrix, vars, homogeneous, (optional: OptionsList)] solves the linear system 'matrix' over the variables 'vars'.  The matrix does not need to be sparse or a SparseArray.  If homogeneous is False then the last element of vars is the variable used to represent 1 so you are always effectively solving the homogeneous system Ax=0.  OptionsList is an optional parameter taking the form {string1, string2...}.  By default FiniteFieldSolveMatrix will use the first prime it solves over to determine the linearly independent rows and only use those rows when solving over subsequent primes.  To disable this behavior include 'KeepLinearDepRows' in OptionsList.  By default FiniteFieldSolve will use the first prime to detect if solving the equations sets any variables to zero.  These variables will not be solved for in the future and the ensuing linearly dependent rows will be removed by row reducing over the second prime.  To disable this behavior include 'KeepZeroVariables' in OptionsList.  Row reduction will default to 16-bit primes which are generally faster and use less memory.  To use 32-bit primes include '32bit' in OptionsList.  By default if 'matrix' is a SparseArray then its rows will be sorted by their density so that more complicated linearly dependent rows are discarded.  To disable this feature include 'NoRowSorting' in OptionsList.  For memory reasons, 'ClearDenominators' is not an option for FiniteFieldSolveMatrix.  By default, the function statically allocates a contiguous block of memory for the matrix to improve performance.  This may not be possible for sufficiently large matrices, in which case you can switch to dynamically allocating the matrix by adding 'dynamic' to OptionsList.";

RowReduceOverPrime::usage = "RowReduceOverPrime[matrix, prime, (optional: StaticOrDynamicMem), (optional: RowsToUse), (optional: ColsToUse)] row reduces 'matrix' (not necessarily sparse or a SparseArray) over the prime.  The prime must be less than 2^32 where primes less than 2^16 will be faster and consume less memory.  In the output, rows are not rearranged to be in strict RREF form.  Futhermore, rows of zeros are not deleted so it is easy to determine which rows of the initial matrix are linearly independent.  The function will return $Failed if the matrix could not be projected over the given prime.  RowsToUse is an optional argument that defaults to All.  If you only want to use certain rows in the matrix then set RowsToUse to, for example, {1,4,5...}.  ColsToUse is the analog of RowsToUse but for columns.  StaticOrDynamicMem controls how the memory for the matrix is allocated.  StaticOrDynamicMem defaults to 'static' where the matrix is statically allocated as a contiguous block of memory to improve speed.  For sufficiently large matrices there may not be a contiguous block of memory available so you may need to dynamically allocate the matrix by setting StaticOrDynamicMem to 'dynamic'.";

FindLinearlyIndependentRows::usage = "FindLinearlyIndependentRows[InputMatrix, (optional: prime), (optional: OptionsList)] returns a matrix made from the linearly independent rows of InputMatrix.  Running this function is considerably faster than solving the whole system so it may be valuable to run this function before solving an over-constrained system.  By default if 'InputMatrix' is a SparseArray then its rows will be sorted by their density so that more complicated linearly dependent rows are discarded.  To disable this feature include 'NoRowSorting' in OptionsList.  By default, the function statically allocates a contiguous block of memory for the matrix to improve performance.  This may not be possible for sufficiently large matrices, in which case you can switch to dynamically allocating the matrix by adding 'dynamic' to OptionsList.";

FindLinearlyIndependentEquations::usage = "FindLinearlyIndependentEquations[InputEquations, (optional: prime), (optional: OptionsList)] returns the linearly independent InputEquations.  Running this function is considerably faster than solving the whole system so it may be valuable to run this function before solving an over-constrained system.  By default the rows of InputEquations are sorted by their density so that more complicated linearly dependent equations are discarded.  To disable this feature include 'NoRowSorting' in OptionsList.  By default, the function statically allocates a contiguous block of memory for the matrix to improve performance.  This may not be possible for sufficiently large matrices, in which case you can switch to dynamically allocating the matrix by adding 'dynamic' to OptionsList.";

ConsistentMatrixQ::usage = "ConsistentMatrixQ[matrix, (optional: prime), (optional: OptionsList)] takes a matrix representing an inhomogeneous system of equations and returns False if the system is inconsistent and True otherwise.  As with FiniteFieldSolve, the last column of the matrix is assumed to represent the inhomogeneous terms.  Running this function may be considerably faster than solving the whole system so it may be valuable to run this function before solving an inhomogeneous system especially if an inconsistent solution is likely.  By default, the function statically allocates a contiguous block of memory for the matrix to improve performance.  This may not be possible for sufficiently large matrices, in which case you can switch to dynamically allocating the matrix by adding 'dynamic' to OptionsList.";

ConsistentEquationsQ::usage = "ConsistentEquationsQ[equations, (optional: prime), (optional: OptionsList)] takes an inhomogeneous system of equations and returns False if the system is inconsistent and True otherwise.  Running this function may be considerably faster than solving the whole system so it may be valuable to run this function before solving an inhomogeneous system especially if an inconsistent solution is likely.  By default, the function statically allocates a contiguous block of memory for the matrix to improve performance.  This may not be possible for sufficiently large matrices, in which case you can switch to dynamically allocating the matrix by adding 'dynamic' to OptionsList.";


Begin["`Private`"]


Reconstruct[0, _]:=0;
Reconstruct[a_, n_]:=With[{v=LatticeReduce[{{a, 1}, {n, 0}}][[1]]}, v[[1]]/v[[2]]];
(*From Ben Page https://youtu.be/3cGyeN1a7bs?t=2806*)


ChineseRemainderMats[{matrices__SparseArray}, primes_List]:=
With[{NonZeroPos = DeleteDuplicates[Flatten[#["NonzeroPositions"]&/@{matrices}, 1]]},
    SparseArray[Table[pos->ChineseRemainder[#[[Sequence@@pos]]&/@{matrices}, primes],{pos, NonZeroPos}],Dimensions[{matrices}[[1]]]]
];


RationalToInt[0,p_Integer]:=0;
RationalToInt[a_Rational,p_Integer]:=Mod[Numerator[a]*ModularInverse[Denominator[a],p],p];
RationalToInt[a_Integer,p_Integer]:=Mod[a,p];


PackageDir=$InputFileName//DirectoryName;(*Get the package's directory.  $InputeFileName is only available when you initialize the package so you have to store it as a private variable.*)

CompileLibraryLink[NumRow_Integer, NumCol_Integer, prime_Integer, StaticOrDynamicMem_:"static", FullSolveOrOnlyFindPivots_:"FullSolve"]:=
Block[{LibraryLinkFullPath, gccString, PerformanceString, HeadersString, MatrixOptionsString, SourceString, oString, TargetString, CommandString, IsOpenMPInstalled, OpenmpString, message, SharedLibString, SharedLibExtension, os, uIntShort, uIntLong, ProcType, MarchOrMcpu, BytesPerInt, FreeMem, MatMem, MemCutoff, MallocString, StaticallyAllocateMem, OnlyFindPivotsString},

	If[!(prime<2^32&&PrimeQ[prime]),Print[prime, " needs to be prime and less than 2^32"];Abort[];];
	
	If[prime<2^16,
		uIntShort="uint16_t";
		uIntLong="uint32_t";
		,
		uIntShort="uint32_t";
		uIntLong="uint64_t";
	];
	
	If[prime<2^16,
		BytesPerInt=2;
		,
		BytesPerInt=4;
	];
	
	Which[ToLowerCase[StaticOrDynamicMem]==="static",
		StaticallyAllocateMem=True,
		ToLowerCase[StaticOrDynamicMem]==="dynamic",
		StaticallyAllocateMem=False,
		True,
		Print["'StaticOrDynamicMem' needs to be 'static' or 'dynamic'"];
		Abort[];
	];
	
	FreeMem=MemoryAvailable[]/1024^3//N;
	MatMem=NumRow*NumCol*BytesPerInt/1024^3//N;
	MemCutoff=1/3//N;
	If[MatMem>MemCutoff*FreeMem,
		Print["Potentially low memory."];
		Print["Free memory ",FreeMem//NumberForm[#,3]&//ToString," GB.  Memory needed for matrix ",MatMem//NumberForm[#,3]&//ToString," GB."];
		If[StaticallyAllocateMem===True,
			Print["If there is erratic behavior or crashes try adding 'dynamic' to OptionsList."];,
			Print["There may be erratic behavior or crashes."];
		];
	];

	(*Build the Library Link shared library.*)
	(*THE WHITESPACE IN THE STRINGS IS VERY IMPORTANT!  The compiler options may need to be changed for different computers.*)
	LibraryLinkFullPath=StringJoin[PackageDir,"RowReduceLink"];
	
	os=$SystemID//ToLowerCase;
	Which[StringContainsQ[os,"mac"],
		(*mac options*)
		gccString = "clang++ ";(*"/usr/local/bin/g++-12 ";*)
		SharedLibString="-dynamiclib ";
		SharedLibExtension=".dylib";
		,
		StringContainsQ[os,"linux"],
		(*linux options*)
		gccString= "g++ ";
		SharedLibString="-fpic -shared ";
		SharedLibExtension=".so";
		,
		True,
		Print["Operating system not recognized.  Only Linux and Mac are supported."]; Abort[];
	];
	
	IsOpenMPInstalled=False;(*True;*)
	If[IsOpenMPInstalled, OpenmpString="-fopenmp ", OpenmpString=" "];
	
	ProcType=$ProcessorType//ToLowerCase;
	Which[StringContainsQ[ProcType,"x86"],
		MarchOrMcpu="-march=native ";
		,
		StringContainsQ[ProcType,"arm"],
		MarchOrMcpu="-mcpu=native ";
		,
		True,
		MarchOrMcpu=" ";
	];
	PerformanceString = StringJoin[MarchOrMcpu,"-O3 "];
	
	If[StaticallyAllocateMem,
		MallocString=" -D ALLOC_STATIC_MEM=true ",
		MallocString=" -D ALLOC_STATIC_MEM=false "
	];
	
	Which[ToLowerCase[FullSolveOrOnlyFindPivots]===ToLowerCase["FullSolve"],
		OnlyFindPivotsString=" -D ONLY_FIND_PIVOTS=false ",
		ToLowerCase[FullSolveOrOnlyFindPivots]===ToLowerCase["OnlyFindPivots"],
		OnlyFindPivotsString=" -D ONLY_FIND_PIVOTS=true ",
		True,
		Print["'FullSolveOrOnlyFindPivots' needs to be 'FullSolve' or 'OnlyFindPivots'"];
		Abort[];
	];

	HeadersString = StringJoin["-I ", $InstallationDirectory, "/SystemFiles/IncludeFiles/C"];
	MatrixOptionsString = StringJoin[" -D PRIME=", prime // ToString, " -D NUM_ROW=", NumRow//ToString, " -D NUM_COL=", NumCol//ToString, " -D U_INT_SHORT=", uIntShort, " -D U_INT_LONG=", uIntLong, " "];
	SourceString = StringJoin[LibraryLinkFullPath, ".cpp"];
	oString = " -o ";
	TargetString = StringJoin[LibraryLinkFullPath, SharedLibExtension];
	CommandString = StringJoin[gccString, PerformanceString, SharedLibString, OpenmpString, HeadersString, MatrixOptionsString, MallocString, OnlyFindPivotsString, SourceString, oString, TargetString];
	message=CommandString // Run;
	If[message=!=0,
		Print["Error compiling the shared library!  Try running the following command in a terminal for more information."];
		Print[CommandString];
		If[StringContainsQ[ProcType,"arm"],
			Print["Different compilers have different issues with ARM processors so it may be worth disabling '-mcpu=native ' or trying '-march=native ' instead."];
		];
		Abort[];
	];

];


RowReduceOverPrime[CoefArr_,prime_Integer,StaticOrDynamicMem_:"static",RowsToUse_:All,ColsToUse_:All]:=RowReduceOverPrimeHelper[CoefArr,prime,StaticOrDynamicMem,RowsToUse,ColsToUse,"FullSolve"];


RowReduceOverPrimeHelper[CoefArr_,prime_Integer,StaticOrDynamicMem_:"static",RowsToUse_:All,ColsToUse_:All, FullSolveOrOnlyFindPivots_:"FullSolve"]:=
Block[{i, row, mat, RowReduceNumericArray, PopulateRowOfMatrix, LibraryLinkFullPath, RowRange, ColRange, uIntType},
	
	If[RowsToUse===All,RowRange=CoefArr//Dimensions//First//Range,RowRange=RowsToUse];
	If[ColsToUse===All,ColRange=CoefArr//Dimensions//Last//Range,ColRange=ColsToUse];
	
	CompileLibraryLink[RowRange//Length, ColRange//Length, prime, StaticOrDynamicMem, FullSolveOrOnlyFindPivots];
	
	(*Print["Dimensions: ", RowRange//Length, ", ", ColRange//Length];*)(*Helpful for debugging*)
	
	LibraryLinkFullPath=StringJoin[PackageDir,"RowReduceLink"];
	
	(*This function loads one row of data into the matrix stored in the LibraryLink shared library.*)
	PopulateRowOfMatrix=LibraryFunctionLoad[LibraryLinkFullPath, "PopulateRowOfMatrix", {{LibraryDataType[NumericArray], "Constant"},{LibraryDataType[Integer],"Constant"}}, {Integer}];(*I had issues when I marked any of these as Constant even though I think they are.*)
	
	If[prime<2^16,uIntType="UnsignedInteger16",uIntType="UnsignedInteger32"];
	
	(*The following For loop represents a non-trivial portion of the solve time*)
	(*I think passing as a MNumericArray is a tiny bit slower than passing as a MTensor but the difference is very small.*)
	For[i=1,i<=Length[RowRange],i++,
		row=Quiet[Check[CoefArr[[RowRange[[i]]]]//RationalToInt[#,prime]&/@#&//NumericArray[#,uIntType]&//#[[ColRange]]&,Return[$Failed]]];
		(*It is fastest to section the row after it has been changed from a SparseArray into a NumericArray, that is, put [[ColRange]] last.  It's really dumb that it's faster to do RationalToInt on data you won't use rather than sectioning the array first.  This is one of the consequences of not making an internal copy of the input CoefArr.*)
		PopulateRowOfMatrix[row,i];
	];
	(*If CoefArr is a SparseArray with low density then it is a little more effecient to do...*)
	(*row=Quiet[Check[CoefArr[[RowRange[[i]]]]//ArrayRules//MapAt[RationalToInt[#,prime]&,#,-1]&/@#&//SparseArray[#,CoefArr//Dimensions//Last]&//NumericArray[#,uIntType]&,Return[$Failed]]];*)
	
	RowReduceNumericArray=LibraryFunctionLoad[LibraryLinkFullPath, "RowReduceNumericArray", {}, {LibraryDataType[SparseArray]}];
	
	(*Do the row reduction*)
	mat=RowReduceNumericArray[];
	
	LibraryUnload[LibraryLinkFullPath];
	(*The memory useage looks like this:  There is the original copy of the matrix called CoefArr in arbitrary precision rationals.  The Library Link function has a 16-bit (or 32-bit) copy of this matrix.  You pass the matrix one row at a time because if you pass the matrix by pointer then the row reduction is slower.  When mat is row reduced it is turned into a new SparseArray of (hopefully) negligible memory.  So the memory you need to solve CoefArr should be the memory needed to store CoefArr plus the size of mat.  The memory will grow slightly over time as you build up the solution using more primes.*)
	
	(*mat may contain rows of zeros (representing linearly dependent rows).  To delete them you'd have to do...*)
	(*mat["NonzeroPositions"]//First/@#&//Union//mat[[#]]&*)
	
	mat
];


rrefToRules[rref_SparseArray,vars_List,OneAlias_]:=
Block[{VarArray, VarSol},
	Table[
	VarArray = (Sort[ArrayRules[SparseRow][[;;-2]]]/.Rule[{b_},x_]:>x*vars[[b]]);
	If[Length[VarArray]===0,
		VarSol = VarArray[[1]]->0;
		,
		VarSol = First[VarArray]->-Total[Rest[VarArray]];
	];
	If[VarSol===(OneAlias->0),
		Return[$Failed,Block];
	];
	VarSol
	
	,{SparseRow,rref}]
];


MemType[OptionsList_]:=If[MemberQ[OptionsList//ToLowerCase,"dynamic"//ToLowerCase],
		"dynamic",
		"static"
];


FiniteFieldSolveMatrix[CoefArr_,vars_List,homogeneous_,OptionsList_List:{}]:=
Block[{attempts, SolRules, VerbosePrint, OneAlias, PrimeList, UsedPrimes, projection, reconstruction, PrimeToUse, NewProjection, NewConstruction, LinearIndepRows, RowsToUse, SortMatIntoStrictRREFForm, RemoveLinearlyDependentRows, ColumnsOfZeroVars, ZeroRules, ColsToUse, RemoveVariablesSetToZero, TmpTime, IndepVars, IndepVarsRep, NullVec, FoundSolution, PrintModErr, IssuedWarning},
	
	(*Basic tests on input data*)
	If[BooleanQ[homogeneous]==False,Print[homogeneous, " needs to be True or False"];Abort[]];
	If[Not[Or[Head[CoefArr]==SparseArray,Head[CoefArr]==List]],Print["The Head of the input matrix needs to be List or SparseArray"];Abort[]];
	If[(CoefArr//Dimensions//Last)=!=Length[vars],Print["The number of variables does not match the number of columns in the matrix"];Abort[]];
	
	RemoveLinearlyDependentRows=MemberQ[OptionsList//ToLowerCase,"KeepLinearDepRows"//ToLowerCase]//Not;
	RemoveVariablesSetToZero=MemberQ[OptionsList//ToLowerCase,"KeepZeroVariables"//ToLowerCase]//Not;
	
	(*If the system is inhomogeneous, make OneAlias the last element of vars*)
	If[homogeneous,OneAlias=Nothing,OneAlias=vars//Last];
	
	If[MemberQ[OptionsList//ToLowerCase,"verbose"],VerbosePrint[str___]:=Print[str]];
	IssuedWarning=False;
	PrintModErr:=If[!IssuedWarning,IssuedWarning=True; Print["Bad projection!  Have you tried clearing the denominators of the system of equations by adding 'ClearDenominators' to the OptionsList of FiniteFieldSolve?  Further instances of this warning will be suppressed."];];

	(*If you remove rows or columns from the matrix then when you solve the matrix again over a different prime, you might end up with an row reduced matrix with the rows in a different order.  That is, the linearly dependent rows might have affected the order in which you solve the rows.  So if you remove the linearly dependent rows you need to canonicalize the ordering of the rows.  This is only for reconstructing the actual solution so only use this right before reconstructing something.*)
	SortMatIntoStrictRREFForm[mat_]:=
	If[Or[RemoveLinearlyDependentRows,RemoveVariablesSetToZero],
		Block[{canonicalRowOrdering},
			(*Sort the rows into RREF form*)
			canonicalRowOrdering=mat["NonzeroPositions"]//GatherBy[#,First]&//SortBy[#,Last]&/@#&//First/@#&//SortBy[#,Last]&//First/@#&;
			mat[[canonicalRowOrdering]]
			]
		,
		mat
	];
	
	If[MemberQ[OptionsList//ToLowerCase,"32bit"//ToLowerCase],
		PrimeList = Range[203280221,203280221-500,-1]//Prime/@#&(*The largest prime needs to fit in 32 bits*)
		,
		PrimeList = Range[6542,6542-500,-1]//Prime/@#&;(*The largest prime needs to fit in 16 bits*)
	];
	
	UsedPrimes = {};
	attempts = 1;
	projection=$Failed;
	
	(*If requested, do not sort the rows of CoefArr by their density.*)
	If[And[MemberQ[OptionsList//ToLowerCase,"NoRowSorting"//ToLowerCase]//Not,Head[CoefArr]===SparseArray],
		RowsToUse=CoefArr//#["Density"]&/@#&//Ordering;
		(*You could also sort the rows so that a row with its first non-zero entry very far right goes above other rows.  The rows that are high up don't need to update as many positions in the rows below them.  You could also sort rows by some combination of their density and the position of their first non-zero entry.*)
		(*RowsToUse=CoefArr//#["NonzeroPositions"]&/@#&//First/@#&//Flatten//-1*#&/@#&//Ordering;(*Assumes that SparseArray positions are already sorted*)*)
		,
		RowsToUse=CoefArr//Length//Range;
		(*The reason you don't set RowsToUse to All is that you might omit certain rows later if RemoveVariablesSetToZero is True*)
	];
	
	(*Build initial solution*)
	(*For the first prime, check if the matrix is inconsistent, select the linearly independent rows, and omit columns that only serve to set variables to zero.*)
	
	While[projection===$Failed,
		VerbosePrint["Matrix dimensions: ", CoefArr//Dimensions];
		TmpTime=AbsoluteTime[];
		projection = RowReduceOverPrime[CoefArr,PrimeList[[attempts]],MemType[OptionsList],RowsToUse];(*projection may contain rows of zeros representing linearly dependent rows*)
		(*All rows of zeros are included in projection right now so projection is just a pure rref*)
		VerbosePrint["Time (sec) used to row reduce: ", AbsoluteTime[]-TmpTime];
		If[projection===$Failed,PrintModErr;];		
		UsedPrimes = {PrimeList[[attempts]]};
		attempts++;
	];
	
	(*Test if you were handed the zero matrix.  This will erroneously abort if you were handed a matrix of only 65521's (or multiples of whatever prime you are reducing over).*)
	If[projection["NonzeroPositions"]==={},Print["Zero input matrix detected."];Return[{}]];
	
	(*Check to see if the matrix is inconsistent and abort if it is.  There's no need to fully solve the system if you can quickly find out that it's inconsistent.*)
	If[homogeneous===False,
		(*The if statements are nested for performance reasons.  You don't want to call ConsistentMatrixQHelper unnecessarily.*)
		If[ConsistentMatrixQHelper[projection]===False,
			Print["Inconsistent solution"];
			Abort[];
		];
	];
	
	(*Find the linearly independent rows, i.e., the nonzero rows of the rref.
	Remove the zero rows from projection.  If solving with RemoveLinearlyDependentRows===True, remove the linearly dependent rows from CoefArr*)
	LinearIndepRows=projection["NonzeroPositions"]//First/@#&//Union;
	projection=projection[[LinearIndepRows]];
	
	(*Unfortunately you can't just set coefArrNew = CoefArr[[LinearIndepRows]] because then MMA will make a new copy of the matrix.
	You could avoid making a copy of the matrix by doing SetAttributes[FiniteFieldSolve,HoldFirst] but then you would overwirte the input matrix.
	The best solution I can think of is to do a bunch of gymnastics where you only pass in certain rows to row reduce.*)
	If[RemoveLinearlyDependentRows, RowsToUse=RowsToUse[[LinearIndepRows]]];
	
	(*Find the columns that just set variables to zero and remove them from future row reductions.*)
	(*As an example of why the RREF tells you which columns to remove but not which rows, consider the matrix {{1,1,x},{1,0,0}}.*)
	If[RemoveVariablesSetToZero,
		ColumnsOfZeroVars=projection["NonzeroPositions"]//GatherBy[#,First]&//Select[#,Length[#]==1&]&//Flatten/@#&//Last/@#&;
		ZeroRules=ColumnsOfZeroVars//vars[[#]]&/@#&//#->0&/@#&;
		ColsToUse=(projection//Dimensions//Last//Range)~Complement~ColumnsOfZeroVars;(*So this is just the complement of ColumnsOfZeroVars*)
		If[ColsToUse==={},Return[ZeroRules]];
		,
		ZeroRules={};
		ColsToUse=All;
	];
	
	projection=projection//#[[All,ColsToUse]]&//SortMatIntoStrictRREFForm;

	(*After removing variables that are set to zero, the matrix could be empty, so you return early.  (Proceeding with normal execution will produce many errors.)  You could mistakenly return early here if your matrix involves a bunch of variables that get set to zero and all of the other entries in the matrix are multiples of 65521 (or another prime) that 'projection' interprets as 0.*)
	If[projection==={},Return[ZeroRules]];(*If projection is an empty SparseArray (after the zero vars are removed) then SortMatIntoStrictRREFForm will turn projection into {}.*)
	
	reconstruction = SparseArray[projection["NonzeroPositions"]->(Reconstruct[#,UsedPrimes[[1]]]&/@projection["NonzeroValues"]),Dimensions[projection]];
	VerbosePrint["Prime used: ",UsedPrimes[[1]]];
	
	While[attempts<=Length[PrimeList],
	
		VerbosePrint["Working on prime number: ", Length[UsedPrimes]+1];
		
		If[And[Length[UsedPrimes]==2,RemoveVariablesSetToZero,RemoveLinearlyDependentRows],
			(*You found the zero variables with the first prime. With the second prime you found the newly linearly dependent variables. Now if you're on the third prime you can omit those rows using the LinearIndepRows from solving over the 2nd prime.  This is all more obvious if you print the dimenions of the matrix with each prime.*)
			RowsToUse=RowsToUse[[LinearIndepRows]];
			(*I thought you'd have to go back and modify the RREFs and projections for previous primes but that doesn't seem to be the case since they're in strict RREF form.*)
		];
		
		NewProjection = $Failed;
		While[NewProjection===$Failed,
			PrimeToUse = PrimeList[[attempts]];
			TmpTime=AbsoluteTime[];
			
			SolRules = rrefToRules[reconstruction,vars[[ColsToUse]],OneAlias];
			If[SolRules===$Failed,
				Print["Inconsistent solution"];
				Return[{OneAlias->0}]
			];
			
			IndepVars=SolRules//Last/@#&//Union//Variables;
			IndepVarsRep=IndepVars//MapThread[Rule,{#,RandomInteger[{0,PrimeList[[attempts]]-1},Length[#]]}]&//Dispatch;
			Quiet[Check[NullVec=vars[[ColsToUse]]/.IndepVarsRep/.Dispatch[SolRules/.IndepVarsRep]//RationalToInt[#,PrimeList[[attempts]]]&/@#&;,PrintModErr; attempts++; Continue[];]];
			(*The following matrix-vector multiplication is surprisingly fast.*)
			Quiet[Check[FoundSolution=CoefArr[[RowsToUse,ColsToUse]] . NullVec//RationalToInt[#,PrimeList[[attempts]]]&/@#&//DeleteCases[0]//#==={}&;,PrintModErr; attempts++; Continue[];]];
			(*FoundSolution will be true if the random null vector really is a null vector, meaning you've found a solution*)
			If[FoundSolution,
				Return[Join[ZeroRules,SolRules]];,
				
				VerbosePrint["Matrix dimensions: ", {Length[RowsToUse],Length[ColsToUse]}];
				NewProjection = RowReduceOverPrime[CoefArr,PrimeList[[attempts]],MemType[OptionsList],RowsToUse,ColsToUse];
				VerbosePrint["Time (sec) used to row reduce: ", AbsoluteTime[]-TmpTime];
				If[NewProjection===$Failed,PrintModErr;];
				attempts++;
			];
		];
		VerbosePrint["Prime used: ",PrimeToUse];
		
		LinearIndepRows=NewProjection["NonzeroPositions"]//First/@#&//Union;
		NewProjection=NewProjection[[LinearIndepRows]];
		NewProjection = NewProjection//SortMatIntoStrictRREFForm;
		NewProjection = ChineseRemainderMats[{projection,NewProjection},{Times@@UsedPrimes,PrimeToUse}];

		AppendTo[UsedPrimes,PrimeToUse];
		
		NewConstruction = SparseArray[NewProjection["NonzeroPositions"]->(Reconstruct[#,Times@@UsedPrimes]&/@NewProjection["NonzeroValues"]),Dimensions[NewProjection]];
									
		(*------------*)
		
		(*Identifying the extraneous columns of independent variables will help if you have lots of free vars in your system*)
		
		(*
		PivotCols=reconstruction["NonzeroPositions"]//GatherBy[#,First]&//First/@#&//Last/@#&;
		IndepVarCols=Complement[reconstruction//Dimensions//Last//Range,PivotCols];
		cols1=reconstruction//#[[IndepVarCols]]&/@#&//SparseArray;
		cols2=NewConstruction//#[[IndepVarCols]]&/@#&//SparseArray;
		cols1-cols2//SparseArray//#["NonzeroPositions"]&//Last/@#&//Union//Length//Print;
		*)
		
		(*------------*)
		
		projection = NewProjection;
		reconstruction = NewConstruction;
	
	];
	
	Return[$Failed];(*This only happens if you exit the While loop above which means you ran out of primes.*)
];


LCMHelper[RowSparseArr_]:=If[RowSparseArr["NonzeroValues"]==={},
	RowSparseArr,
	RowSparseArr["NonzeroValues"]//Denominator/@#&//LCM@@#&//#*RowSparseArr&
];


one;(*This Head is the variable that will be used to represent 1.  one is in the Private scope (Context) of this package.*)


EquationsToMatrixAndVars[equations_List]:=
Block[{vars, matrix, homogeneous, CoefArrs},

	vars=equations/.x_==y_:>x-y//Variables;
	CoefArrs=CoefficientArrays[equations,vars];
	If[Length[CoefArrs]=!=2, Print["The input equations are not linear."];Abort[];];
	homogeneous=CoefArrs//First//#["NonzeroPositions"]&//#==={}&;

	If[homogeneous,
		matrix=CoefArrs[[2]];,
		matrix=MapThread[Append,{CoefArrs[[2]],CoefArrs[[1]]}]//SparseArray;
		AppendTo[vars,one];
	];
	Clear[CoefArrs];

	{matrix,vars}
];


FiniteFieldSolve[equations_List, OptionsList_List:{}]:=
Block[{vars, matrix, homogeneous, CoefArrs, sol},

	{matrix,vars}=EquationsToMatrixAndVars[equations];
	homogeneous=vars//Last//#===one&//Not;

	If[MemberQ[OptionsList//ToLowerCase,"ClearDenominators"//ToLowerCase], matrix=matrix//LCMHelper/@#&//SparseArray];
	(*I don't have the option to clear denominators in FiniteFieldSolveMatrix because I don't want to incur an unnecessary memory hit.*)

	sol=FiniteFieldSolveMatrix[matrix,vars,homogeneous,OptionsList];
	If[homogeneous,sol,sol/.one->1]
];


FindLinearlyIndependentRowsHelper[mat_, prime_Integer:65521, OptionsList_List:{}]:=
Block[{rref,NonZeroRows,RowsToUse},

	(*If requested, do not sort the rows of CoefArr by their density.*)
	If[And[MemberQ[OptionsList//ToLowerCase,"NoRowSorting"//ToLowerCase]//Not,Head[mat]===SparseArray],
		RowsToUse=mat//#["Density"]&/@#&//Ordering;
		,
		RowsToUse=mat//Length//Range;
	];
	
	rref=RowReduceOverPrimeHelper[mat,prime,MemType[OptionsList],RowsToUse,All,"OnlyFindPivots"];
	If[rref===$Failed,Return[$Failed]];
	NonZeroRows=rref["NonzeroPositions"]//First/@#&//Sort;
	RowsToUse[[NonZeroRows]]
];


FindLinearlyIndependentRows[mat_, prime_Integer:65521, OptionsList_List:{}]:=
Block[{rows},

	rows=FindLinearlyIndependentRowsHelper[mat, prime, OptionsList];
	If[rows===$Failed,$Failed,mat[[rows]]]
];


FindLinearlyIndependentEquations[equations_List, prime_Integer:65521, OptionsList_List:{}]:=
Block[{mat,vars,rows},

	{mat,vars}=EquationsToMatrixAndVars[equations];
	rows=FindLinearlyIndependentRowsHelper[mat, prime, OptionsList];
	If[rows===$Failed,$Failed,equations[[rows]]]
];


(*ConsistentMatrixQHelper[rref_]:=
Block[{NonZeroPos,ProblematicRows,NumberOfVariables},

	(*You have an inconsistent matrix if the last variable (which is supposed to represent 1) is ever set to zero, that is, if you ever have a row that involves the last variable where the number of non-zero entries in the row is 1.*)
	NumberOfVariables=rref//Dimensions//Last;
	NonZeroPos=rref["NonzeroPositions"];
	ProblematicRows=NonZeroPos//Select[#,MatchQ[#,{_,NumberOfVariables}]&]&//First/@#&;
	rref[[ProblematicRows]]//#["NonzeroPositions"]&/@#&//Length/@#&//MemberQ[#,1]&//Not(*This assumes that SparseArray correctly removes all zeros from "NonzeroPositions"*)
];*)


ConsistentMatrixQHelper[rref_]:=
Block[{NumberOfVariables,RowColTuple},

	(*The matrix is inconsistent if the last column is a pivot*)
	NumberOfVariables=rref//Dimensions//Last;
	RowColTuple=rref["NonzeroPositions"]//SelectFirst[#,MatchQ[#,{_,NumberOfVariables}]&]&;
	If[RowColTuple===Missing["NotFound"], Return[True]];
	rref[[RowColTuple//First]]["NonzeroPositions"]//Length[#]>1&
];


ConsistentMatrixQ[CoefficientMatrix_, prime_Integer:65521, OptionsList_List:{}]:=
Block[{rref},

	rref=RowReduceOverPrimeHelper[CoefficientMatrix,prime,MemType[OptionsList],All,All,"OnlyFindPivots"];
	ConsistentMatrixQHelper[rref]
];


ConsistentEquationsQ[equations_List, prime_Integer:65521, OptionsList_List:{}]:=
Block[{mat,vars,homogeneous},

	{mat,vars}=EquationsToMatrixAndVars[equations];
	homogeneous=vars//Last//#===one&//Not;
	If[homogeneous,Print["The input equations aren't inhomogeneous"];Abort[];];
	ConsistentMatrixQ[mat,prime,OptionsList]
];


End[]

EndPackage[]

Print["
---- FINITE FIELD SOLVE ----

FiniteFieldSolve exactly solves large linear systems of equations over the rationals.
Some pieces of the front end were borrowed from spasmlink written by Gregor K\[ADoubleDot]lin, Alex Edison, and Mao Zeng and available at gitlab.com/kaelingre/spasmlink.
For more information about FiniteFieldSolve see github.com/jfmangan/FiniteFieldSolve and arxiv.org/abs/2311.01671 by James Mangan."];
