(* ::Package:: *)

BeginPackage["FiniteFieldSolve`"]


FiniteFieldSolve::usage = "FiniteFieldSolve[equations, (optional: optionsList)] solves a List of linear equations.  FiniteFieldSolve will automatically detect the variables in the equations.  The equations can contain '==' but if they do not contain '==' then it is assumed that the equation is equal to zero.  Note that 'equations' will likely take up more memory than the SparseArray representing them, so it is recommended to directly call FiniteFieldSolveMatrix for advanced usage.  optionsList is an optional parameter taking the form {string1, string2...}.  Including 'verbose' in optionsList will print more output.  By default FiniteFieldSolve will use the first prime it solves over to determine the linearly independent rows and only use those rows when solving over subsequent primes.  To disable this behavior include 'KeepLinearDepRows' in optionsList.  By default FiniteFieldSolve will use the first prime to detect if solving the equations sets any variables to zero.  These variables will not be solved for in the future and the ensuing linearly dependent rows will be removed by row reducing over the second prime.  To disable this behavior include 'KeepZeroVariables' in optionsList.  Row reduction will default to 16-bit primes which are generally faster and use less memory.  To use 32-bit primes include '32bit' in optionsList.  Add 'ClearDenominators' to optionsList in order to clear all of the denominators in the equations.  This is especially useful for dense equations generated numerically.  By default the equations are sorted by their density (after calling CoefficientArrrays on 'equations') so that more complicated linearly dependent rows are discarded.  To disable this feature include 'NoRowSorting' in optionsList.  By default, the function statically allocates a contiguous block of memory for the matrix to improve performance.  This may not be possible for sufficiently large matrices, in which case you can switch to dynamically allocating the matrix by adding 'dynamic' to optionsList.";

FiniteFieldSolveMatrix::usage = "FiniteFieldSolveMatrix[matrix, vars, homogeneous, (optional: optionsList)] solves the linear system 'matrix' over the variables 'vars'.  The matrix does not need to be sparse or a SparseArray.  If homogeneous is False then the last element of vars is the variable used to represent 1 so you are always effectively solving the homogeneous system Ax=0.  optionsList is an optional parameter taking the form {string1, string2...}.  By default FiniteFieldSolveMatrix will use the first prime it solves over to determine the linearly independent rows and only use those rows when solving over subsequent primes.  To disable this behavior include 'KeepLinearDepRows' in optionsList.  By default FiniteFieldSolve will use the first prime to detect if solving the equations sets any variables to zero.  These variables will not be solved for in the future and the ensuing linearly dependent rows will be removed by row reducing over the second prime.  To disable this behavior include 'KeepZeroVariables' in optionsList.  Row reduction will default to 16-bit primes which are generally faster and use less memory.  To use 32-bit primes include '32bit' in optionsList.  By default if 'matrix' is a SparseArray then its rows will be sorted by their density so that more complicated linearly dependent rows are discarded.  To disable this feature include 'NoRowSorting' in optionsList.  For memory reasons, 'ClearDenominators' is not an option for FiniteFieldSolveMatrix.  By default, the function statically allocates a contiguous block of memory for the matrix to improve performance.  This may not be possible for sufficiently large matrices, in which case you can switch to dynamically allocating the matrix by adding 'dynamic' to optionsList.";

RowReduceOverPrime::usage = "RowReduceOverPrime[matrix, prime, (optional: StaticOrDynamicMem), (optional: rowsToUse), (optional: colsToUse)] row reduces 'matrix' (not necessarily sparse or a SparseArray) over the prime.  The prime must be less than 2^32 where primes less than 2^16 will be faster and consume less memory.  In the output, rows are not rearranged to be in strict RREF form.  Futhermore, rows of zeros are not deleted so it is easy to determine which rows of the initial matrix are linearly independent.  The function will return $Failed if the matrix could not be projected over the given prime.  rowsToUse is an optional argument that defaults to All.  If you only want to use certain rows in the matrix then set rowsToUse to, for example, {1,4,5...}.  colsToUse is the analog of rowsToUse but for columns.  StaticOrDynamicMem controls how the memory for the matrix is allocated.  StaticOrDynamicMem defaults to 'static' where the matrix is statically allocated as a contiguous block of memory to improve speed.  For sufficiently large matrices there may not be a contiguous block of memory available so you may need to dynamically allocate the matrix by setting StaticOrDynamicMem to 'dynamic'.";

FindLinearlyIndependentRows::usage = "FindLinearlyIndependentRows[InputMatrix, (optional: prime), (optional: optionsList)] returns a matrix made from the linearly independent rows of InputMatrix.  Running this function is considerably faster than solving the whole system so it may be valuable to run this function before solving an over-constrained system.  By default if 'InputMatrix' is a SparseArray then its rows will be sorted by their density so that more complicated linearly dependent rows are discarded.  To disable this feature include 'NoRowSorting' in optionsList.  By default, the function statically allocates a contiguous block of memory for the matrix to improve performance.  This may not be possible for sufficiently large matrices, in which case you can switch to dynamically allocating the matrix by adding 'dynamic' to optionsList.";

FindLinearlyIndependentEquations::usage = "FindLinearlyIndependentEquations[InputEquations, (optional: prime), (optional: optionsList)] returns the linearly independent InputEquations.  Running this function is considerably faster than solving the whole system so it may be valuable to run this function before solving an over-constrained system.  By default the rows of InputEquations are sorted by their density so that more complicated linearly dependent equations are discarded.  To disable this feature include 'NoRowSorting' in optionsList.  By default, the function statically allocates a contiguous block of memory for the matrix to improve performance.  This may not be possible for sufficiently large matrices, in which case you can switch to dynamically allocating the matrix by adding 'dynamic' to optionsList.";

ConsistentMatrixQ::usage = "ConsistentMatrixQ[matrix, (optional: prime), (optional: optionsList)] takes a matrix representing an inhomogeneous system of equations and returns False if the system is inconsistent and True otherwise.  As with FiniteFieldSolve, the last column of the matrix is assumed to represent the inhomogeneous terms.  Running this function may be considerably faster than solving the whole system so it may be valuable to run this function before solving an inhomogeneous system especially if an inconsistent solution is likely.  By default, the function statically allocates a contiguous block of memory for the matrix to improve performance.  This may not be possible for sufficiently large matrices, in which case you can switch to dynamically allocating the matrix by adding 'dynamic' to optionsList.";

ConsistentEquationsQ::usage = "ConsistentEquationsQ[equations, (optional: prime), (optional: optionsList)] takes an inhomogeneous system of equations and returns False if the system is inconsistent and True otherwise.  Running this function may be considerably faster than solving the whole system so it may be valuable to run this function before solving an inhomogeneous system especially if an inconsistent solution is likely.  By default, the function statically allocates a contiguous block of memory for the matrix to improve performance.  This may not be possible for sufficiently large matrices, in which case you can switch to dynamically allocating the matrix by adding 'dynamic' to optionsList.";


Begin["`Private`"]


reconstruct[0, _] := 0;
reconstruct[a_, n_] := 
  With[{v = LatticeReduce[{{a, 1}, {n, 0}}][[1]]}, v[[1]]/v[[2]]];
(*From Ben Page https://youtu.be/3cGyeN1a7bs?t=2806*)


chineseRemainderMats[{matrices__SparseArray}, primes_List] := With[{nonZeroPos = DeleteDuplicates[Flatten[#["NonzeroPositions"]&/@{matrices}, 1]]},
    SparseArray[
    	Table[
        	pos->ChineseRemainder[#[[Sequence@@pos]]&/@{matrices}, primes]
	    	,{pos, nonZeroPos}]
    	, Dimensions[{matrices}[[1]]]]
]


rationalToInt[0,pp_Integer]:=0;
rationalToInt[aa_Rational,pp_Integer]:=Mod[Numerator[aa]*ModularInverse[Denominator[aa],pp],pp];
rationalToInt[aa_Integer,pp_Integer]:=Mod[aa,pp];


PackageDir=$InputFileName//DirectoryName;(*Get the package's directory.  $InputeFileName is only available when you initialize the package so you have to store it as a private variable.*)

CompileLibraryLink[numRow_Integer,numCol_Integer,prime_Integer,StaticOrDynamicMem_:"static"]:=Block[{LibraryLinkFullPath,gccString, performanceString, headersString, matrixOptionsString, sourceString, oString,targetString,commandString,IsOpenMPInstalled,openmpString,message,sharedLibString,sharedLibExtension,os,uIntShort,uIntLong, procType, MarchOrMcpu, BytesPerInt,FreeMem,MatMem, MemCutoff, mallocString, StaticallyAllocateMem},

	If[!(prime<2^32&&PrimeQ[prime]),Print[prime, " needs to be prime and less than 2^32"];Abort[];];
	
	If[prime<2^16,
	uIntShort="uint16_t";
	uIntLong="uint32_t";,
	uIntShort="uint32_t";
	uIntLong="uint64_t";
	];
	
	If[prime<2^16,
	BytesPerInt=2;,
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
	MatMem=numRow*numCol*BytesPerInt/1024^3//N;
	MemCutoff=1/3//N;
	If[MatMem>MemCutoff*FreeMem,
		Print["Potentially low memory."];
		Print["Free memory ",FreeMem//NumberForm[#,3]&//ToString," GB.  Memory needed for matrix ",MatMem//NumberForm[#,3]&//ToString," GB."];
		If[StaticallyAllocateMem===True,
			Print["If there is erratic behavior or crashes try adding 'dynamic' to optionsList."];,
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
	sharedLibString="-dynamiclib ";
	sharedLibExtension=".dylib";
	,
	StringContainsQ[os,"linux"],
	(*linux options*)
	gccString= "g++ ";
	sharedLibString="-fpic -shared ";
	sharedLibExtension=".so";
	,
	True,
	Print["Operating system not recognized.  Only Linux and Mac are supported."]; Abort[];
	];
	
	IsOpenMPInstalled=False;(*True;*)
	If[IsOpenMPInstalled, openmpString="-fopenmp ", openmpString=" "];
	
	procType=$ProcessorType//ToLowerCase;
	Which[StringContainsQ[procType,"x86"],
	MarchOrMcpu="-march=native ";
	,
	StringContainsQ[procType,"arm"],
	MarchOrMcpu="-mcpu=native ";
	,
	True,
	MarchOrMcpu=" ";
	];
	performanceString = StringJoin[MarchOrMcpu,"-O3 "];
	
	If[StaticallyAllocateMem,
	mallocString=" -D ALLOC_STATIC_MEM=true ",
	mallocString=" -D ALLOC_STATIC_MEM=false "
	];

	headersString = StringJoin["-I ", $InstallationDirectory, "/SystemFiles/IncludeFiles/C"];
	matrixOptionsString = StringJoin[" -D PRIME=", prime // ToString, " -D NUM_ROW=", numRow//ToString, " -D NUM_COL=", numCol//ToString, " -D U_INT_SHORT=", uIntShort, " -D U_INT_LONG=", uIntLong, " "];
	sourceString = StringJoin[LibraryLinkFullPath, ".cpp"];
	oString = " -o ";
	targetString = StringJoin[LibraryLinkFullPath, sharedLibExtension];
	commandString = StringJoin[gccString, performanceString, sharedLibString, openmpString, headersString, matrixOptionsString, mallocString, sourceString, oString, targetString];
	message=commandString // Run;
	If[message=!=0,Print["Error compiling the shared library!  Try running the following command in a terminal for more information."];Print[commandString];If[StringContainsQ[procType,"arm"],Print["Different compilers have different issues with ARM processors so it may be worth disabling '-mcpu=native ' or trying '-march=native ' instead."];];Abort[];];

]


RowReduceOverPrime[coefArr_,prime_Integer,StaticOrDynamicMem_:"static",rowsToUse_:All,colsToUse_:All]:=Block[
	{i,row,mat,RowReduceNumericArray,PopulateRowOfMatrix,LibraryLinkFullPath,rowRange,colRange,uIntType},
	
	If[rowsToUse===All,rowRange=coefArr//Dimensions//First//Range,rowRange=rowsToUse];
	If[colsToUse===All,colRange=coefArr//Dimensions//Last//Range,colRange=colsToUse];
	
	CompileLibraryLink[rowRange//Length, colRange//Length, prime, StaticOrDynamicMem];
	
	(*Print["Dimensions: ", rowRange//Length, ", ", colRange//Length];*)(*Helpful for debugging*)
	
	LibraryLinkFullPath=StringJoin[PackageDir,"RowReduceLink"];
	
	(*This function loads one row of data into the matrix stored in the LibraryLink shared library.*)
	PopulateRowOfMatrix=LibraryFunctionLoad[LibraryLinkFullPath, "PopulateRowOfMatrix", {{LibraryDataType[NumericArray], "Constant"},{LibraryDataType[Integer],"Constant"}}, {Integer}];(*I had issues when I marked any of these as Constant even though I think they are.*)
	
	If[prime<2^16,uIntType="UnsignedInteger16",uIntType="UnsignedInteger32"];
	
	(*The following For loop represents a non-trivial portion of the solve time*)
	(*I think passing as a MNumericArray is a tiny bit slower than passing as a MTensor but the difference is very small.*)
	For[i=1,i<=Length[rowRange],i++,
	row=Quiet[Check[coefArr[[rowRange[[i]]]]//rationalToInt[#,prime]&/@#&//NumericArray[#,uIntType]&//#[[colRange]]&,Return[$Failed]]];
	(*It is fastest to section the row after it has been changed from a SparseArray into a NumericArray, that is, put [[colRange]] last.  It's really dumb that it's faster to do rationalToInt on data you won't use rather than sectioning the array first.  This is one of the consequences of not making an internal copy of the input coefArr.*)
	PopulateRowOfMatrix[row,i];
	];
	(*If coefArr is a SparseArray with low density then it is a little more effecient to do...*)
	(*row=Quiet[Check[coefArr[[rowRange[[i]]]]//ArrayRules//MapAt[rationalToInt[#,prime]&,#,-1]&/@#&//SparseArray[#,coefArr//Dimensions//Last]&//NumericArray[#,uIntType]&,Return[$Failed]]];*)
	
	RowReduceNumericArray=LibraryFunctionLoad[LibraryLinkFullPath, "RowReduceNumericArray", {}, {LibraryDataType[SparseArray]}];
	
	(*Do the row reduction*)
	mat=RowReduceNumericArray[];
	
	LibraryUnload[LibraryLinkFullPath];
	(*The memory useage looks like this:  There is the original copy of the matrix called coefArr in arbitrary precision rationals.  The Library Link function has a 16-bit (or 32-bit) copy of this matrix.  You pass the matrix one row at a time because if you pass the matrix by pointer then the row reduction is slower.  When mat is row reduced it is turned into a new SparseArray of (hopefully) negligible memory.  So the memory you need to solve coefArr should be the memory needed to store coefArr plus the size of mat.  The memory will grow slightly over time as you build up the solution using more primes.*)
	
	(*mat may contain rows of zeros (representing linearly dependent rows).  To delete them you'd have to do...*)
	(*mat["NonzeroPositions"]//First/@#&//Union//mat[[#]]&*)
	
	mat
]


rrefToRules[rrefMat_SparseArray,vars_List,oneAlias_]:=Block[
	{solRules,varArray,varSol},
	solRules = Table[
	varArray = (Sort[ArrayRules[sparseRow][[;;-2]]]/.Rule[{b_},x_]:>x*vars[[b]]);
	If[Length[varArray]===0,
		varSol = varArray[[1]]->0;
		,
		varSol = First[varArray]->-Total[Rest[varArray]];
	];
	If[varSol===(oneAlias->0),
		Return[$Failed,Block];
	];
	varSol
	
	,{sparseRow,rrefMat}];
	solRules
]


MemType[optionsList_]:=If[MemberQ[optionsList//ToLowerCase,"dynamic"//ToLowerCase],
		"dynamic",
		"static"
];


FiniteFieldSolveMatrix[coefArr_,vars_List,homogeneous_,optionsList_List:{}]:=Block[
	{attempts, solRules, verbosePrint, oneAlias,
	primeList, usedPrimes, projection, reconstruction,
	primeToUse, newProjection, newConstruction, linearIndepRows,
	rowsToUse, SortMatIntoStrictRREFForm,
	removeLinearlyDependentRows, ColumnsOfZeroVars, ZeroRules,
	colsToUse, removeVariablesSetToZero, TmpTime,
	indepVars, indepVarsRep, nullVec, foundSolution, printModErr,
	issuedWarning},
	
	(*Basic tests on input data*)
	If[BooleanQ[homogeneous]==False,Print[homogeneous, " needs to be True or False"];Abort[]];
	If[Not[Or[Head[coefArr]==SparseArray,Head[coefArr]==List]],Print["The Head of the input matrix needs to be List or SparseArray"];Abort[]];
	If[(coefArr//Dimensions//Last)=!=Length[vars],Print["The number of variables does not match the number of columns in the matrix"];Abort[]];
	
	removeLinearlyDependentRows=MemberQ[optionsList//ToLowerCase,"KeepLinearDepRows"//ToLowerCase]//Not;
	removeVariablesSetToZero=MemberQ[optionsList//ToLowerCase,"KeepZeroVariables"//ToLowerCase]//Not;
	
	(*If the system is inhomogeneous, make oneAlias the last element of vars*)
	If[homogeneous,oneAlias=Nothing,oneAlias=vars//Last];
	
	If[MemberQ[optionsList//ToLowerCase,"verbose"],verbosePrint[str___]:=Print[str]];
	issuedWarning=False;
	printModErr:=If[!issuedWarning,issuedWarning=True; Print["Bad projection!  Have you tried clearing the denominators of the system of equations by adding 'ClearDenominators' to the optionsList of FiniteFieldSolve?  Further instances of this warning will be suppressed."];];

	(*If you remove rows or columns from the matrix then when you solve the matrix again over a different prime, you might end up with an row reduced matrix with the rows in a different order.  That is, the linearly dependent rows might have affected the order in which you solve the rows.  So if you remove the linearly dependent rows you need to canonicalize the ordering of the rows.  This is only for reconstructing the actual solution so only use this right before reconstructing something.*)
	SortMatIntoStrictRREFForm[mat_]:=
	If[Or[removeLinearlyDependentRows,removeVariablesSetToZero],
		Block[{canonicalRowOrdering},
			(*Sort the rows into RREF form*)
			canonicalRowOrdering=mat["NonzeroPositions"]//GatherBy[#,First]&//SortBy[#,Last]&/@#&//First/@#&//SortBy[#,Last]&//First/@#&;
			mat[[canonicalRowOrdering]]
			]
		,mat];
	
	If[MemberQ[optionsList//ToLowerCase,"32bit"//ToLowerCase],
	primeList = Range[203280221,203280221-500,-1]//Prime/@#&(*The largest prime needs to fit in 32 bits*)
	,
	primeList = Range[6542,6542-500,-1]//Prime/@#&;(*The largest prime needs to fit in 16 bits*)
	];
	
	usedPrimes = {};
	attempts = 1;
	projection=$Failed;
	
	(*If requested, do not sort the rows of coefArr by their density.*)
	If[And[MemberQ[optionsList//ToLowerCase,"NoRowSorting"//ToLowerCase]//Not,Head[coefArr]===SparseArray],
	rowsToUse=coefArr//#["Density"]&/@#&//Ordering;
	(*You could also sort the rows so that a row with its first non-zero entry very far right goes above other rows.  The rows that are high up don't need to update as many positions in the rows below them.  You could also sort rows by some combination of their density and the position of their first non-zero entry.*)
	(*rowsToUse=coefArr//#["NonzeroPositions"]&/@#&//First/@#&//Flatten//-1*#&/@#&//Ordering;(*Assumes that SparseArray positions are already sorted*)*)
	,
	rowsToUse=coefArr//Length//Range;
	(*The reason you don't set rowsToUse to All is that you might omit certain rows later if removeVariablesSetToZero is True*)
	];
	
	(*Build initial solution*)
	(*For the first prime, check if the matrix is inconsistent, select the linearly independent rows, and omit columns that only serve to set variables to zero.*)
	
	While[projection===$Failed,
		verbosePrint["Matrix dimensions: ", coefArr//Dimensions];
		TmpTime=AbsoluteTime[];
		projection = RowReduceOverPrime[coefArr,primeList[[attempts]],MemType[optionsList],rowsToUse];(*projection may contain rows of zeros representing linearly dependent rows*)
		(*All rows of zeros are included in projection right now so projection is just a pure rref*)
		verbosePrint["Time (sec) used to row reduce: ", AbsoluteTime[]-TmpTime];
		If[projection===$Failed,printModErr;];		
		usedPrimes = {primeList[[attempts]]};
		attempts++;
	];
	
	(*Test if you were handed the zero matrix.  This will erroneously abort if you were handed a matrix of only 65521's (or multiples of whatever prime you are reducing over).*)
	If[projection["NonzeroPositions"]==={},Print["Zero input matrix detected."];Return[{}]];
	
	(*Check to see if the matrix is inconsistent and abort if it is.  There's no need to fully solve the system if you can quickly find out that it's inconsistent.*)
	If[homogeneous===False,
		If[ConsistentMatrixQHelper[projection]===False,
			Print["Inconsistent solution"];
			Abort[];
		];
	];
	
	(*Find the linearly independent rows, i.e., the nonzero rows of the rref.
	Remove the zero rows from projection.  If solving with removeLinearlyDependentRows===True, remove the linearly dependent rows from coefArr*)
	linearIndepRows=projection["NonzeroPositions"]//First/@#&//Union;
	projection=projection[[linearIndepRows]];
	
	(*Unfortunately you can't just set coefArrNew = coefArr[[linearIndepRows]] because then MMA will make a new copy of the matrix.
	You could avoid making a copy of the matrix by doing SetAttributes[FiniteFieldSolve,HoldFirst] but then you would overwirte the input matrix.
	The best solution I can think of is to do a bunch of gymnastics where you only pass in certain rows to row reduce.*)
	If[removeLinearlyDependentRows,
	rowsToUse=rowsToUse[[linearIndepRows]]
	];
	
	(*Find the columns that just set variables to zero and remove them from future row reductions.*)
	(*As an example of why the RREF tells you which columns to remove but not which rows, consider the matrix {{1,1,x},{1,0,0}}.*)
	If[removeVariablesSetToZero,
		ColumnsOfZeroVars=projection["NonzeroPositions"]//GatherBy[#,First]&//Select[#,Length[#]==1&]&//Flatten/@#&//Last/@#&;
		ZeroRules=ColumnsOfZeroVars//vars[[#]]&/@#&//#->0&/@#&;
		colsToUse=(projection//Dimensions//Last//Range)~Complement~ColumnsOfZeroVars;(*So this is just the complement of ColumnsOfZeroVars*)
		If[colsToUse==={},Return[ZeroRules]];
		,
		ZeroRules={};
		colsToUse=All;
	];
	
	projection=projection//#[[All,colsToUse]]&//SortMatIntoStrictRREFForm;

	(*After removing variables that are set to zero, the matrix could be empty, so you return early.  (Proceeding with normal execution will produce many errors.)  You could mistakenly return early here if your matrix involves a bunch of variables that get set to zero and all of the other entries in the matrix are multiples of 65521 (or another prime) that 'projection' interprets as 0.*)
	If[projection==={},Return[ZeroRules]];(*If projection is an empty SparseArray (after the zero vars are removed) then SortMatIntoStrictRREFForm will turn projection into {}.*)
	
	reconstruction = SparseArray[projection["NonzeroPositions"]->(reconstruct[#,usedPrimes[[1]]]&/@projection["NonzeroValues"]),Dimensions[projection]];
	verbosePrint["Prime used: ",usedPrimes[[1]]];
	
	While[attempts<=Length[primeList],
	
		verbosePrint["Working on prime number: ", Length[usedPrimes]+1];
		
		If[And[Length[usedPrimes]==2,removeVariablesSetToZero,removeLinearlyDependentRows],
			(*You found the zero variables with the first prime. With the second prime you found the newly linearly dependent variables. Now if you're on the third prime you can omit those rows using the linearIndepRows from solving over the 2nd prime.  This is all more obvious if you print the dimenions of the matrix with each prime.*)
			rowsToUse=rowsToUse[[linearIndepRows]];
			(*I thought you'd have to go back and modify the RREFs and projections for previous primes but that doesn't seem to be the case since they're in strict RREF form.*)
		];
		
		newProjection = $Failed;
		While[newProjection===$Failed,
			primeToUse = primeList[[attempts]];
			TmpTime=AbsoluteTime[];
			
			solRules = rrefToRules[reconstruction,vars[[colsToUse]],oneAlias];
			If[solRules===$Failed,
				Print["Inconsistent solution"];
				Return[{oneAlias->0}]
			];
			
			indepVars=solRules//Last/@#&//Union//Variables;
			indepVarsRep=indepVars//MapThread[Rule,{#,RandomInteger[{0,primeList[[attempts]]-1},Length[#]]}]&//Dispatch;
			Quiet[Check[nullVec=vars[[colsToUse]]/.indepVarsRep/.Dispatch[solRules/.indepVarsRep]//rationalToInt[#,primeList[[attempts]]]&/@#&;,printModErr; attempts++; Continue[];]];
			(*The following matrix-vector multiplication is surprisingly fast.*)
			Quiet[Check[foundSolution=coefArr[[rowsToUse,colsToUse]] . nullVec//rationalToInt[#,primeList[[attempts]]]&/@#&//DeleteCases[0]//#==={}&;,printModErr; attempts++; Continue[];]];
			(*foundSolution will be true if the random null vector really is a null vector, meaning you've found a solution*)
			If[foundSolution,
				Return[Join[ZeroRules,solRules]];,
				
				verbosePrint["Matrix dimensions: ", {Length[rowsToUse],Length[colsToUse]}];
				newProjection = RowReduceOverPrime[coefArr,primeList[[attempts]],MemType[optionsList],rowsToUse,colsToUse];
				verbosePrint["Time (sec) used to row reduce: ", AbsoluteTime[]-TmpTime];
				If[newProjection===$Failed,printModErr;];
				attempts++;
			];
		];
		verbosePrint["Prime used: ",primeToUse];
		
		linearIndepRows=newProjection["NonzeroPositions"]//First/@#&//Union;
		newProjection=newProjection[[linearIndepRows]];
		newProjection = newProjection//SortMatIntoStrictRREFForm;
		newProjection = chineseRemainderMats[{projection,newProjection},{Times@@usedPrimes,primeToUse}];

		AppendTo[usedPrimes,primeToUse];
		
		newConstruction = SparseArray[newProjection["NonzeroPositions"]->(reconstruct[#,Times@@usedPrimes]&/@newProjection["NonzeroValues"]),Dimensions[newProjection]];
									
		(*------------*)
		
		(*Identifying the extraneous columns of independent variables will help if you have lots of free vars in your system*)
		
		(*
		pivotCols=reconstruction["NonzeroPositions"]//GatherBy[#,First]&//First/@#&//Last/@#&;
		indepVarCols=Complement[reconstruction//Dimensions//Last//Range,pivotCols];
		cols1=reconstruction//#[[indepVarCols]]&/@#&//SparseArray;
		cols2=newConstruction//#[[indepVarCols]]&/@#&//SparseArray;
		cols1-cols2//SparseArray//#["NonzeroPositions"]&//Last/@#&//Union//Length//Print;
		*)
		
		(*------------*)
		
		projection = newProjection;
		reconstruction = newConstruction;
	
	];
	
	Return[$Failed];(*This only happens if you exit the While loop above which means you ran out of primes.*)
];


LCMHelper[rowSparseArr_]:=If[rowSparseArr["NonzeroValues"]==={},
rowSparseArr,
rowSparseArr["NonzeroValues"]//Denominator/@#&//LCM@@#&//#*rowSparseArr&
];


one;(*This Head is the variable that will be used to represent 1.  one is in the Private scope (Context) of this package.*)


EquationsToMatrixAndVars[equations_List]:=Block[{vars, matrix, homogeneous, coefArrs},
vars=equations/.x_==y_:>x-y//Variables;
coefArrs=CoefficientArrays[equations,vars];
If[Length[coefArrs]=!=2, Print["The input equations are not linear."];Abort[];];
homogeneous=coefArrs//First//#["NonzeroPositions"]&//#==={}&;

If[homogeneous,
	matrix=coefArrs[[2]];,
	matrix=MapThread[Append,{coefArrs[[2]],coefArrs[[1]]}]//SparseArray;
	AppendTo[vars,one];
];
Clear[coefArrs];

{matrix,vars}
];


FiniteFieldSolve[equations_List, optionsList_List:{}]:=Block[{vars, matrix, homogeneous, coefArrs, sol},
{matrix,vars}=EquationsToMatrixAndVars[equations];
homogeneous=vars//Last//#===one&//Not;

If[MemberQ[optionsList//ToLowerCase,"ClearDenominators"//ToLowerCase], matrix=matrix//LCMHelper/@#&//SparseArray];(*I don't have the option to clear denominators in FiniteFieldSolveMatrix because I don't want to incur an unnecessary memory hit.*)

sol=FiniteFieldSolveMatrix[matrix,vars,homogeneous,optionsList];
If[homogeneous,sol,sol/.one->1]
];


FindLinearlyIndependentRowsHelper[mat_, prime_Integer:65521, optionsList_List:{}]:=Block[{rref,NonZeroRows,rowsToUse},

	(*If requested, do not sort the rows of coefArr by their density.*)
	If[And[MemberQ[optionsList//ToLowerCase,"NoRowSorting"//ToLowerCase]//Not,Head[mat]===SparseArray],
	rowsToUse=mat//#["Density"]&/@#&//Ordering;
	,
	rowsToUse=mat//Length//Range;
	];
	
	rref=RowReduceOverPrime[mat,prime,MemType[optionsList],rowsToUse];
	If[rref===$Failed,Return[$Failed]];
	NonZeroRows=rref["NonzeroPositions"]//First/@#&//Union;
	rowsToUse[[NonZeroRows]]
];


FindLinearlyIndependentRows[mat_, prime_Integer:65521, optionsList_List:{}]:=
Block[{rows},
	rows=FindLinearlyIndependentRowsHelper[mat, prime, optionsList];
	If[rows===$Failed,$Failed,mat[[rows]]]
]


FindLinearlyIndependentEquations[equations_List, prime_Integer:65521, optionsList_List:{}]:=
Block[{mat,vars,rows},
	{mat,vars}=EquationsToMatrixAndVars[equations];
	rows=FindLinearlyIndependentRowsHelper[mat, prime, optionsList];
	If[rows===$Failed,$Failed,equations[[rows]]]
];


ConsistentMatrixQHelper[rref_]:=Block[{NonZeroPos,ProblematicRows,NumberOfVariables},
(*You have an inconsistent matrix if the last variable (which is supposed to represent 1) is ever set to zero, that is, if you ever have a row that involves the last variable where the number of non-zero entries in the row is 1.*)
NumberOfVariables=rref//Dimensions//Last;
NonZeroPos=rref["NonzeroPositions"];
ProblematicRows=NonZeroPos//Select[#,MatchQ[#,{_,NumberOfVariables}]&]&//First/@#&;
rref[[ProblematicRows]]//#["NonzeroPositions"]&/@#&//Length/@#&//MemberQ[#,1]&//Not(*This assumes that SparseArray correctly removes all zeros from "NonzeroPositions"*)
];


ConsistentMatrixQ[CoefficientMatrix_, prime_Integer:65521, optionsList_List:{}]:=Block[{rref},
rref=RowReduceOverPrime[CoefficientMatrix,prime,MemType[optionsList]];
ConsistentMatrixQHelper[rref]
];


ConsistentEquationsQ[equations_List, prime_Integer:65521, optionsList_List:{}]:=Block[{mat,vars,homogeneous},
{mat,vars}=EquationsToMatrixAndVars[equations];
homogeneous=vars//Last//#===one&//Not;
If[homogeneous,Print["The input equations aren't inhomogeneous"];Abort[];];
ConsistentMatrixQ[mat,prime,optionsList]
];


End[]

EndPackage[]

Print["
---- FINITE FIELD SOLVE ----

FiniteFieldSolve exactly solves large linear systems of equations over the rationals.
Some pieces of the front end were borrowed from spasmlink written by Gregor K\[ADoubleDot]lin, Alex Edison, and Mao Zeng and available at gitlab.com/kaelingre/spasmlink.
For more information about FiniteFieldSolve see github.com/jfmangan/FiniteFieldSolve and arxiv.org/abs/2311.01671 by James Mangan."];
