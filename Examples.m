(* ::Package:: *)

(* ::Section:: *)
(*Load FiniteFieldSolve*)


<<FiniteFieldSolve`


(* ::Section:: *)
(*Basic example*)


SimpleEquations={x[1]-x[2],1/2x[1]+x[2]-3};
SimpleEqSol=SimpleEquations//FiniteFieldSolve
SimpleEquations/.SimpleEqSol


(* ::Section:: *)
(*Setup dot product*)


SetAttributes[d,Orderless];
d[x_,y_+z_]:=d[x,y]+d[x,z];
d[x_,y_ p[z_]]:=y d[x,p[z]];
d2[x_]:=d[x,x];


(* ::Section:: *)
(*Setup contact term*)


n=8;
pList=Array[p,n];


AllDots=Outer[d,pList,pList]//Flatten//DeleteDuplicates;


dRules=Join[Table[d[p[i],p[i]]==0,{i,1,n}],AllDots//#==(#/.p[n]->-Total[Array[p,n-1]])&/@#&]//Reduce//ToRules;


IndepDots=dRules//Last/@#&//DeleteCases[0]//#/.d[x__]:>Sow[d[x]]&//Reap//Last//First//Flatten//DeleteDuplicates;


ContactBasis=Table[IndepDots,4]//Outer[Times,##]&@@#&//Flatten//DeleteDuplicates;


ContactBasis//Length


cVars=Array[c,Length[ContactBasis]];
ContactTerm=cVars . ContactBasis;


cVarsFull=Join[cVars,{c1,c0}];


(* ::Section:: *)
(*Build and solve perm sym equations*)


RepList[ListOld_,ListNew_]:=MapThread[Rule,{ListOld,ListNew}];


CyclicContactSym=ContactTerm-(ContactTerm/.RepList[pList,RotateLeft[pList,1]])/.dRules//CoefficientArrays[#,IndepDots]&//Last//#["NonzeroValues"]&//DeleteCases[0];//AbsoluteTiming
CyclicContactSym//Length


SwapContactSym=ContactTerm-(ContactTerm/.RepList[{p[2],p[3]},{p[3],p[2]}])/.dRules//CoefficientArrays[#,IndepDots]&//Last//#["NonzeroValues"]&//DeleteCases[0];//AbsoluteTiming
SwapContactSym//Length


SymSol=Join[CyclicContactSym,SwapContactSym]//FiniteFieldSolve;//AbsoluteTiming


Join[CyclicContactSym,SwapContactSym]/.Dispatch[SymSol]//Expand//DeleteCases[0]//AbsoluteTiming


cVars//Length
SymSol//Length


(* ::Section:: *)
(*Build 8pt DBI tree and enforce soft*)


PermSum[expr_,pList_]:=Sum[expr/.RepList[pList,perm],{perm,Permutations[pList]}];


v4Expr=1/4!PermSum[d[p1,p2]d[p3,p4],{p1,p2,p3,p4}]//Expand;
v4[p1_,p2_,p3_,p4_]:=Evaluate[v4Expr];


v6Expr=1/6!PermSum[d[p1,p2]d[p3,p4]d[p5,p6],{p1,p2,p3,p4,p5,p6}]//Expand;
v6[p1_,p2_,p3_,p4_,p5_,p6_]:=Evaluate[v6Expr];


amp=PermSum[c0 v4[p[1],p[2],p[3],-(p[1]+p[2]+p[3])]v4[p[1]+p[2]+p[3],p[4],p[5]+p[6]+p[7],p[8]]v4[p[5],p[6],p[7],-(p[5]+p[6]+p[7])]/(d2[p[1]+p[2]+p[3]]d2[p[5]+p[6]+p[7]])+c1 v6[p[1],p[2],p[3],p[4],p[5],p[6]+p[7]+p[8]]v4[p[6],p[7],p[8],-(p[6]+p[7]+p[8])]/d2[p[6]+p[7]+p[8]],Array[p,8]]+(ContactTerm/.Dispatch[SymSol])/.dRules;//AbsoluteTiming


(* ::Text:: *)
(*c0 and c1 take care of the symmetry factors*)


RndMax=10;
MapListToRandomNumbers[list_]:=Dispatch[MapThread[Rule,{list,RandomInteger[{1,RndMax},Length[list]]}]];


(* ::Text:: *)
(*Perform the soft shift on a leg that *wasn't* special when setting up the on-shell kinematics.*)


SoftEqn=Table[amp/.p[4]->z p[4]/.MapListToRandomNumbers[IndepDots]//Series[#,{z,0,1}]&//Normal//CoefficientList[#,z]&,Length[cVarsFull]-Length[SymSol]]//Flatten;//AbsoluteTiming


SoftSol=SoftEqn//0==#&/@#&//Reduce[#,Reverse[cVarsFull]]&//ToRules


(* ::Section:: *)
(*Dense example*)


SeedRandom[1];
RndMax=1000;
RndRat:=RandomInteger[{1,RndMax}]/RandomInteger[{1,RndMax}];


NumEqns=500;(*The number of equations is increased in the example in the paper.*)
RndEqns=Table[x[i]-RndRat x[NumEqns+1],{i,1,NumEqns}];


ScrambledEqns=Table[RndRat,NumEqns,NumEqns] . RndEqns;//AbsoluteTiming


xVars=Array[x,NumEqns+1];
ScrambledEqns//CoefficientArrays[#,xVars]&//Last//#["Density"]&


(* ::Text:: *)
(*The density will always be 100% by design*)


ScrambledEqns//0==#&/@#&//Solve[#,Reverse[xVars]]&//Quiet//First//RndEqns/.#&//DeleteCases[0]//#==={}&//AbsoluteTiming


ScrambledEqns//FiniteFieldSolve//RndEqns/.#&//DeleteCases[0]//#==={}&//AbsoluteTiming
