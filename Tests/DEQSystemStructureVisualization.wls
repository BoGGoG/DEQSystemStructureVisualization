#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*Test DEQSystemStructureVisualization*)


sourceFolder = "source";
sourcePath = FileNameJoin[{ParentDirectory@NotebookDirectory[], sourceFolder}];
$Path=Once[Append[$Path,sourcePath]];
<<"DEQSystemStructureVisualization`"


(* ::Section:: *)
(*AllSumRepresentations*)


AllSumRepresentations[0]
AllSumRepresentations[1]
AllSumRepresentations[2]
AllSumRepresentations[3]
AllSumRepresentations[4]
AllSumRepresentations[5]


SumsToNQ[n_][list_]:= Total[list] == n;
TestAllSumRepresentations[n_] := Map[SumsToNQ[n], AllSumRepresentations[n]] (* Test all elements that should sum to n *)


Map[TestAllSumRepresentations, Range[10]]


Length[AllSumRepresentations[10]]


(* ::Subsection:: *)
(*Integer Partition*)


(* ::Text:: *)
(*I found out that integer partition does exactly what I needed. Well, I learned a lot by implementing it.*)
(*Integer partition is 50k-60k times faster than AllSumRepresentations*)


Length[IntegerPartitions[10]]


Length[AllSumRepresentations[10]]


(* ::Section:: *)
(*AllNthDerivatives*)


<<"DEQSystemStructureVisualization`"


allNthDerivatives[f[x], 0]


allNthDerivatives[f[x], 1]


allNthDerivatives[f[x,y,z], 0]


allNthDerivatives[f[x,y,z], 1]


allNthDerivatives[f[x], 2]


allNthDerivatives[f[x,y], 2]


allNthDerivatives[f[x,y], 3]


allNthDerivatives[f[x,y,z], 2]


allNthDerivatives[f[x,y,z], 3]


allNthDerivatives[{f[x], g[x,y]}, 2]


(* ::Section:: *)
(*AllUpToNthDerivatives*)


AllUpToNthDerivatives[f[x],2]


AllUpToNthDerivatives[f[x,y,x],2]


AllUpToNthDerivatives[{f[x], g[x,y]}, 2]


TableForm[AllUpToNthDerivatives[{f[x], g[x,y], h[a,b,c]}, 3]]


(* ::Section:: *)
(*ShowDEQSystemForm*)


<<"DEQSystemStructureVisualization`"


fields = {f[x], g[x,y]}


deq1 = qf f''[x] + pf f'[x] + wf f[x] + s1 + qgx D[g[x,y], {x,2}] + qgy D[g[x,y], {y,2}]
deq2 = qf2 f''[x] + qgxy2 D[g[x,y], x,y] + wg2 g[x,y] + s2
deqs = {deq1, deq2};


ShowDEQSystemStructure[deqs, fields, "MaxDerivativeOrder"->2]
