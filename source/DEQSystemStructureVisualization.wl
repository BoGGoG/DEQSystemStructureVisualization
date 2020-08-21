(* ::Package:: *)

(* ::Title::Closed:: *)
(*Package DEQSystemStructureVisualization*)


(* ::Text:: *)
(*Title: DEQSystemStructureVisualization*)
(*Purpose:*)
(*Authors: Marco Knipfer*)


(* ::Title::Closed:: *)
(*Begin Package*)


BeginPackage["DEQSystemStructureVisualization`"];

Unprotect["DEQSystemStructureVisualization`*"];
ClearAll["DEQSystemStructureVisualization`*", "DEQSystemStructureVisualization`Private`*"];


(* ::Chapter::Closed:: *)
(*Messages & Public Declarations*)


(* ::Text:: *)
(*Messages and documentation for public facing functions go here. *)


(* ::Section::Closed:: *)
(*Messages "GetArgs"*)


GetArgs::usage="GetArgs[f[x,y,...]] gets the arguments of the function f as a Sequence. In this case: Sequence[x,y,...].
Can convert to list by calling List on result.
";


(* ::Section::Closed:: *)
(*Messages "MapJoin"*)


MapJoin::usage="MapJoin[list, listsList]: Joins list with every sublist of listsList at level 1.
E.g. MapJoin[{1,2}, {{3,4},{5,6}}] \[Rule] {{1,2,3,4}, {1,2,5,6}}
";


(* ::Section::Closed:: *)
(*Messages "AllSumRepresentations"*)


AllSumRepresentations::usage="AllSumRepresentations[n]: Gives all possible combinations of integers that add up to n";


(* ::Section::Closed:: *)
(*Messages "allNthDerivatives"*)


allNthDerivatives::usage="allNthDerivatives[f, nn] returns all nth derivatives of the function f with respect to all variables,
example: allNthDerivatives[f[x,y], 2] --> {D[f[x,y], {x,2}], D[f[x,y], {y,2}], D[f[x,y], {x,1}, {y,1}]}
Assumes derivatives commute. Could add noncommuting later.
";


(* ::Section::Closed:: *)
(*Messages "AllUpToNthDerivatives"*)


AllUpToNthDerivatives::usage="AllUpToNthDerivatives[f, nMax] returns all n<=nMax derivatives of the function f with respect to all variables.
If you plug in a List of funcitons the output format is
fs = {f[x], g[x,y]}
Output:
	{ {all functions}, {all 1st derivatives}, {all 2nd derivatives}, ... }
Each sector is grouped by the respective functions
	{ all 1st derivatives } = { {f'[x]}, {\!\(\*SuperscriptBox[\(g\), TagBox[
RowBox[{\"(\", 
RowBox[{\"1\", \",\", \"0\"}], \")\"}],
Derivative],\nMultilineFunction->None]\)[x,y],\!\(\*SuperscriptBox[\(g\), TagBox[
RowBox[{\"(\", 
RowBox[{\"0\", \",\", \"1\"}], \")\"}],
Derivative],\nMultilineFunction->None]\)[x,y]} }
and so on.

";


(* ::Section::Closed:: *)
(*Messages "FunctionsDerivatives"*)


FunctionsDerivatives::usage="FunctionsDerivatives[functions, coords, numberOfDerivatives] returns a list of derivates of the functions with respect to all coords up to order numberOfDerivatives.";


(* ::Section::Closed:: *)
(*Messages "DEQStructure"*)


DEQStructure::usage="asdf";


(* ::Section::Closed:: *)
(*Messages "ExtractFunctions"*)


ExtractFunctions::usage="NOT WORKING. To extract all unknown functions from equations";


(* ::Section::Closed:: *)
(*Messages "ShowDEQSystemStructure"*)


ShowDEQSystemStructure::usage="ShowDEQSystemStructure[DEQs, fields] prints a table showing which derivatives of the fields are in which equation.
Input:
- DEQs: A system of differential equations, only LHS of LHS==0
- fields: The fields including their arguments, e.g. f[x], g[x,y], ...

Output:
Table:
- rows: Number of the DEQ: DEQ 1, DEQ 2, ...
- cols: order of derivative: 0 derivatives, 1 derivative, 2 derivatives, ...

Options:
- MaxDerivativeOrder: Default 2. I haven't yet written code to do this automatically
 ";


(* ::Title::Closed:: *)
(*Begin Private/Functions Definitions*)


(* ::Text:: *)
(*Private definitions here.*)


Begin["`Private`"];


(* ::Section::Closed:: *)
(*GetArgs*)


GetArgs[_[xx__]] := xx;


(* ::Section::Closed:: *)
(*MapJoin*)


MapJoin[list_, listList_] := Map[Join[list, #]&, listList];


(* ::Section::Closed:: *)
(*AllSumRepresentations*)


(* ::Text:: *)
(*This is actually the Integer Partition problem as I found out later.*)
(*There is a built in Function IntegerPartition, which gives exactly the same answer as far as I can tell, but IntegerPartition is*)
(*like 50k-60k times faster than mine for large n.*)


AllSumRepresentationsOrdered[0] := {{}};
AllSumRepresentationsOrdered[1] := {{1}};
AllSumRepresentationsOrdered[n_] := Flatten[Map[ MapJoin[{n-#}, AllSumRepresentationsOrdered[#]] &, Range[0,n-1]],1];
AllSumRepresentations[n_] := DeleteDuplicates[Map[Sort[#, Greater]&, AllSumRepresentationsOrdered[n]]];
AllSumRepresentations[n_, OptionsPattern["MaxNumberOfSummands"->-1]] := Block[
		{maxNumberOfSummands, allSumRepresentations},
	maxNumberOfSummands = OptionValue["MaxNumberOfSummands"];
	allSumRepresentations = AllSumRepresentations[n];
	
	If[maxNumberOfSummands == -1,
		allSumRepresentations,
		Select[allSumRepresentations, Length[#]<=maxNumberOfSummands &]
	]
];
SetAttributes[AllSumRepresentations, Listable];


(* ::Section::Closed:: *)
(*allNthDerivatives*)


allNthDerivatives[function_, n_] := Block[{args, numberOfArgs, derivativeNumbersBare, 
		derivativeNumbers, derivativeNumbersNonPermuted, onlyHeads},
	args = GetArgs[function];
	numberOfArgs = Length@List@args;
	
	derivativeNumbersBare = AllSumRepresentations[n, "MaxNumberOfSummands"->numberOfArgs];
	derivativeNumbersNonPermuted = Map[PadRight[#, numberOfArgs]&, derivativeNumbersBare];(* fill up with zeros until numberOfArgs *)
	derivativeNumbers = Flatten[Map[Permutations, derivativeNumbersNonPermuted],1];
	onlyHeads = Through[Map[Apply[Derivative], derivativeNumbers][Head@function]];
	Through[onlyHeads[args]]
];


SetAttributes[allNthDerivatives, Listable]


(* ::Section::Closed:: *)
(*AllUpToNthDerivatives*)


AllUpToNthDerivatives[function_, n_] := Block[{numberOfCoords},
	Map[allNthDerivatives[function, #]&, Range[0,n]]
]


AllUpToNthDerivatives[functions_?ListQ, n_] := Block[{numberOfCoords},
	Map[allNthDerivatives[functions, #]&, Range[0,n]]
]
(* somehow this gave wrong answers for lists of functions. This somehow fixes it ... *)


(* ::Section::Closed:: *)
(*DEQStructure*)


FlattenFieldsAndDerivs[fieldsAndDerivs_] := Map[Flatten, fieldsAndDerivs];
(* { { all 0th derivs }, {all 1st derivs}, {all 2nd derivs} ]
each sector is flat
  *)


DEQStructure[DEQ_, fields_, OptionsPattern["MaxDerivativeOrder"->2]] := Block[{n, fieldsAndDerivs},
	n = OptionValue["MaxDerivativeOrder"];
	fieldsAndDerivs = AllUpToNthDerivatives[fields, n];
	fieldsAndDerivs = FlattenFieldsAndDerivs[fieldsAndDerivs];
	Map[Select[#, Not@FreeQ[DEQ, #]&]&, fieldsAndDerivs]
];


DEQStructure[DEQs_?ListQ, fields_, OptionsPattern["MaxDerivativeOrder"->2]] := Block[{n},
	n = OptionValue["MaxDerivativeOrder"];
	Map[DEQStructure[#, fields, "MaxDerivativeOrder" -> n]&, DEQs]
]


(* ::Section::Closed:: *)
(*ExtractFunctions*)


ExtractFunctions[equation_, OptionsPattern["WhiteList"->{}]] := Block[{whiteList},
	whiteList = OptionValue["WhiteList"];
	If[whiteList == {},
		Cases[equation, f_[xs__]],
		Cases[equation, f_[xs__]/;NotFreesQ[f, whiteList]]]
];


(* ::Section::Closed:: *)
(*ShowDEQSystemStructure*)


(* https://codegolf.stackexchange.com/questions/4707/outputting-ordinal-numbers-1st-2nd-3rd *)
ordinalString[0] := "0th";
ordinalString[n_/;n>0] := Block[{p},
	Quiet@StringSplit[SpokenString[p[[n]]]][[2]]
]


DEQRowLabels[n_] := Map[StringJoin["DEQ ", ToString[#]]&, Range[n]];


DEQColLabel[col_] := StringJoin[ordinalString[col-1], " derivatives"];


DEQColLabels[n_] := Map[DEQColLabel, Range[n]];


ShowDEQSystemStructure[DEQs_, fields_, OptionsPattern["MaxDerivativeOrder"->2]]:= Block[
	{maxDeriv, rowLabels, colLabels, structure},
	maxDeriv = OptionValue["MaxDerivativeOrder"];
	rowLabels = DEQRowLabels[Length@DEQs];
	colLabels = DEQColLabels[maxDeriv+1];
	structure = DEQStructure[DEQs, fields, "MaxDerivativeOrder"->maxDeriv];
	Print@TableForm[structure, TableHeadings->{rowLabels,colLabels},
TableDepth->2, TableAlignments->Left]
]


(* ::Title::Closed:: *)
(*End Package*)


(* ::Text:: *)
(*Sets the functions of the package to be protected from changes.*)


Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["DEQSystemStructureVisualization`*"], Head[#] === Symbol &]];

End[];
EndPackage[];
