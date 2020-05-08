(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if size is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "size"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "size_aes" -> 10] &];
  newDataset
];

(* If size is given as an actual size, then assume that's the size the user wants everything to be *)
reconcileAesthetics[dataset_, size_?NumericQ, "size"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "size_aes" -> size] &];
  newDataset
];

(* If a string is passed in, then assume that's the key in the dataset on how to size the data. Then must determine whether the data is discrete or not *)
reconcileAesthetics[dataset_, key_?StringQ, "size"] /; keyExistsQAll[dataset, key] := Module[{newDataset, data, sizeFunc, discreteDataQ, keys, minMax},
  newDataset = dataset;
  data = newDataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    sizeFunc = Function[AssociationThread[keys, Subdivide[10, 25, Length[keys] - 1]][#]];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    sizeFunc = With[{minMax = minMax}, Function[x, Rescale[x, minMax, {10, 25}]]];
  ];
  newDataset = newDataset // Map[Append[#, "size_aes" -> sizeFunc[#[key]]] &];
  newDataset
];

(* If a function is passed in, then use it to determine how to size the data assuming the function will be applied "row-wise" to the dataset, as an example "size" -> Function[#somegroup < 10] *)
reconcileAesthetics[dataset_, func_Function, "size"] := Module[{newDataset, groupedDataset, sizes},
  newDataset = dataset;
  groupedDataset = GroupBy[dataset, func] // KeySort;
  sizes = Range[Length[groupedDataset]] // Map[Rescale[#, {1, Length[groupedDataset]}, {10, 25}] &];
  newDataset = groupedDataset // Values // MapIndexed[Function[{group, index}, Map[Function[row, Append[row, "size_aes" -> sizes[[First@index]]]], group]]] // Flatten;
  newDataset
];

reconcileAesthetics[dataset_, _, "size"] := Throw[Echo["Unclear on how to determine the size"];Null];

End[];

EndPackage[];
