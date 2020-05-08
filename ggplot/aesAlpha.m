(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if alpha is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "alpha"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "alpha_aes" -> Opacity[1]] &];
  newDataset
];

(* If alpha is given as an actual alpha, then assume that's the alpha the user wants everything to be *)
reconcileAesthetics[dataset_, alpha_Opacity, "alpha"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "alpha_aes" -> alpha] &];
  newDataset
];

(* If a string is passed in, then assume that's the key in the dataset on how to alpha the data. Then must determine whether the data is discrete or not *)
reconcileAesthetics[dataset_, key_?StringQ, "alpha"] /; keyExistsQAll[dataset, key] := Module[{newDataset, data, alphaFunc, discreteDataQ, keys, minMax},
  newDataset = dataset;
  data = newDataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    alphaFunc = Function[Opacity[AssociationThread[keys, Subdivide[0.1, 1, Length[keys] - 1]][#]]];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    alphaFunc = With[{minMax = minMax}, Function[Opacity[Rescale[#, minMax, {0.1, 1}]]]];
  ];
  newDataset = newDataset // Map[Append[#, "alpha_aes" -> alphaFunc[#[key]]] &];
  newDataset
];

(* If a function is passed in, then use it to determine how to alpha the data assuming the function will be applied "row-wise" to the dataset, as an example "alpha" -> Function[#somegroup < 10] *)
reconcileAesthetics[dataset_, func_Function, "alpha"] := Module[{newDataset, groupedDataset, alphas},
  newDataset = dataset;
  groupedDataset = GroupBy[dataset, func] // KeySort;
  alphas = Range[Length[groupedDataset]] // Map[Opacity[Rescale[#, {1, Length[groupedDataset]}, {0.1, 1}]] &];
  newDataset = groupedDataset // Values // MapIndexed[Function[{group, index}, Map[Function[row, Append[row, "alpha_aes" -> alphas[[First@index]]]], group]]] // Flatten;
  newDataset
];

reconcileAesthetics[dataset_, _, "alpha"] := Throw[Echo["Unclear on how to determine the alpha"];Null];

End[];

EndPackage[];
