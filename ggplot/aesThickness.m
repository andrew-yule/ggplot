(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if thickness is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "thickness"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "thickness_aes" -> Thick] &];
  newDataset
];

(* If thickness is given as an actual thickness, then assume that's the thickness the user wants everything to be *)
reconcileAesthetics[dataset_, thickness_Thickness, "thickness"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "thickness_aes" -> thickness] &];
  newDataset
];

(* TODO: Add more logic into handling of thickness *)
(* If a string is passed in, then assume that's the key in the dataset on how to thickness the data. Then must determine whether the data is discrete or not *)
reconcileAesthetics[dataset_, key_?StringQ, "thickness"] /; keyExistsQAll[dataset, key] := Module[{newDataset, data, thicknessFunc, discreteDataQ, keys, minMax},
  newDataset = dataset;
  data = newDataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    thicknessFunc = Function[Thick];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    thicknessFunc = Function[Thick];
  ];
  newDataset = newDataset // Map[Append[#, "thickness_aes" -> thicknessFunc[#[key]]] &];
  newDataset
];

(* TODO: Add more logic into handling of thickness *)
(* If a function is passed in, then use it to determine how to thickness the data assuming the function will be applied "row-wise" to the dataset, as an example "thickness" -> Function[#somegroup < 10] *)
reconcileAesthetics[dataset_, func_Function, "thickness"] := Module[{newDataset, groupedDataset, thicknesss},
  newDataset = dataset;
  groupedDataset = GroupBy[dataset, func] // KeySort;
  thicknesss = ConstantArray[Thick, Length[groupedDataset]];
  newDataset = groupedDataset // Values // MapIndexed[Function[{group, index}, Map[Function[row, Append[row, "thickness_aes" -> thicknesss[[First@index]]]], group]]] // Flatten;
  newDataset
];

reconcileAesthetics[dataset_, _, "thickness"] := Throw[Echo["Unclear on how to determine the thickness"];Null];

End[];

EndPackage[];
