(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if color is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "color"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "color_aes" -> Black] &];
  newDataset
];

(* If color is given as an actual color, then assume that's the color the user wants everything to be *)
reconcileAesthetics[dataset_, color_?ColorQ, "color"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "color_aes" -> color] &];
  newDataset
];

(* If a string is passed in, then assume that's the key in the dataset on how to color the data. Then must determine whether the data is discrete or not *)
reconcileAesthetics[dataset_, key_?StringQ, "color"] /; keyExistsQAll[dataset, key] := Module[{newDataset, data, colorFunc, discreteDataQ, keys, minMax},
  newDataset = dataset;
  data = newDataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    colorFunc = Function[AssociationThread[keys, ggplotColorsFunc[Length[keys]]][#]];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    colorFunc = With[{minMax = minMax}, Function[Blend[{Red, Blue}, Rescale[#, minMax]]]];
  ];
  newDataset = newDataset // Map[Append[#, "color_aes" -> colorFunc[#[key]]] &];
  newDataset
];

(* If a function is passed in, then use it to determine how to color the data assuming the function will be applied "row-wise" to the dataset, as an example "color" -> Function[#somegroup < 10] *)
reconcileAesthetics[dataset_, func_Function, "color"] := Module[{newDataset, groupedDataset, colors},
  newDataset = dataset;
  groupedDataset = GroupBy[dataset, func] // KeySort;
  colors = ggplotColorsFunc[Length[groupedDataset]];
  newDataset = groupedDataset // Values // MapIndexed[Function[{group, index}, Map[Function[row, Append[row, "color_aes" -> colors[[First@index]]]], group]]] // Flatten;
  newDataset
];

reconcileAesthetics[dataset_, _, "color"] := Throw[Echo["Unclear on how to determine the color"];Null];

(* Helper functions for colors *)
ggplotColorsFunc[1] := {Black};
ggplotColorsFunc[numberOfSeries_?IntegerQ] /; numberOfSeries > 1 := Drop[LCHColor[0.65, 0.6, #] & /@ (Subdivide[30, 390, numberOfSeries]/390), -1];
ggplotColorsFunc[___] := ggplotColorsFunc[1];

End[];

EndPackage[];
