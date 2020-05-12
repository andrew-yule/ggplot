(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if shape is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "shape"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "shape_aes" -> "\[FilledCircle]"] &];
  newDataset
];

(* If shape is given as an actual shape, then assume that's the shape the user wants everything to be *)
reconcileAesthetics[dataset_, shape_Symbol, "shape"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "shape_aes" -> shape] &];
  newDataset
];

(* If a string is passed in, then assume that's the key in the dataset on how to shape the data. Then must determine whether the data is discrete or not *)
reconcileAesthetics[dataset_, key_?StringQ, "shape"] /; keyExistsQAll[dataset, key] := Module[{newDataset, data, shapeFunc, discreteDataQ, keys, minMax},
  newDataset = dataset;
  data = newDataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    shapeFunc = Function[AssociationThread[keys, shapesFunc[Length[keys]]]];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    shapeFunc = Message[ggplot::shapeContinuous]; Throw[Null];
  ];
  newDataset = newDataset // Map[Append[#, "shape_aes" -> shapeFunc[#[key]]] &];
  newDataset
];

(* If a function is passed in, then use it to determine how to shape the data assuming the function will be applied "row-wise" to the dataset, as an example "shape" -> Function[#somegroup < 10] *)
reconcileAesthetics[dataset_, func_Function, "shape"] := Module[{newDataset, groupedDataset, shapes},
  newDataset = dataset;
  groupedDataset = GroupBy[dataset, func] // KeySort;
  shapes = shapesFunc[Length[groupedDataset]];
  newDataset = groupedDataset // Values // MapIndexed[Function[{group, index}, Map[Function[row, Append[row, "shape_aes" -> shapes[[First@index]]]], group]]] // Flatten;
  newDataset
];

shapesFunc[numberOfSeries_?IntegerQ] /; Between[numberOfSeries, {1, 7}] := {"\[FilledCircle]", "\[FilledUpTriangle]", "\[FilledSquare]", "\[FivePointedStar]", "\[FilledDiamond]", "\[FilledRectangle]", "\[FilledDownTriangle]"}[[1;;numberOfSeries]];
shapesFunc[numberOfSeries_?IntegerQ] /; numberOfSeries > 7 := (Message[ggplot::shapeCount]; Throw[Null];)

End[];

EndPackage[];
