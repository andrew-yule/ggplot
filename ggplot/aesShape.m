(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if shape is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "shape"] := Function["\[FilledCircle]"];

(* If an aesthetic is used but does not match a key, then assume it's a user directly specifying what they want. *)
(* Users can either specify a function directly or some kind of return value for what function i.e. 'Black' or 'Function[Black]' *)
reconcileAesthetics[dataset_, func_Function, "shape"] := func;
reconcileAesthetics[dataset_, val_, "shape"] /; !keyExistsQAll[dataset, val] := Function[val];

(* More complex logic if aesthetic is used and mapped to a key *)
reconcileAesthetics[dataset_, key_, "shape"] := Module[{data, func, discreteDataQ, keys, minMax},
  data = dataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    func = If[Length[keys] > 5, Message[ggplot::shapeCount]; Throw[Null];, Function[AssociationThread[keys, Take[{"\[FilledCircle]", "\[FilledUpTriangle]", "\[FilledSquare]", "\[FivePointedStar]", "\[FilledDiamond]"}, Length[keys]]][#]]];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    func = Message[ggplot::shapeContinuous]; Throw[Null];
  ];
  func
];

End[];

EndPackage[];
