(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if alpha is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "alpha"]  := Function[Opacity[1]];

(* If an aesthetic is used but does not match a key, then assume it's a user directly specifying what they want. *)
(* Users can either specify a function directly or some kind of return value for what function i.e. 'Black' or 'Function[Black]' *)
reconcileAesthetics[dataset_, func_Function, "alpha"] := func;
reconcileAesthetics[dataset_, val_, "alpha"] /; !keyExistsQAll[dataset, val] := Function[val];

(* More complex logic if aesthetic is used and mapped to a key *)
reconcileAesthetics[dataset_, key_, "alpha"] := Module[{data, func, discreteDataQ, keys, minMax},
  data = dataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    func = Function[Opacity[AssociationThread[keys, Subdivide[0.1, 1, Length[keys] - 1]][#]]];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    func = With[{minMax = minMax}, Function[Opacity[Rescale[#, minMax, {0.1, 1}]]]];
  ];
  func
];

End[];

EndPackage[];
