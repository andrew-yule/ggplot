(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if color is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "color"]  := Function[Black];

(* If an aesthetic is used but does not match a key, then assume it's a user directly specifying what they want. *)
(* Users can either specify a function directly or some kind of return value for what function i.e. 'Black' or 'Function[Black]' *)
reconcileAesthetics[dataset_, func_Function, "color"] := func;
reconcileAesthetics[dataset_, val_, "color"] /; !keyExistsQAll[dataset, val] := Function[val];

(* More complex logic if aesthetic is used and mapped to a key *)
reconcileAesthetics[dataset_, key_, "color"] := Module[{data, func, discreteDataQ, keys, minMax},
  data = dataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    func = Function[AssociationThread[keys, ggplotColorsFunc[Length[keys]]][#]];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    func = With[{minMax = minMax}, Function[Blend[{Red, Blue}, Rescale[#, minMax]]]];
  ];
  func
];

(* Helper functions for colors *)
ggplotColorsFunc[1] := Black;
ggplotColorsFunc[numberOfSeries_?IntegerQ] /; numberOfSeries > 1 := Drop[LCHColor[0.65, 0.6, #] & /@ (Subdivide[30, 390, numberOfSeries]/390), -1];
ggplotColorsFunc[___] := ggplotColorsFunc[1];

End[];

EndPackage[];
