(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];
(* Exported symbols added here with SymbolName::usage *)


Begin["`Private`"];

isDiscreteDataQ[data_]    := If[MatchQ[DeleteDuplicates[data], {_?StringQ ..}], True, False];
getAllKeys[data_]         := data // Keys /* Flatten /* DeleteDuplicates;
getDiscreteKeys[data_]    := Sort[DeleteDuplicates[data]];
getContinuousRange[data_] := MinMax[data];

(* Functions to determine aesthetics *)

(* Default values if not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "color"]     := Function[Black];
reconcileAesthetics[dataset_, Null, "size"]      := Function[10];
reconcileAesthetics[dataset_, Null, "alpha"]     := Function[Opacity[1]];
reconcileAesthetics[dataset_, Null, "shape"]     := Function["\[FilledCircle]"];
reconcileAesthetics[dataset_, Null, "thickness"] := Function[Thick];

(* If an aesthetic is used but does not match a key, then assume it's a user directly specifying *)
reconcileAesthetics[dataset_, val_Function, aes_]  := val;
(* Note there is a bug with KeyExists as it won't accept Key["string"] in order to see if the key does exist *)
(*reconcileAesthetics[dataset_, val_, aes_] /; !(dataset // Map[KeyExistsQ[Key[val]]] /* ContainsAny[{True}]) := Function[val];*)
(* Using Lookup instead *)
reconcileAesthetics[dataset_, val_, aes_] /; (dataset // Map[Lookup[#, Key[val], False] &] /* ContainsAny[{False}]) := Function[val];

(* More complex logic if aesthetic is used and mapped to a key *)
reconcileAesthetics[dataset_, key_, aes_] := Module[{data, func, discreteDataQ, keys, minMax},
  data = dataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    func = Switch[aes,
      "color",      Function[AssociationThread[keys, ggplotColorsFunc[Length[keys]]][#]],
      "size",       Function[AssociationThread[keys, Subdivide[10, 25, Length[keys] - 1]][#]],
      "alpha",      Function[Opacity[AssociationThread[keys, Subdivide[0.1, 1, Length[keys] - 1]][#]]],
      "shape",      If[Length[keys] > 5, Message[ggplot::shapecount]; Throw[Null];, Function[AssociationThread[keys, Take[{"\[FilledCircle]", "\[FilledUpTriangle]", "\[FilledSquare]", "\[FivePointedStar]", "\[FilledDiamond]"}, Length[keys]]][#]]],
      "thickness",  Function[Thick]
    ];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    func = Switch[aes,
      "color",      With[{minMax = minMax}, Function[Blend[{Red, Blue}, Rescale[#, minMax]]]],
      "size",       With[{minMax = minMax}, Function[x, Rescale[x, minMax, {10, 25}]]],
      "alpha",      With[{minMax = minMax}, Function[Opacity[Rescale[#, minMax, {0.1, 1}]]]],
      "shape",      Message[ggplot::shapecontinuous]; Throw[Null];,
      "thickness",  Function[Thick]
    ];
  ];
  func
];

(* Helper functions for colors *)
ggplotColorsFunc[1] := Black;
ggplotColorsFunc[numberOfSeries_?IntegerQ] /; numberOfSeries > 1 := Drop[LCHColor[0.65, 0.6, #] & /@ (Subdivide[30, 390, numberOfSeries]/390), -1];
ggplotColorsFunc[___] := ggplotColorsFunc[1];

(* TODO: Implement actual logic for these functions below *)

determineThicknessFunc[___] := Function[Thick];
(* determineLineTypeFunc[] := Function[Dashing[1]]; *) (* Need to disable for now as Dashing[1] causes a known Graphics issue with lines flickering. Bug has been reported to Wolfram. *)

End[];

EndPackage[];
