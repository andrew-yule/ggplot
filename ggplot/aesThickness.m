(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Default function if thickness is not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "thickness"] := Function[Thick]; (* Need to disable any Dashing function for now as Dashing[1] causes a known Graphics issue with lines flickering. Bug has been reported to Wolfram. *)

(* If an aesthetic is used but does not match a key, then assume it's a user directly specifying what they want. *)
(* Users can either specify a function directly or some kind of return value for what function i.e. 'Black' or 'Function[Black]' *)
reconcileAesthetics[dataset_, func_Function, "thickness"] := func;
reconcileAesthetics[dataset_, val_, "thickness"] /; !keyExistsQAll[dataset, val] := Function[val];

(* TODO: Add more logic into handling of thickness *)
(* More complex logic if aesthetic is used and mapped to a key *)
reconcileAesthetics[dataset_, key_, "thickness"] := Module[{data, func, discreteDataQ, keys, minMax},
  data = dataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    func = Function[Thick];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    func = Function[Thick];
  ];
  func
];

End[];

EndPackage[];
