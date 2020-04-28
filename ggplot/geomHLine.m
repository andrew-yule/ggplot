(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"];

(* geomHLine implementation *)
Options[geomHLine] = {"y" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "dashing" -> Null};
geomHLine[dataset_?ListQ, aesthetics : OptionsPattern[]] := Module[{groupbyKeys, colorFunc, thicknessFunc, alphaFunc, lineTypeFunc, output},

  (* Ensure Y has been given *)
  If[OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  (* For each key necessary, get functions to be used to specify the aesthetic *)
  colorFunc     = reconcileAesthetics[dataset, OptionValue["color"], "color"];
  alphaFunc     = reconcileAesthetics[dataset, OptionValue["alpha"], "alpha"];
  thicknessFunc = reconcileAesthetics[dataset, OptionValue["thickness"], "thickness"];
  (*dashingFunc = reconcileAesthetics[dataset, OptionValue["dashing"], "dashing"];*)

  (* Group the data and apply correct aesthetics while making a line primitive *)
  groupbyKeys = DeleteCases[{OptionValue["color"], OptionValue["thickness"], OptionValue["alpha"], OptionValue["thickness"], OptionValue["dashing"]}, Null];
  output = dataset // GroupBy[((# /@ groupbyKeys) &)] // Map[{
    colorFunc[Quiet@#[[1, OptionValue["color"]]]],
    alphaFunc[Quiet@#[[1, OptionValue["alpha"]]]],
    thicknessFunc[Quiet@#[[1, OptionValue["thickness"]]]],
    (*lineTypeFunc[Quiet@#[[1, OptionValue["linetype"]]]],*)
    InfiniteLine[{{0, OptionValue["y"]}, {1, OptionValue["y"]}}]
  } &] // Values;

  output
];

End[];

EndPackage[];
