(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomVLine implementation *)
Options[geomVLine] = {"xIntercept" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "dashing" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomVLine[dataset_?ListQ, aesthetics : OptionsPattern[]] := Module[{groupbyKeys, colorFunc, thicknessFunc, alphaFunc, lineTypeFunc, output},

  (* Ensure X has been given *)
  If[OptionValue["xIntercept"] === Null, Message[ggplot::xInterceptNotGiven]; Throw[Null];];

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
    (* Note: using 2 and 3 for case when Log scale functions are being used *)
    InfiniteLine[{{OptionValue["xScaleFunc"][OptionValue["xIntercept"]], OptionValue["yScaleFunc"][2]}, {OptionValue["xScaleFunc"][OptionValue["xIntercept"]], OptionValue["yScaleFunc"][3]}}]
  } &] // Values;

  output
];

End[];

EndPackage[];
