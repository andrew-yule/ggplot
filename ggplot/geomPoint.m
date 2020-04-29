(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomPoint implementation *)

Options[geomPoint] = {"x" -> Null, "y" -> Null, "color" -> Null, "size" -> Null, "alpha" -> Null, "shape" -> Null};
geomPoint[dataset_?ListQ, aesthetics : OptionsPattern[]] := Module[{colorFunc, sizeFunc, alphaFunc, shapeFunc, output},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  (* For each key necessary, get functions to be used to specify the aesthetic *)
  colorFunc = reconcileAesthetics[dataset, OptionValue["color"], "color"];
  sizeFunc  = reconcileAesthetics[dataset, OptionValue["size"], "size"];
  alphaFunc = reconcileAesthetics[dataset, OptionValue["alpha"], "alpha"];
  shapeFunc = reconcileAesthetics[dataset, OptionValue["shape"], "shape"];

  (*Grab the point data and for each Point apply the correct aesthetic*)
  output = dataset // Map[{
    colorFunc[#[OptionValue["color"]]],
    alphaFunc[#[OptionValue["alpha"]]],
    Inset[Style[shapeFunc[#[OptionValue["shape"]]], sizeFunc[#[OptionValue["size"]]]], {#[OptionValue["x"]], #[OptionValue["y"]]}]
  } &];

  (* Grouping data but doing a GeometricTransformation on similar Inset values to speed up the plotting once inside Graphics *)
  output = output // GroupBy[Function[{#[[1]], #[[2]], Inset[#[[3, 1]], {0, 0}]}] -> Function[#[[3, 2]]]] // Normal // Map[{#[[1, 1]], #[[1, 2]], GeometricTransformation[#[[1, 3]], List /@ #[[2]]]} &];
  output

];

End[];

EndPackage[];
