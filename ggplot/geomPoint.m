(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomPoint implementation *)

Options[geomPoint] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "size" -> Null, "alpha" -> Null, "shape" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomPoint[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := Module[{newDataset, colorFunc, sizeFunc, alphaFunc, shapeFunc, output},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  newDataset = OptionValue["data"];

  (* Switch dates to absolute times *)
  newDataset = Replace[newDataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* For each key necessary, reconcile the aesthetics and append them to the dataset as a column name i.e. "color_aes" -> somecolor *)
  newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["size"], "size"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["alpha"], "alpha"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["shape"], "shape"];

  (*Grab the point data and for each Point apply the correct aesthetic*)
  output = newDataset // Map[{
    #["color_aes"],
    #["alpha_aes"],
    Inset[Style[#["shape_aes"], #["size_aes"]], {OptionValue["xScaleFunc"][#[OptionValue["x"]]], OptionValue["yScaleFunc"][#[OptionValue["y"]]]}]
  } &];

  (* Grouping data but doing a GeometricTransformation on similar Inset values to speed up the plotting once inside Graphics *)
  output = output // GroupBy[Function[{#[[1]], #[[2]], Inset[#[[3, 1]], {0, 0}]}] -> Function[#[[3, 2]]]] // Normal // Map[{#[[1, 1]], #[[1, 2]], GeometricTransformation[#[[1, 3]], List /@ #[[2]]]} &];
  output

];

End[];

EndPackage[];
