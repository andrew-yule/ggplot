(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomSmooth implementation *)

Options[geomSmooth] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "dashing" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomSmooth[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := Module[{newDataset, groupbyKeys, colorFunc, thicknessFunc, alphaFunc, lineTypeFunc, output},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  newDataset = OptionValue["data"];

  (* Switch dates to absolute times *)
  newDataset = Replace[newDataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* For each key necessary, reconcile the aesthetics and append them to the dataset as a column name i.e. "color_aes" -> somecolor *)
  newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["alpha"], "alpha"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["thickness"], "thickness"];
  (*newDataset = reconcileAesthetics[newDataset, OptionValue["dashing"], "dashing"];*) (* bug here with Dashing and Graphics that has been reported to Wolfram *)

  (* Group the data based on their aesthetic keys and then apply correct aesthetics while making a line primitive *)
  groupbyKeys = Function[{#["color_aes"], #["alpha_aes"], #["thickness_aes"]}];
  output =  newDataset //
      GroupBy[groupbyKeys] //
      Values //
      Map[{
        #[[1, "color_aes"]],
        #[[1, "alpha_aes"]],
        #[[1, "thickness_aes"]],
        (* IMPORTANT NOTE: according to ggplot2 in R, statistics transforms occur AFTER the scaling changes *)
        Line@lmFit@Map[Function[point, {OptionValue["xScaleFunc"]@point[[1]], OptionValue["yScaleFunc"]@point[[2]]}]]@Sort@Transpose[{#[[All, OptionValue["x"]]], #[[All, OptionValue["y"]]]}]
      } &];

  output
];

lmFit[data_] := Block[{x, f, results},
  f = LinearModelFit[data, x, x];
  results = Table[{x, f[x]}, {x, data[[All, 1]]}];
  results
];

End[];

EndPackage[];
