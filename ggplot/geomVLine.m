(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomVLine implementation *)
Options[geomVLine] = {"data" -> {}, "xIntercept" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "dashing" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomVLine[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := Module[{newDataset, groupbyKeys, colorFunc, thicknessFunc, alphaFunc, lineTypeFunc, output},

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
        (* Note: using 2 and 3 for case when Log scale functions are being used *)
        InfiniteLine[{{OptionValue["xScaleFunc"][OptionValue["xIntercept"]], OptionValue["yScaleFunc"][2]}, {OptionValue["xScaleFunc"][OptionValue["xIntercept"]], OptionValue["yScaleFunc"][3]}}]
      } &];

  output
];

End[];

EndPackage[];
