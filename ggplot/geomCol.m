(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

Options[geomCol] = {"x" -> Null, "y" -> Null, "color" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomCol[dataset_?ListQ, aesthetics : OptionsPattern[]] := Module[{newDataset, groupbyKeys, colorFunc, output},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  newDataset = dataset;

  (* For each key necessary, reconcile the aesthetics and append them to the dataset as a column name i.e. "color_aes" -> somecolor *)
  newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];

  (*Grab the rectangles and for each apply the correct aesthetic*)
  output = newDataset // Map[{
    #["color_aes"],
    Rectangle[{#[OptionValue["x"]] - 0.05, 0}, {#[OptionValue["x"]] + 0.05 , #[OptionValue["y"]]}]
  } &];

  (*output = output // ReverseSortBy[#[[&]*)

  (* TODO: Add a group function to remove duplicated primitives*)
  output
];

End[];

EndPackage[];
