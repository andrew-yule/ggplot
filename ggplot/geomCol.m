(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];
(* Exported symbols added here with SymbolName::usage *)


Begin["`Private`"];

Options[geomCol] = {"x" -> Null, "y" -> Null, "color" -> Null};
geomCol[dataset_?ListQ, aesthetics : OptionsPattern[]] := Module[{groupbyKeys, colorFunc, output},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  (* For each key necessary, get functions to be used to specify the aesthetic *)
  colorFunc = reconcileAesthetics[dataset, OptionValue["color"], "color"];

  (*Grab the point data and for each Point apply the correct aesthetic*)
  output = dataset // Map[{
    colorFunc[#[OptionValue["color"]]],
    Rectangle[{#[OptionValue["x"]] - 0.05, 0}, {#[OptionValue["x"]] + 0.05 , #[OptionValue["y"]]}]
  } &];

  (*output = output // ReverseSortBy[#[[&]*)

  (* TODO: Add a group function to remove duplicated primitives*)
  output
];

End[];

EndPackage[];
