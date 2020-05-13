(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomHistogram implementation *)

(* TODO: Need to review geomHistogram in R as there are probably some missing aesthetics and options here NEED TO ADD BINWIDTH etc. *)
Options[geomHistogram] = {"data" -> {}, "x" -> Null, "color" -> Null, "alpha" -> Null, "bspec" -> Automatic, "hspec" -> Automatic, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomHistogram[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := Module[{newDataset, groupbyKeys, rectangleCoordinates, output},
  (* Ensure X has been given *)
  If[OptionValue["x"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  newDataset = OptionValue["data"];

  (* Switch dates to absolute times *)
  newDataset = Replace[newDataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* For each key necessary, reconcile the aesthetics and append them to the dataset as a column name i.e. "color_aes" -> somecolor *)
  newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["alpha"], "alpha"];
  
  (* Group the data based on their aesthetic keys and then apply correct aesthetics while making a rectangle graphic primitive *)
  groupbyKeys = Function[{#["color_aes"], #["alpha_aes"]}];
  output = newDataset //
                GroupBy[groupbyKeys] //
                Values //
                Map[{
                  #[[1, "color_aes"]],
                  #[[1, "alpha_aes"]],
                  Rectangle@@@createRectangles[#[[All, OptionValue["x"]]], "bspec" -> OptionValue["bspec"], "hspec" -> OptionValue["hspec"], "xScaleFunc" -> OptionValue["xScaleFunc"], "yScaleFunc" -> OptionValue["yScaleFunc"]]
                } &];
              
  output

];

Options[createRectangles] = {"bspec" -> Automatic, "hspec" -> Automatic, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
createRectangles[values_, OptionsPattern[]] := Module[{cleanedValues, bins, heights, rectangleCoordinates},
  (* Clean the data in case of Log functions being passed in *)
  cleanedValues = values;
  If[OptionValue["xScaleFunc"] === Function[Log[#]], 
    cleanedValues = Cases[cleanedValues, x_/; NumericQ[Log[x]] && x != 0];
    ];
  
  (* Get the bins and heights for the rectangles on the histogram *)
  {bins, heights} = HistogramList[cleanedValues, OptionValue["bspec"], OptionValue["hspec"]];
  bins = OptionValue["xScaleFunc"] /@ bins;
  heights = OptionValue["yScaleFunc"] /@ heights;
  rectangleCoordinates = Map[Transpose]@Transpose[{Partition[bins, 2, 1], Map[{0, #} &, heights]}]; (* {{{x1, y1}, {x2, y2}} ...}*)
  rectangleCoordinates = Cases[rectangleCoordinates, {{x1_?NumericQ, y1_?NumericQ}, {x2_?NumericQ, y2_?NumericQ}}];
  rectangleCoordinates
];

End[];

EndPackage[];
