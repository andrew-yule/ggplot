(* :Title: ggplot *)
(* :Context: ggplot` *)
(* :Author: andrewyule *)
(* :Date: 2019-11-10 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2019 andrewyule *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["ggplot`"];
(* Exported symbols added here with SymbolName::usage *)

ggPlot::usage     = "TBD";
geomPoint::usage  = "TBD";

Begin["`Private`"];

ggPlotGraphics = Graphics;
SetOptions[ggPlotGraphics, Frame -> True, PlotRange -> All, GridLines -> Automatic, AspectRatio -> 7/10, PlotRangeClipping -> True];

ggPlot[dataset_, geoms:(geomPoint[__] | {geomPoint[__]..}), opts : OptionsPattern[]] := Module[{graphic},
  graphic = Cases[geoms, geomPoint[aesthetics__]:> geomPoint[dataset, aesthetics], {0, Infinity}] // ggPlotGraphics[#, FilterRules[{opts}, Options[Graphics]]]&;
  graphic
];

isDiscreteDataQ[data_] := If[MatchQ[DeleteDuplicates[data], {_?StringQ ..}], True, False];
getDiscreteKeys[data_] := Sort[DeleteDuplicates[data]];
getContinuousRange[data_] := MinMax[data];

(* Functions to determine aesthetics *)

ggplotColorsFunc[1] := Black;
ggplotColorsFunc[numberOfSeries_?IntegerQ] /; numberOfSeries > 1 := Drop[LCHColor[0.65, 0.6, #] & /@ (Subdivide[30, 390, numberOfSeries]/390), -1];
ggplotColorsFunc[___] := ggplotColorsFunc[1];

determineColorFunc[dataset_, key_] := Module[{data, colorFunc, discreteDataQ, keys, minMax},
  data = dataset[[All, key]];
  colorFunc = Function[Black];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = getDiscreteKeys[data];
    colorFunc = AssociationThread[keys -> ggplotColorsFunc[Length[keys]]];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    colorFunc = With[{minMax = minMax}, Function[x, Blend[{Red, Blue}, Rescale[x, minMax]]]]
  ];
  colorFunc
];
determineColorFunc[dataset_, Null] := Function[Black];

determineSizeFunc[dataset_, key_] := Module[{data, sizeFunc, discreteDataQ, keys, minMax},
  data = dataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  (* TODO: We don't really care about discrete data for the size (I think??)*)
  If[discreteDataQ,
    sizeFunc = Function[10];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    sizeFunc = With[{minMax = minMax}, Function[x, Rescale[x, minMax, {5, 20}]]];
  ];
  sizeFunc
];
determineSizeFunc[dataset_, Null] := Function[10];

determineAlphaFunc[] := Function[Opacity[1]];
determineShapeFunc[] := Function["\[FilledCircle]"];

(* geomPoint implementation *)

Options[geomPoint] = {"color" -> Null, "size" -> Null, "alpha" -> Null, "shape" -> Null};
geomPoint[dataset_, "x" -> xKey_, "y" -> yKey_, optionalAesthetics : OptionsPattern[]] := Module[{colorFunc, sizeFunc, alphaFunc, shapeFunc, output},
  (* Gather keys that are needed (should be smart enough to recognize a key in the dataset) *)  (* For each key necessary, get functions to be used to specify the aesthetic *)
  colorFunc = determineColorFunc[dataset, OptionValue["color"]];
  sizeFunc = determineSizeFunc[dataset, OptionValue["size"]];
  alphaFunc = determineAlphaFunc[];
  shapeFunc = determineShapeFunc[];

  (*Grab the point data and for each Point apply the correct aesthetic*)
  output = dataset // Map[{
    colorFunc[#[OptionValue["color"]]],
    alphaFunc[#[OptionValue["alpha"]]],
    Inset[Style[shapeFunc[#[OptionValue["shape"]]], sizeFunc[#[OptionValue["size"]]]], {#[xKey], #[yKey]}]
  } &];
  (*
   Group the data by similar aesthetics and then just apply to the first one so we reduce the memory size
   of the output (since graphics will apply the same forms to following expressions)
  *)
  output = output // GroupBy[Most -> Last] // Normal // ReplaceAll[Rule -> List] // Flatten;
  output
];

End[]; (* `Private` *)

EndPackage[]