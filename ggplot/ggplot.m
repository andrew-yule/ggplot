(* :Title: ggplot *)
(* :Context: ggplot` *)
(* :Author: andrewyule *)
(* :Date: 2019-11-10 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

ggplot::xOrYNotGiven        = "A geom was given without specifying the x or y mapping";
ggplot::xInterceptNotGiven  = "No xIntercept value was given for geomHLine";
ggplot::yInterceptNotGiven  = "No yIntercept value was given for geomHLine";
ggplot::shapeContinuous     = "A continuous variable can not be mapped to a shape";
ggplot::shapeCount          = "More than 5 discrete shapes are present, aborting... (this should be fixed)";

validDatasetQ[dataset_] := MatchQ[dataset, {_?AssociationQ..}];

geomsAndScalesPattern = (geomPoint[___] | geomLine[___] | geomSmooth[___] | geomParityLine[___] | geomHLine[___] | geomVLine[___] | geomCol[___]) ...;

(* Main ggplot method and entry point *)
Options[ggplot] = Join[{"data" -> {}}, Options[ListLinePlot], Options[ticks], Options[gridLines](*, Options[Alex`Plotting`linearGridLines],*) (*{DateTicksFormat -> Automatic}*)];
SetOptions[ggplot,
  ImageSize -> 400, AspectRatio -> 7/10, Frame -> True, Axes -> False,
  ImageMargins -> Automatic,
  LabelStyle -> Directive[12, FontFamily -> "Arial"],
  FrameStyle -> Directive[GrayLevel[0.6], Thickness[0.0008`]],
  FrameTicksStyle -> Directive[Black, Opacity[1]],
  FrameTicks -> Automatic, GridLines -> Automatic,  Background -> White,
  PlotRange -> All, PlotRangeClipping -> True
];
Attributes[ggplot] = {HoldAllComplete};
(*ggplot[dataset_?validDatasetQ, geomsAndScales : geomsAndScalesPattern, options: OptionsPattern[]] := ggplot[geomsAndScales, "data" -> dataset, options];*)
(*ggplot[geomsAndScales : geomsAndScalesPattern, options: OptionsPattern[]][dataset_?validDatasetQ] := ggplot[geomsAndScales, "data" -> dataset, options];*)
ggplot[args___] := Catch[Module[{rules, dataset, x, y, points, lines, smoothLines, columns, abLines, hLines, vLines, graphicsPrimitives, xScaleType, yScaleType, xScaleFunc, yScaleFunc, xTickFunc, yTickFunc, xGridLineFunc, yGridLineFunc, graphic},

  rules = Cases[Hold@{args}, _Rule, {2}];

  dataset = Lookup[rules, "data", {}];
  x = Lookup[rules, "x", Null];
  y = Lookup[rules, "y", Null];

  (* Switch dates to absolute times *)
  dataset = Replace[dataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* Get all scaling information *)
  xScaleType = reconcileXScales[Hold@{args}]; (* returns Linear / Date / Log / Log10 / Log2 *)
  yScaleType = reconcileYScales[Hold@{args}]; (* returns Linear / Date / Log / Log10 / Log2 *)

  (* Creating scaling functions to use for x and y *)
  xScaleFunc = With[{f = ToExpression[xScaleType /. "Linear" | "Date" -> "Identity"]}, Function[f[#]]];
  yScaleFunc = With[{f = ToExpression[yScaleType /. "Linear" | "Date" -> "Identity"]}, Function[f[#]]];

  (* Compile all geom information *)
  points      = Cases[Hold@{args}, geomPoint[opts___]      :> geomPoint[opts,      "data" -> dataset, "x" -> x, "y" -> y, "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  lines       = Cases[Hold@{args}, geomLine[opts___]       :> geomLine[opts,       "data" -> dataset, "x" -> x, "y" -> y, "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  smoothLines = Cases[Hold@{args}, geomSmooth[opts___]     :> geomSmooth[opts,     "data" -> dataset, "x" -> x, "y" -> y, "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  abLines     = Cases[Hold@{args}, geomParityLine[opts___] :> geomParityLine[opts, "data" -> dataset, "x" -> x, "y" -> y, "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  hLines      = Cases[Hold@{args}, geomHLine[opts___]      :> geomHLine[opts,      "data" -> dataset, "x" -> x, "y" -> y, "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  vLines      = Cases[Hold@{args}, geomVLine[opts___]      :> geomVLine[opts,      "data" -> dataset, "x" -> x, "y" -> y, "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  (* columns need a lot more work to sort through *)
  (*columns     = Cases[{geoms}, geomCol[aesthetics__] :> geomCol[dataset, aesthetics, "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];*)

  graphicsPrimitives = {points, lines, smoothLines, abLines, hLines, vLines} // Flatten;

  (* Tick / GridLine functions passed into ggplot FrameTicks -> _ call *)
  With[{tickOptions = FilterRules[{rules}, Options[ticks]], gridLineOptions = FilterRules[{rules}, Options[gridLines]]},
    xTickFunc = Function[{min, max}, ticks[xScaleType, min, max, tickOptions]];
    yTickFunc = Function[{min, max}, ticks[yScaleType, min, max, tickOptions]];

    xGridLineFunc = Function[{min, max}, gridLines[xScaleType, min, max, gridLineOptions]];
    yGridLineFunc = Function[{min, max}, gridLines[yScaleType, min, max, gridLineOptions]]
  ];

  graphic = Graphics[graphicsPrimitives, Frame -> True, AspectRatio -> 0.7(*,
    FrameLabel ->
        If[MatchQ[OptionValue[FrameLabel], {{_?StringQ, _?StringQ}, {_?StringQ, _?StringQ}} | {_?StringQ, _?StringQ} | _?StringQ],
          (OptionValue[FrameLabel] /. str_?StringQ :> Style[str, Opacity[1], FontColor -> Black]),
          OptionValue[FrameLabel]
        ],
    PlotStyle -> OptionValue[PlotStyle],
    ImageSize -> OptionValue[ImageSize],
    AspectRatio -> OptionValue[AspectRatio],
    Frame -> OptionValue[Frame],
    Axes -> OptionValue[Axes],
    LabelStyle -> OptionValue[LabelStyle],
    FrameStyle -> OptionValue[FrameStyle],
    FrameTicksStyle -> OptionValue[FrameTicksStyle],
    FrameTicks -> If[OptionValue[FrameTicks] === Automatic,
      {{yTickFunc, False}, {xTickFunc, False}},
      OptionValue[FrameTicks]
    ],
    GridLines -> If[OptionValue[GridLines] === Automatic,
      {xGridLineFunc, yGridLineFunc},
      OptionValue[GridLines]
    ],
    GridLinesStyle -> Automatic, (* shouldn't need this but do for some reason *)
    Background -> OptionValue[Background],
    ImageMargins -> OptionValue[ImageMargins],
    PlotRangeClipping -> OptionValue[PlotRangeClipping],
    FilterRules[{rules}, Options[ListPlot]]*)
  ];

  graphic
]];
(*ggplot[geomsAndScales___, options : OptionsPattern[]][dataset_?validDatasetQ] := ggplot["data" -> dataset, geomsAndScales, options];*)

End[];

EndPackage[]