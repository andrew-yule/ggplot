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
ggplot::shapeCount          = "More than 7 discrete shapes are present, aborting... (this should be fixed)";

validDatasetQ[dataset_] := MatchQ[dataset, {_?AssociationQ..}];

Attributes[argPatternQ] = {HoldAllComplete};
argPatternQ[expr___] := MatchQ[Hold[expr], Hold[(_Rule | geomPoint[___] | geomLine[___] | geomSmooth[___] | geomVLine[___] | geomHLine[___] | geomParityLine[___] | geomCol[___] | scaleXDate[___] | scaleXLinear[___] | scaleXLog[___] | scaleYDate[___] | scaleYLinear[___] | scaleYLog[___]) ...]];

(* Main ggplot method and entry point *)
Options[ggplot] = Join[{"data" -> {}}, Options[ListLinePlot], Options[ticks], Options[gridLines] (*{DateTicksFormat -> Automatic}*)];
SetOptions[ggplot,
  ImageSize -> 400, AspectRatio -> 7/10, Frame -> True, Axes -> False,
  ImageMargins -> Automatic,
  LabelStyle -> Directive[12, FontFamily -> "Arial"],
  FrameLabel -> Automatic,
  FrameStyle -> Directive[GrayLevel[0.6], Thickness[0.0008`]],
  FrameTicksStyle -> Directive[Black, Opacity[1]],
  FrameTicks -> Automatic, GridLines -> Automatic,  Background -> White,
  PlotRange -> All, PlotRangeClipping -> True
];
Attributes[ggplot] = {HoldAllComplete};
ggplot[ds_?validDatasetQ, args___?argPatternQ] := ggplot["data" -> ds, args];
ggplot[args___?argPatternQ][ds_?validDatasetQ] := ggplot["data" -> ds, args];
ggplot[args___?argPatternQ] /; Count[Hold[args], ("data" -> _), {0, Infinity}] > 0 := Catch[Module[{options, dataset, defaultXLabel, defaultYLabel, frameLabel, points, lines, smoothLines, columns, abLines, hLines, vLines, graphicsPrimitives, xScaleType, yScaleType, xScaleFunc, yScaleFunc, xTickFunc, yTickFunc, xGridLineFunc, yGridLineFunc, graphic},

  options = Cases[Hold@{args}, _Rule, {2}];

  dataset = Lookup[options, "data", {}];

  options = Join[options, {"data" -> dataset, "x" -> Lookup[options, "x", Null], "y" -> Lookup[options, "y", Null]}];

  (* Default x and y labels *)
  defaultXLabel = First@Cases[Hold@args, ("x" -> xlbl_) :> ToString[xlbl], {0, Infinity}];
  defaultYLabel = First@Cases[Hold@args, ("y" -> ylbl_) :> ToString[ylbl], {0, Infinity}];
  frameLabel = Lookup[options, FrameLabel, OptionValue[ggplot, FrameLabel]]; (* allow default FrameLabel style to be given as well and have it trump any other labeling unless it's 'Automatic'*)
  frameLabel = Which[
    frameLabel === Automatic,
    {defaultXLabel, defaultYLabel} /. str_?StringQ :> Style[str, Opacity[1], FontColor -> Black],
    MatchQ[frameLabel, {{_?StringQ, _?StringQ}, {_?StringQ, _?StringQ}} | {_?StringQ, _?StringQ} | _?StringQ],
    frameLabel /. str_?StringQ :> Style[str, Opacity[1], FontColor -> Black],
    True,
    frameLabel
  ];

  (* Get all scaling information *)
  xScaleType = reconcileXScales[Hold@{args}]; (* returns Linear / Date / Log / Log10 / Log2 *)
  yScaleType = reconcileYScales[Hold@{args}]; (* returns Linear / Date / Log / Log10 / Log2 *)

  (* Creating scaling functions to use for x and y *)
  xScaleFunc = With[{f = ToExpression[xScaleType /. "Linear" | "Date" -> "Identity"]}, Function[f[#]]];
  yScaleFunc = With[{f = ToExpression[yScaleType /. "Linear" | "Date" -> "Identity"]}, Function[f[#]]];

  (* Compile all geom information *)
  points      = Cases[Hold@{args}, geomPoint[opts___]      :> geomPoint[opts,      FilterRules[options, Options[geomPoint]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  lines       = Cases[Hold@{args}, geomLine[opts___]       :> geomLine[opts,       FilterRules[options, Options[geomLine]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  smoothLines = Cases[Hold@{args}, geomSmooth[opts___]     :> geomSmooth[opts,     FilterRules[options, Options[geomSmooth]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  abLines     = Cases[Hold@{args}, geomParityLine[opts___] :> geomParityLine[opts, FilterRules[options, Options[geomParityLine]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  hLines      = Cases[Hold@{args}, geomHLine[opts___]      :> geomHLine[opts,      FilterRules[options, Options[geomHLine]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  vLines      = Cases[Hold@{args}, geomVLine[opts___]      :> geomVLine[opts,      FilterRules[options, Options[geomVLine]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  (* columns need a lot more work to sort through *)
  (*columns     = Cases[{geoms}, geomCol[aesthetics__] :> geomCol[dataset, aesthetics, "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];*)

  graphicsPrimitives = {points, lines, smoothLines, abLines, hLines, vLines} // Flatten;

  (* Tick / GridLine functions passed into ggplot FrameTicks -> _ call *)
  With[{tickOptions = FilterRules[{options}, Options[ticks]], gridLineOptions = FilterRules[{options}, Options[gridLines]]},
    xTickFunc = Function[{min, max}, ticks[xScaleType, min, max, tickOptions]];
    yTickFunc = Function[{min, max}, ticks[yScaleType, min, max, tickOptions]];

    xGridLineFunc = Function[{min, max}, gridLines[xScaleType, min, max, gridLineOptions]];
    yGridLineFunc = Function[{min, max}, gridLines[yScaleType, min, max, gridLineOptions]]
  ];

  graphic = Graphics[graphicsPrimitives,
    FrameLabel        -> frameLabel,
    PlotStyle         -> Lookup[options, PlotStyle, OptionValue[ggplot, PlotStyle]],
    ImageSize         -> Lookup[options, ImageSize, OptionValue[ggplot, ImageSize]],
    AspectRatio       -> Lookup[options, AspectRatio, OptionValue[ggplot, AspectRatio]],
    Frame             -> Lookup[options, Frame, OptionValue[ggplot, Frame]],
    Axes              -> Lookup[options, Axes, OptionValue[ggplot, Axes]],
    LabelStyle        -> Lookup[options, LabelStyle, OptionValue[ggplot, LabelStyle]],
    FrameStyle        -> Lookup[options, FrameStyle, OptionValue[ggplot, FrameStyle]],
    FrameTicksStyle   -> Lookup[options, FrameTicksStyle, OptionValue[ggplot, FrameTicksStyle]],
    FrameTicks        -> If[Lookup[options, FrameTicks, OptionValue[ggplot, FrameTicks]] === Automatic,
      {{yTickFunc, False}, {xTickFunc, False}},
      Lookup[options, FrameTicks, OptionValue[ggplot, FrameTicks]]
    ],
    GridLines         -> If[Lookup[options, GridLines, OptionValue[ggplot, GridLines]] === Automatic,
      {xGridLineFunc, yGridLineFunc},
      Lookup[options, GridLines, OptionValue[ggplot, GridLines]]
    ],
    GridLinesStyle    -> Automatic, (* shouldn't need this but do for some reason *)
    Background        -> Lookup[options, Background, OptionValue[ggplot, Background]],
    ImageMargins      -> Lookup[options, ImageMargins, OptionValue[ggplot, ImageMargins]],
    PlotRangeClipping -> Lookup[options, PlotRangeClipping, OptionValue[ggplot, PlotRangeClipping]],
    FilterRules[{options}, Options[ListLinePlot]]
  ];

  graphic
]];

End[];

EndPackage[]