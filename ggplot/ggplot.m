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
argPatternQ[expr___] := MatchQ[Hold[expr], Hold[(_Rule | geomPoint[___] | geomLine[___] | geomPath[___] | geomSmooth[___] | geomVLine[___] | geomHLine[___] | geomParityLine[___] | geomHistogram[___] | geomCol[___] | scaleXDate[___] | scaleXLinear[___] | scaleXLog[___] | scaleYDate[___] | scaleYLinear[___] | scaleYLog[___]) ...]];

(* Main ggplot method and entry point *)
Options[ggplot] = DeleteDuplicates[Join[{"data" -> {}}, Options[ListLinePlot], Options[ticks], Options[gridLines]]];
(* Options for ggplot are set further below in themes *)
Attributes[ggplot] = {HoldAllComplete};
ggplot[ds_?validDatasetQ, args___?argPatternQ] := ggplot["data" -> ds, args];
ggplot[args___?argPatternQ][ds_?validDatasetQ] := ggplot["data" -> ds, args];
ggplot[args___?argPatternQ] /; Count[Hold[args], ("data" -> _), {0, Infinity}] > 0 := Catch[Module[{heldArgs, options, dataset, defaultXLabel, defaultYLabel, frameLabel, points, lines, paths, smoothLines, columns, abLines, hLines, vLines, histograms, graphicsPrimitives, xScaleType, yScaleType, xScaleFunc, yScaleFunc, xDiscreteLabels, yDiscreteLabels, xTickFunc, yTickFunc, xGridLineFunc, yGridLineFunc, graphic},
  
  heldArgs = Hold[args];
  options = Cases[heldArgs, _Rule, 1];
  dataset = Lookup[options, "data", {}];
  options = Join[options, {"data" -> dataset, "x" -> Lookup[options, "x", Null], "y" -> Lookup[options, "y", Null]}];

  (* Default x and y labels *)
  defaultXLabel = First@Cases[heldArgs, ("x" -> xlbl_) :> ToString[xlbl], {0, Infinity}];
  defaultYLabel = Quiet[Check[First@Cases[heldArgs, ("y" -> ylbl_) :> ToString[ylbl], {0, Infinity}], ""]];
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
  xScaleType = reconcileXScales[heldArgs]; (* returns Discrete / Linear / Date / Log / Log10 / Log2 *)
  yScaleType = reconcileYScales[heldArgs]; (* returns Discrete / Linear / Date / Log / Log10 / Log2 *)

  (* Creating scaling functions to use for x and y *)
  xScaleFunc = If[xScaleType == "Discrete",
    createDiscreteScaleFunc["x", heldArgs],
    With[{f = ToExpression[xScaleType /. "Linear" | "Date" -> "Identity"]}, Function[f[#]]]
  ];
  yScaleFunc = If[yScaleType == "Discrete",
    createDiscreteScaleFunc["y", heldArgs],
    With[{f = ToExpression[yScaleType /. "Linear" | "Date" -> "Identity"]}, Function[f[#]]]
  ];

  If[xScaleType == "Discrete", xDiscreteLabels = createDiscreteScaleLabels["x", heldArgs]];
  If[yScaleType == "Discrete", yDiscreteLabels = createDiscreteScaleLabels["y", heldArgs]];

  (* Compile all geom information which will create graphics primitives *)
  points      = Cases[heldArgs, geomPoint[opts___]      :> geomPoint[opts,      FilterRules[options, Options[geomPoint]],      "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  lines       = Cases[heldArgs, geomLine[opts___]       :> geomLine[opts,       FilterRules[options, Options[geomLine]],       "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  paths       = Cases[heldArgs, geomPath[opts___]       :> geomPath[opts,       FilterRules[options, Options[geomPath]],       "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  smoothLines = Cases[heldArgs, geomSmooth[opts___]     :> geomSmooth[opts,     FilterRules[options, Options[geomSmooth]],     "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  abLines     = Cases[heldArgs, geomParityLine[opts___] :> geomParityLine[opts, FilterRules[options, Options[geomParityLine]], "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  hLines      = Cases[heldArgs, geomHLine[opts___]      :> geomHLine[opts,      FilterRules[options, Options[geomHLine]],      "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  vLines      = Cases[heldArgs, geomVLine[opts___]      :> geomVLine[opts,      FilterRules[options, Options[geomVLine]],      "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  histograms  = Cases[heldArgs, geomHistogram[opts___]  :> geomHistogram[opts,  FilterRules[options, Options[geomHistogram]],  "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];
  (* columns need a lot more work to sort through *)
  (*columns     = Cases[{geoms}, geomCol[aesthetics__] :> geomCol[dataset, aesthetics, "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc], {0, Infinity}];*)

  graphicsPrimitives = {points, lines, paths, smoothLines, abLines, hLines, vLines, histograms} // Flatten;

  (* Tick / GridLine functions passed into ggplot FrameTicks -> _ call *)
  With[{tickAndGridLineOptions = FilterRules[{options}, {Options[ticks], Options[gridLines]}]},
    xTickFunc = If[xScaleType == "Discrete",
      ticks[xScaleType, xDiscreteLabels, tickAndGridLineOptions],
      Function[{min, max}, ticks[xScaleType, min, max, tickAndGridLineOptions]]
    ];
    yTickFunc = If[yScaleType == "Discrete",
      ticks[yScaleType, yDiscreteLabels, tickAndGridLineOptions],
      Function[{min, max}, ticks[yScaleType, min, max, tickAndGridLineOptions]]
    ];

    xGridLineFunc = If[xScaleType == "Discrete",
      gridLines[xScaleType, xDiscreteLabels, tickAndGridLineOptions],
      Function[{min, max}, gridLines[xScaleType, min, max, tickAndGridLineOptions]]
    ];
    yGridLineFunc = If[yScaleType == "Discrete",
      gridLines[yScaleType, yDiscreteLabels, tickAndGridLineOptions],
      Function[{min, max}, gridLines[yScaleType, min, max, tickAndGridLineOptions]]
    ];
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
    Prolog            -> Lookup[options, Prolog, OptionValue[ggplot, Prolog]],
    Method            -> Lookup[options, Method, OptionValue[ggplot, Method]],
    FilterRules[{options}, Options[ListLinePlot]]
  ];

  graphic
]];

End[];

EndPackage[]