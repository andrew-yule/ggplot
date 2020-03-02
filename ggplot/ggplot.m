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

ggplot::usage     = "TBD";
geomPoint::usage  = "TBD";
geomLine::usage   = "TBD";

Begin["`Private`"];

ggplot::xOrYNotGiven    = "A geom was given without specifying the x or y mapping";
ggplot::shapecontinuous = "A continuous variable can not be mapped to a shape";
ggplot::shapecount      = "More than 5 discrete shapes are present, aborting... (this should be fixed)";

(*ggPlotGraphics = Graphics;*)
(*SetOptions[ggPlotGraphics, Frame -> True, PlotRange -> All, GridLines -> Automatic, AspectRatio -> 7/10, PlotRangeClipping -> True];*)

(*
  NOTE: Originally I was using Graphics object directly, however, Graphics does not have Options support for ScalingFunction therefore we need to use
  something like ListPlot, but set PlotStyle to be None, then apply the geom's as Epilog functions

  New UPDATE: It may still be better to use Graphics and then we'll write our own ability to do a scaling function.
*)

Options[ggplot] = Join[Options[ListLinePlot], Options[Alex`Plotting`linearTicks], Options[Alex`Plotting`linearGridLines], {DateTicksFormat -> Automatic}];
SetOptions[ggplot,
  ImageSize -> 400, AspectRatio -> 2/3, Frame -> True, Axes -> False,
  ImageMargins -> Automatic,
  LabelStyle -> Directive[12, FontFamily -> "Arial"],
  FrameStyle -> Directive[GrayLevel[0.6], Thickness[0.0008`]],
  FrameTicksStyle -> Directive[Black, Opacity[1]],
  FrameTicks -> Automatic, GridLines -> Automatic,  Background -> White,
  PlotRange -> All
];
ggplot[dataset_, geoms : (geomPoint[__] | geomLine[__] | {(geomPoint[__] | geomLine[__])..}), opts : OptionsPattern[]] := Catch[Module[{points, lines, scalingFunctionX, scalingFunctionY, graphicsPrimitives, dataForListPlot, graphic},
  points = Cases[geoms, geomPoint[aesthetics__]:> geomPoint[dataset, aesthetics], {0, Infinity}];
  lines = Cases[geoms, geomLine[aesthetics__]:> geomLine[dataset, aesthetics], {0, Infinity}];

  graphicsPrimitives = {points, lines} // Flatten;
  (*dataForListPlot = Cases[graphicsForEpilog, {x_?NumericQ, y_?NumericQ}, {0, Infinity}];*)

  (*
  scalingFunctionX = With[{f = (OptionValue[ScalingFunctions][[1]] //. {s_?StringQ :> ToExpression[s], None -> Identity})}, Function[f[#]]];
  scalingFunctionY = With[{f = (OptionValue[ScalingFunctions][[2]] //. {s_?StringQ :> ToExpression[s], None -> Identity})}, Function[f[#]]];
  graphicsPrimitives = graphicsPrimitives // ReplaceAll[{x_?NumericQ, y_?NumericQ} :> {scalingFunctionX[x], scalingFunctionY[y]}];
  *)

  graphic = Graphics[graphicsPrimitives,
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
      {
        {Function[{minY, maxY}, Alex`Plotting`linearTicks[minY, maxY, FilterRules[Join[{opts}, Options[ggplot]], Options[Alex`Plotting`linearTicks]]]], False},
        {Function[{minX, maxX}, Alex`Plotting`linearTicks[minX, maxX, FilterRules[Join[{opts}, Options[ggplot]], Options[Alex`Plotting`linearTicks]]]], False}
      },
      OptionValue[FrameTicks]
    ],
    GridLines -> If[OptionValue[GridLines] === Automatic,
      {
        Function[{minX, maxX}, Alex`Plotting`linearGridLines[minX, maxX, FilterRules[Join[{opts}, Options[ggplot]], Join[Options[Alex`Plotting`linearTicks], Options[Alex`Plotting`linearGridLines]]]]],
        Function[{minY, maxY}, Alex`Plotting`linearGridLines[minY, maxY, FilterRules[Join[{opts}, Options[ggplot]], Join[Options[Alex`Plotting`linearTicks], Options[Alex`Plotting`linearGridLines]]]]]
      },
      OptionValue[GridLines]
    ],
    GridLinesStyle -> Automatic, (* shouldn't need this but do for some reason *)
    Background -> OptionValue[Background],
    ImageMargins -> OptionValue[ImageMargins],
    FilterRules[{opts}, Options[ListPlot]]
  ];

  graphic
]];

isDiscreteDataQ[data_]    := If[MatchQ[DeleteDuplicates[data], {_?StringQ ..}], True, False];
getAllKeys[data_]         := data // Keys /* Flatten /* DeleteDuplicates;
getDiscreteKeys[data_]    := Sort[DeleteDuplicates[data]];
getContinuousRange[data_] := MinMax[data];

(* Functions to determine aesthetics *)

(* Default values if not being used as an aesthetic *)
reconcileAesthetics[dataset_, Null, "color"]     := Function[Black];
reconcileAesthetics[dataset_, Null, "size"]      := Function[10];
reconcileAesthetics[dataset_, Null, "alpha"]     := Function[Opacity[1]];
reconcileAesthetics[dataset_, Null, "shape"]     := Function["\[FilledCircle]"];
reconcileAesthetics[dataset_, Null, "thickness"] := Function[Thick];

(* If an aesthetic is used but does not match a key, then assume it's a user directly specifying *)
reconcileAesthetics[dataset_, val_Function, aes_]  := val;
(* Note there is a bug with KeyExists as it won't accept Key["string"] in order to see if the key does exist *)
(*reconcileAesthetics[dataset_, val_, aes_] /; !(dataset // Map[KeyExistsQ[Key[val]]] /* ContainsAny[{True}]) := Function[val];*)
(* Using Lookup instead *)
reconcileAesthetics[dataset_, val_, aes_] /; (dataset // Map[Lookup[#, Key[val], False] &] /* ContainsAny[{False}]) := Function[val];

(* More complex logic if aesthetic is used and mapped to a key *)
reconcileAesthetics[dataset_, key_, aes_] := Module[{data, func, discreteDataQ, keys, minMax},
  data = dataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  If[discreteDataQ,
    keys = Sort[getDiscreteKeys[data]];
    func = Switch[aes,
      "color",      Function[AssociationThread[keys, ggplotColorsFunc[Length[keys]]][#]],
      "size",       Function[AssociationThread[keys, Subdivide[10, 25, Length[keys] - 1]][#]],
      "alpha",      Function[Opacity[AssociationThread[keys, Subdivide[0.1, 1, Length[keys] - 1]][#]]],
      "shape",      If[Length[keys] > 5, Message[ggplot::shapecount]; Throw[Null];, Function[AssociationThread[keys, Take[{"\[FilledCircle]", "\[FilledUpTriangle]", "\[FilledSquare]", "\[FivePointedStar]", "\[FilledDiamond]"}, Length[keys]]][#]]],
      "thickness",  Function[Thick]
    ];
  ];
  If[!discreteDataQ,
    minMax = getContinuousRange[data];
    func = Switch[aes,
      "color",      With[{minMax = minMax}, Function[Blend[{Red, Blue}, Rescale[#, minMax]]]],
      "size",       With[{minMax = minMax}, Function[x, Rescale[x, minMax, {10, 25}]]],
      "alpha",      With[{minMax = minMax}, Function[Opacity[Rescale[#, minMax, {0.1, 1}]]]],
      "shape",      Message[ggplot::shapecontinuous]; Throw[Null];,
      "thickness",  Function[Thick]
    ];
  ];
  func
];

(* Helper functions for colors *)
ggplotColorsFunc[1] := Black;
ggplotColorsFunc[numberOfSeries_?IntegerQ] /; numberOfSeries > 1 := Drop[LCHColor[0.65, 0.6, #] & /@ (Subdivide[30, 390, numberOfSeries]/390), -1];
ggplotColorsFunc[___] := ggplotColorsFunc[1];

(* TODO: Implement actual logic for these functions below *)

determineThicknessFunc[___] := Function[Thick];
(* determineLineTypeFunc[] := Function[Dashing[1]]; *) (* Need to disable for now as Dashing[1] causes a known Graphics issue with lines flickering. Bug has been reported to Wolfram. *)

(* geomPoint implementation *)

Options[geomPoint] = {"x" -> Null, "y" -> Null, "color" -> Null, "size" -> Null, "alpha" -> Null, "shape" -> Null};
geomPoint[dataset_?ListQ, aesthetics : OptionsPattern[]] := Module[{colorFunc, sizeFunc, alphaFunc, shapeFunc, output},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  (* For each key necessary, get functions to be used to specify the aesthetic *)
  colorFunc = reconcileAesthetics[dataset, OptionValue["color"], "color"];
  sizeFunc  = reconcileAesthetics[dataset, OptionValue["size"], "size"];
  alphaFunc = reconcileAesthetics[dataset, OptionValue["alpha"], "alpha"];
  shapeFunc = reconcileAesthetics[dataset, OptionValue["shape"], "shape"];

  (*Grab the point data and for each Point apply the correct aesthetic*)
  output = dataset // Map[{
    colorFunc[#[OptionValue["color"]]],
    alphaFunc[#[OptionValue["alpha"]]],
    Inset[Style[shapeFunc[#[OptionValue["shape"]]], sizeFunc[#[OptionValue["size"]]]], {#[OptionValue["x"]], #[OptionValue["y"]]}]
  } &];

  (* Grouping data but doing a GeometricTransformation on similar Inset values to speed up the plotting once inside Graphics *)
  output = output // GroupBy[Function[{#[[1]], #[[2]], Inset[#[[3, 1]], {0, 0}]}] -> Function[#[[3, 2]]]] // Normal // Map[{#[[1, 1]], #[[1, 2]], GeometricTransformation[#[[1, 3]], List /@ #[[2]]]} &];
  output

  (*Original version*)
  (*
   Group the data by similar aesthetics and then just apply to the first one so we reduce the memory size
   of the output (since graphics will apply the same forms to following expressions)
  *)
  (*
  output = output // GroupBy[Most -> Last] // Normal // ReplaceAll[Rule -> List] // Flatten;
  output
  *)
];

(* geomLine implementation *)

Options[geomLine] = {"x" -> Null, "y" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "dashing" -> Null};
geomLine[dataset_?ListQ, aesthetics : OptionsPattern[]] := Module[{groupbyKeys, colorFunc, thicknessFunc, alphaFunc, lineTypeFunc, output},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  (* For each key necessary, get functions to be used to specify the aesthetic *)
  colorFunc     = reconcileAesthetics[dataset, OptionValue["color"], "color"];
  alphaFunc     = reconcileAesthetics[dataset, OptionValue["alpha"], "alpha"];
  thicknessFunc = reconcileAesthetics[dataset, OptionValue["thickness"], "thickness"];
  (*dashingFunc = reconcileAesthetics[dataset, OptionValue["dashing"], "dashing"];*)

  (* Group the data and apply correct aesthetics while making a line primitive *)
  groupbyKeys = DeleteCases[{OptionValue["color"], OptionValue["thickness"], OptionValue["alpha"], OptionValue["thickness"], OptionValue["dashing"]}, Null];
  output = dataset // GroupBy[((# /@ groupbyKeys) &)] // Map[{
    colorFunc[Quiet@#[[1, OptionValue["color"]]]],
    alphaFunc[Quiet@#[[1, OptionValue["alpha"]]]],
    thicknessFunc[Quiet@#[[1, OptionValue["thickness"]]]],
    (*lineTypeFunc[Quiet@#[[1, OptionValue["linetype"]]]],*)
    Line@Sort@Transpose[{#[[All, OptionValue["x"]]], #[[All, OptionValue["y"]]]}]
  } &] // Values;

  output
];

End[]; (* `Private` *)

EndPackage[]