(* :Title: ggplot *)
(* :Context: ggplot` *)
(* :Author: andrewyule *)
(* :Date: 2019-11-10 *)

BeginPackage["ggplot`"];
(* Exported symbols added here with SymbolName::usage *)

ggplot::usage         = "TBD";
geomPoint::usage      = "TBD";
geomLine::usage       = "TBD";
geomSmooth::usage     = "TBD";
geomCol::usage        = "TBD";
geomParityLine::usage = "TBD";
geomHLine::usage      = "TBD";
geomVLine::usage      = "TBD";

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

geomsPattern = (geomPoint[__] | geomLine[__] | geomSmooth[__] | geomCol[__] | geomParityLine[___] | geomHLine[__] | geomVLine[__] | {(geomPoint[__] | geomLine[__] | geomSmooth[__] | geomCol[__] | geomParityLine[___] | geomHLine[__] | geomVLine[__])..});

Options[ggplot] = Join[Options[ListLinePlot], Options[Alex`Plotting`linearTicks], Options[Alex`Plotting`linearGridLines], {DateTicksFormat -> Automatic}];
SetOptions[ggplot,
  ImageSize -> 400, AspectRatio -> 2/3, Frame -> True, Axes -> False,
  ImageMargins -> Automatic,
  LabelStyle -> Directive[12, FontFamily -> "Arial"],
  FrameStyle -> Directive[GrayLevel[0.6], Thickness[0.0008`]],
  FrameTicksStyle -> Directive[Black, Opacity[1]],
  FrameTicks -> Automatic, GridLines -> Automatic,  Background -> White,
  PlotRange -> All,
  numberOfMinorTicksPerMajorTick -> 0
];
ggplot[dataset_, geoms : geomsPattern, opts : OptionsPattern[]] := Catch[Module[{points, lines, smoothLines, columns, abLines, hLines, vLines, graphicsPrimitives, dataForListPlot, graphic},
  (* Compile all geom data *)
  points      = Cases[geoms, geomPoint[aesthetics__] :> geomPoint[dataset, aesthetics], {0, Infinity}];
  lines       = Cases[geoms, geomLine[aesthetics__] :> geomLine[dataset, aesthetics], {0, Infinity}];
  smoothLines = Cases[geoms, geomSmooth[aesthetics__] :> geomSmooth[dataset, aesthetics], {0, Infinity}];
  columns     = Cases[geoms, geomCol[aesthetics__] :> geomCol[dataset, aesthetics], {0, Infinity}];
  abLines     = Cases[geoms, geomParityLine[aesthetics___] :> geomParityLine[dataset, aesthetics], {0, Infinity}];
  hLines      = Cases[geoms, geomHLine[aesthetics__] :> geomHLine[dataset, aesthetics], {0, Infinity}];
  vLines      = Cases[geoms, geomVLine[aesthetics__] :> geomVLine[dataset, aesthetics], {0, Infinity}];

  graphicsPrimitives = {points, lines, smoothLines, columns, abLines, hLines, vLines} // Flatten;

  (* TODO: Address scaling functions *)

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
ggplot[geoms : geomsPattern, opts : OptionsPattern[]][dataset_] := ggplot[dataset, geoms, opts]


End[];

EndPackage[]