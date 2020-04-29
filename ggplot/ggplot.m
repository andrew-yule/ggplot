(* :Title: ggplot *)
(* :Context: ggplot` *)
(* :Author: andrewyule *)
(* :Date: 2019-11-10 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

ggplot::xOrYNotGiven    = "A geom was given without specifying the x or y mapping";
ggplot::shapecontinuous = "A continuous variable can not be mapped to a shape";
ggplot::shapecount      = "More than 5 discrete shapes are present, aborting... (this should be fixed)";

validDatasetQ[dataset_] := MatchQ[dataset, {_?AssociationQ..}];

(* Main ggplot method and entry point *)
Options[ggplot] = Join[Options[ListLinePlot], Options[ticks], Options[gridLines](*, Options[Alex`Plotting`linearGridLines],*) (*{DateTicksFormat -> Automatic}*)];
SetOptions[ggplot,
  ImageSize -> 400, AspectRatio -> 7/10, Frame -> True, Axes -> False,
  ImageMargins -> Automatic,
  LabelStyle -> Directive[12, FontFamily -> "Arial"],
  FrameStyle -> Directive[GrayLevel[0.6], Thickness[0.0008`]],
  FrameTicksStyle -> Directive[Black, Opacity[1]],
  FrameTicks -> Automatic, GridLines -> Automatic,  Background -> White,
  PlotRange -> All
];
ggplot[inputDataset_?validDatasetQ, geoms__, opts : OptionsPattern[]] := Catch[Module[{dataset, points, lines, smoothLines, columns, abLines, hLines, vLines, graphicsPrimitives, scaleX, scaleY, xTickFunc, yTickFunc, xGridLineFunc, yGridLineFunc, graphic},

  (* Switch dates to absolute times *)
  dataset = Replace[inputDataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* Compile all geom information *)
  points      = Cases[{geoms}, geomPoint[aesthetics__] :> geomPoint[dataset, aesthetics], {0, Infinity}];
  lines       = Cases[{geoms}, geomLine[aesthetics__] :> geomLine[dataset, aesthetics], {0, Infinity}];
  smoothLines = Cases[{geoms}, geomSmooth[aesthetics__] :> geomSmooth[dataset, aesthetics], {0, Infinity}];
  columns     = Cases[{geoms}, geomCol[aesthetics__] :> geomCol[dataset, aesthetics], {0, Infinity}];
  abLines     = Cases[{geoms}, geomParityLine[aesthetics___] :> geomParityLine[dataset, aesthetics], {0, Infinity}];
  hLines      = Cases[{geoms}, geomHLine[aesthetics__] :> geomHLine[dataset, aesthetics], {0, Infinity}];
  vLines      = Cases[{geoms}, geomVLine[aesthetics__] :> geomVLine[dataset, aesthetics], {0, Infinity}];

  graphicsPrimitives = {points, lines, smoothLines, columns, abLines, hLines, vLines} // Flatten;

  (* Compile all scaling information *)
  scaleX = reconcileXScales[geoms]; (* returns Linear / Date / Log / Log10 / Log2 *)
  scaleY = reconcileYScales[geoms]; (* returns Linear / Date / Log / Log10 / Log2 *)

  (* TODO: Address scaling functions (must be done before creating tick functions as min/max should already be scaled *)

  (* Tick / GridLine functions passed into ggplot FrameTicks -> _ call *)
  With[{tickOptions = FilterRules[{opts}, Options[ticks]], gridLineOptions = FilterRules[{opts}, Options[gridLines]]},
    xTickFunc = Function[{min, max}, ticks[scaleX, min, max, tickOptions]];
    yTickFunc = Function[{min, max}, ticks[scaleY, min, max, tickOptions]];

    xGridLineFunc = Function[{min, max}, gridLines[scaleX, min, max, gridLineOptions]];
    yGridLineFunc = Function[{min, max}, gridLines[scaleY, min, max, gridLineOptions]]
  ];

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
    FilterRules[{opts}, Options[ListPlot]]
  ];

  graphic
]];
ggplot[geoms__, opts : OptionsPattern[]][dataset_?validDatasetQ] := ggplot[dataset, geoms, opts];

End[];

EndPackage[]