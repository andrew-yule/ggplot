(* :Title: ggplot *)
(* :Context: ggplot` *)
(* :Author: andrewyule *)
(* :Date: 2019-11-10 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Theme setting *)

ggplotSetTheme[ggplotThemeWhite] := Module[{},
  SetOptions[ggplot,
    ImageSize                       -> 400,
    AspectRatio                     -> 7/10,
    Frame                           -> True,
    Axes                            -> False,
    ImageMargins                    -> Automatic,
    LabelStyle                      -> Directive[12, FontFamily -> "Arial"],
    FrameLabel                      -> Automatic,
    FrameStyle                      -> Directive[GrayLevel[0.6], Thickness[0.0008`]],
    FrameTicksStyle                 -> Directive[Black, Opacity[1]],
    FrameTicks                      -> Automatic,
    GridLines                       -> Automatic,
    Background                      -> White,
    PlotRange                       -> All,
    PlotRangeClipping               -> True,
    Method                          -> Automatic,
    Prolog                          -> {}
  ];
  SetOptions[ticks,
    numberOfMajorTicks2             -> 8,
    numberOfMinorTicksPerMajorTick2 -> 1,
    majorTickStyle2                 -> Directive[GrayLevel[0], Thickness[0.001]],
    minorTickStyle2                 -> Directive[GrayLevel[0], Thickness[0.001]],
    majorTickLength2                -> {0., 0.},
    minorTickLength2                -> {0., 0.},
    DateTicksFormat                 -> Automatic,
    majorGridLineStyle2             -> Directive[GrayLevel[0.8], Thickness[0.002]],
    minorGridLineStyle2             -> Directive[GrayLevel[0.9], Thickness[0.001]]
  ];
  Options[gridLines] = Options[ticks];
  Options[formatTicks] = Options[ticks];
  Options[formatGridLines] = Options[ticks];
  $ggplotTheme = ggplotThemeWhite;
];

ggplotSetTheme[ggplotThemeGray] := Module[{},
  SetOptions[ggplot,
    ImageSize                       -> 400,
    AspectRatio                     -> 7/10,
    Frame                           -> True,
    Axes                            -> False,
    ImageMargins                    -> Automatic,
    LabelStyle                      -> Directive[12, FontFamily -> "Arial"],
    FrameLabel                      -> Automatic,
    FrameStyle                      -> Directive[Opacity[0]],
    FrameTicksStyle                 -> Directive[Black, Opacity[1]],
    FrameTicks                      -> Automatic,
    GridLines                       -> Automatic,
    Background                      -> White,
    PlotRange                       -> All,
    PlotRangeClipping               -> True,
    Method                          -> {"GridLinesInFront" -> True}, (* important to have this for gray background *)
    Prolog                          -> {RGBColor[0.92, 0.92, 0.92, 1.], Rectangle[Scaled[{0, 0}], Scaled[{1, 1}]]}
  ];
  SetOptions[ticks,
    numberOfMajorTicks2             -> 6,
    numberOfMinorTicksPerMajorTick2 -> 2,
    majorTickStyle2                 -> Directive[Opacity[1], Thickness[0.002], Black],
    minorTickStyle2                 -> Directive[GrayLevel[0], Thickness[0.001]],
    majorTickLength2                -> {0., 0.0075},
    minorTickLength2                -> {0., 0.},
    DateTicksFormat                 -> Automatic,
    majorGridLineStyle2             -> Directive[White, Thickness[0.002]],
    minorGridLineStyle2             -> Directive[White, Thickness[0.001]]
  ];
  Options[gridLines] = Options[ticks];
  Options[formatTicks] = Options[ticks];
  Options[formatGridLines] = Options[ticks];
  $ggplotTheme = ggplotThemeGray;
];

End[];

EndPackage[]