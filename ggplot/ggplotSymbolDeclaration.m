(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

ggplot::usage             = "TBD";
ggplotThemeWhite::usage   = "TBD";
ggplotThemeGray::usage    = "TBD";
ggplotSetTheme::usage     = "TBD";
$ggplotTheme::usage       = "TBD";

(* Geoms *)
geomPoint::usage        = "TBD";
geomLine::usage         = "TBD";
geomPath::usage         = "TBD";
geomSmooth::usage       = "TBD";
geomCol::usage          = "TBD";
geomParityLine::usage   = "TBD";
geomHLine::usage        = "TBD";
geomVLine::usage        = "TBD";
geomHistogram::usage    = "TBD";

(* Scales *)
scaleXLinear2::usage   = "TBD";
scaleYLinear2::usage   = "TBD";
scaleXDate2::usage     = "TBD";
scaleYDate2::usage     = "TBD";
scaleXLog2::usage      = "TBD";
scaleYLog2::usage      = "TBD";

(* ticks2 *)
ticks2::usage                            = "TBD";
numberOfMajorTicks2::usage              = "TBD";
numberOfMinorTicksPerMajorTick2::usage  = "TBD";
majorTickStyle2::usage                  = "TBD";
minorTickStyle2::usage                  = "TBD";
majorTickLength2::usage                 = "TBD";
minorTickLength2::usage                 = "TBD";

Options[ticks2] = {numberOfMajorTicks2 -> 8, numberOfMinorTicksPerMajorTick2 -> 1, majorTickStyle2 -> Directive[GrayLevel[0], Thickness[0.001`]], minorTickStyle2 -> Directive[GrayLevel[0], Thickness[0.001`]], majorTickLength2 -> {0., 0.}, minorTickLength2 -> {0., 0.}, DateTicksFormat -> Automatic, majorGridLineStyle2 -> Directive[GrayLevel[0.6], Thickness[0.0008]], minorGridLineStyle2 -> Directive[GrayLevel[0.85], Thickness[0.0008]]};

(* gridLines2*)
gridLines2::usage            = "TBD";
majorGridLineStyle2::usage  = "TBD";
minorGridLineStyle2::usage  = "TBD";

Options[gridLines2] = Options[ticks2];

Begin["`Private`"];

End[];

EndPackage[];
