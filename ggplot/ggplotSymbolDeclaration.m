(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

ggplot::usage         = "TBD";

(* Geoms *)
geomPoint::usage        = "TBD";
geomLine::usage         = "TBD";
geomSmooth::usage       = "TBD";
geomCol::usage          = "TBD";
geomParityLine::usage   = "TBD";
geomHLine::usage        = "TBD";
geomVLine::usage        = "TBD";
geomHistogram::usage    = "TBD";

(* Scales *)
scaleXLinear::usage   = "TBD";
scaleYLinear::usage   = "TBD";
scaleXDate::usage     = "TBD";
scaleYDate::usage     = "TBD";
scaleXLog::usage      = "TBD";
scaleYLog::usage      = "TBD";

(* Ticks *)
ticks::usage                            = "TBD";
numberOfMajorTicks2::usage              = "TBD";
numberOfMinorTicksPerMajorTick2::usage  = "TBD";
majorTickStyle2::usage                  = "TBD";
minorTickStyle2::usage                  = "TBD";
majorTickLength2::usage                 = "TBD";
minorTickLength2::usage                 = "TBD";

Options[ticks] = {numberOfMajorTicks2 -> 8, numberOfMinorTicksPerMajorTick2 -> 1, majorTickStyle2 -> Directive[GrayLevel[0], Thickness[0.001`]], minorTickStyle2 -> Directive[GrayLevel[0], Thickness[0.001`]], majorTickLength2 -> {0., 0.}, minorTickLength2 -> {0., 0.}, DateTicksFormat -> Automatic, majorGridLineStyle2 -> Directive[GrayLevel[0.6], Thickness[0.0008]], minorGridLineStyle2 -> Directive[GrayLevel[0.85], Thickness[0.0008]]};

(* GridLines*)
gridLines::usage            = "TBD";
majorGridLineStyle2::usage  = "TBD";
minorGridLineStyle2::usage  = "TBD";

Options[gridLines] = Options[ticks];

Begin["`Private`"];

End[];

EndPackage[];
