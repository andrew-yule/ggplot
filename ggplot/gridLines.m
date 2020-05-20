(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Main gridLine creation function which copies exactly the ticks function, then just reformats the output *)
(* TODO: the options here are still created and referenced in Alex, need to reconcile that *)
Options[formatGridLines] = Join[Options[ticks], Options[gridLines]];
formatGridLines[list_?ListQ, opts : OptionsPattern[]] := ReplaceAll[list, {
  (*Major ticks*)
  {value_?NumericQ, display : Except[(_?StringQ | _Spacer)], ___} :> {value, OptionValue[majorGridLineStyle2]},
  (*Minor ticks*)
  {value_?NumericQ, display : (_?StringQ | _Spacer), ___} :> {value, OptionValue[minorGridLineStyle2]}
}];

(* Publicly accessbile tick functions *)

gridLines["Linear" | "Identity", min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := formatGridLines[ticks["Linear", min, max, opts], opts];

(* Special method for handling Dates because we want to go just a little bit further beyond min/max than defaults will do *)
gridLines["Date", min : (_?NumericQ | _?DateObjectQ), max : (_?NumericQ | _?DateObjectQ), opts: OptionsPattern[]] := Module[{dateTicks, newMin, newMax, dateGridLines},
  dateTicks = Charting`FindDateDivisions[{min, max}, OptionValue[numberOfMajorTicks2]];
  {newMin, newMax} = With[{tickDistance = (#[[2]] - #[[1]]) & @dateTicks[[1;;2, 1]]}, {AbsoluteTime[min] - tickDistance, AbsoluteTime[max] + tickDistance}]; (* Update the min/max to go one more outside of the determined ticks as sometimes date ticks don't include everything the way we would like them to *)
  dateTicks = Charting`FindDateDivisions[{newMin, newMax}, OptionValue[numberOfMajorTicks2]];
  dateGridLines = dateTicks /. {value_?NumericQ, dateString_,___} :> {value, OptionValue[majorGridLineStyle2]};
  dateGridLines
];

gridLines["Log", min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := formatGridLines[ticks["Log", min, max, opts], opts];
gridLines["Log10", min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := formatGridLines[ticks["Log10", min, max, opts], opts];
gridLines["Log2", min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := formatGridLines[ticks["Log2", min, max, opts], opts];

gridLines["Discrete", lbls_?ListQ, opts : OptionsPattern[]] := formatGridLines[ticks["Discrete", lbls, opts], opts];

(* If using the ggplotThemeGray, we need to put the gridlines in the Prolog as the gray background will cover them otherwise so these are helper functions for that *)
convertGridLinesToActualLinesForProlog["X" gridLines_] := gridLines /. {line_, d_Directive} :> {d, InfiniteLine[{{line, 0}, {line, 1}}]};
convertGridLinesToActualLinesForProlog["Y" gridLines_] := gridLines /. {line_, d_Directive} :> {d, InfiniteLine[{{0, line}, {1, line}}]};

End[];

EndPackage[];
