(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Main gridLine creation function which copies exactly the ticks function, then just reformats the output *)
(* TODO: the options here are still created and referenced in Alex, need to reconcile that *)
gridLines[list_?ListQ, opts : OptionsPattern[]] := ReplaceAll[list, {
  (*Major ticks*)
  {value_?NumericQ, display : Except[(_?StringQ | _Spacer)], ___} :> {value, OptionValue[majorGridLineStyle2]},
  (*Minor ticks*)
  {value_?NumericQ, display : (_?StringQ | _Spacer), ___} :> {value, OptionValue[minorGridLineStyle2]}
}];

(* Special method for handling Dates*)
gridLines["Date", min : (_?NumericQ | _?DateObjectQ), max : (_?NumericQ | _?DateObjectQ), opts: OptionsPattern[]] := Module[{dateTicks, newMin, newMax, dateGridLines},
  dateTicks = Charting`FindDateDivisions[{min, max}, OptionValue[numberOfMajorTicks2]];
  {newMin, newMax} = With[{tickDistance = (#[[2]] - #[[1]]) & @dateTicks[[1;;2, 1]]}, {AbsoluteTime[min] - tickDistance, AbsoluteTime[max] + tickDistance}]; (* Update the min/max to go one more outside of the determined ticks as sometimes date ticks don't include everything the way we would like them to *)
  dateTicks = Charting`FindDateDivisions[{newMin, newMax}, OptionValue[numberOfMajorTicks2]];
  dateGridLines = dateTicks /. {value_?NumericQ, dateString_,___} :> {value, OptionValue[majorGridLineStyle2]};
  dateGridLines
];

gridLines[min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := gridLines[ticks[min, max, FilterRules[{opts}, Options[ticks]]], FilterRules[{opts}, Options[gridLines]]];

gridLines[func_?StringQ, min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := gridLines[ticks[func, min, max, FilterRules[{opts}, Options[ticks]]], FilterRules[{opts}, Options[gridLines]]];

End[];

EndPackage[];
