(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* TODO: the options here are still created and referenced in Alex, need to reconcile that somehow *)

(* Options are actual symbols so they are referenced in ggplotSymbolDeclaration *)

(* Main tick formatting functions *)

Options[formatTicks] = Options[ticks];
formatTicks[list_?ListQ, opts : OptionsPattern[]] := ReplaceAll[list, {
  (*Major ticks*)
  {value_?NumericQ, display : Except[(_?StringQ | _Spacer)], tickDistance_} :> {value, display, OptionValue[majorTickLength2], OptionValue[majorTickStyle2]},
  (*Minor ticks*)
  {value_?NumericQ, display : (_?StringQ | _Spacer), tickDistance_} :> {value, display, OptionValue[minorTickLength2], OptionValue[minorTickStyle2]}
}];

(* Publicly accessbile tick functions *)

ticks["Linear" | "Identity", min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := formatTicks[Charting`ScaledTicks["Identity"][min, max, {OptionValue[numberOfMajorTicks2], OptionValue[numberOfMinorTicksPerMajorTick2]}], opts];

(* Special method for handling Dates*)
ticks["Date", min : (_?NumericQ | _?DateObjectQ), max : (_?NumericQ | _?DateObjectQ), opts: OptionsPattern[]] := Module[{dateTicks, newMin, newMax},
  dateTicks = Charting`FindDateDivisions[{min, max}, OptionValue[numberOfMajorTicks2]];
  {newMin, newMax} = With[{tickDistance = (#[[2]] - #[[1]]) & @dateTicks[[1;;2, 1]]}, {AbsoluteTime[min] - tickDistance, AbsoluteTime[max] + tickDistance}]; (* Update the min/max to go one more outside of the determined ticks as sometimes date ticks don't include everything the way we would like them to *)
  dateTicks = Charting`FindDateDivisions[{newMin, newMax}, OptionValue[numberOfMajorTicks2]];
  dateTicks = dateTicks /. {value_?NumericQ, dateString_,___} :> {value, If[OptionValue[DateTicksFormat] === Automatic, dateString, DateString[value, OptionValue[DateTicksFormat]]], OptionValue[majorTickLength2], OptionValue[majorTickStyle2]};
  dateTicks
];

(* Log, Log10, Log2, or Reverse *)
ticks["Log", min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := formatTicks[Charting`ScaledTicks["Log"][min, max, {OptionValue[numberOfMajorTicks2], OptionValue[numberOfMinorTicksPerMajorTick2]}], opts];
ticks["Log10", min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := formatTicks[Charting`ScaledTicks["Log10"][min, max, {OptionValue[numberOfMajorTicks2], OptionValue[numberOfMinorTicksPerMajorTick2]}], opts];
ticks["Log2", min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := formatTicks[Charting`ScaledTicks["Log2"][min, max, {OptionValue[numberOfMajorTicks2], OptionValue[numberOfMinorTicksPerMajorTick2]}], opts];

(* Special method for handling discrete ticks (i.e. things like strings) *)
ticks["Discrete", lbls_?ListQ, opts : OptionsPattern[]] := Module[{numberOfLabels, majorTicks, minorTicks, allTicks},
  numberOfLabels = Length[lbls];
  {majorTicks, minorTicks} = FindDivisions[{1, numberOfLabels}, {numberOfLabels, OptionValue[numberOfMinorTicksPerMajorTick2]}];
  majorTicks = MapIndexed[Function[{value, index}, {value, lbls[[First@index]], OptionValue[majorTickLength2], OptionValue[majorTickStyle2]}], majorTicks];
  minorTicks = Map[Function[{value}, {value, "", OptionValue[minorTickLength2], OptionValue[minorTickStyle2]}], DeleteDuplicates[Flatten[minorTicks]]];
  allTicks = Join[majorTicks, minorTicks];
  allTicks
];

(* TODO: Add support for directly passing in a list of major and minor ticks similar to how we did it for linearTicks in Alex *)

End[];

EndPackage[];
