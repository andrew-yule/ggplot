(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* Main tick creation function *)
(* TODO: the options here are still created and referenced in Alex, need to reconcile that *)
ticks[list_?ListQ, opts : OptionsPattern[]] := ReplaceAll[list, {
  (*Major ticks*)
  {value_?NumericQ, display : Except[(_?StringQ | _Spacer)], tickDistance_} :> {value, display, OptionValue[majorTickLength2], OptionValue[majorTickStyle2]},
  (*Minor ticks*)
  {value_?NumericQ, display : (_?StringQ | _Spacer), tickDistance_} :> {value, display, OptionValue[minorTickLength2], OptionValue[minorTickStyle2]}
}];
ticks[min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := ticks[Charting`ScaledTicks["Identity"][min, max, {OptionValue[numberOfMajorTicks2], OptionValue[numberOfMinorTicksPerMajorTick2]}], opts];
ticks[func_?StringQ, min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := ticks[Charting`ScaledTicks[func][min, max, {OptionValue[numberOfMajorTicks2], OptionValue[numberOfMinorTicksPerMajorTick2]}], opts];


End[];

EndPackage[];
