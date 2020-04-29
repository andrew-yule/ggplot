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
gridLines[min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := gridLines[ticks[min, max, FilterRules[{opts}, Options[ticks]]], FilterRules[{opts}, Options[gridLines]]];
gridLines[func_?StringQ, min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := gridLines[ticks[func, min, max, FilterRules[{opts}, Options[ticks]]], FilterRules[{opts}, Options[gridLines]]];

End[];

EndPackage[];
