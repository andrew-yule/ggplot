(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

Attributes[reconcileXScales] = {HoldAll};
reconcileXScales[geoms___] /; Count[{geoms}, scaleXDate[___], Infinity] > 0 := "Date";
reconcileXScales[geoms___] /; Count[{geoms}, scaleXLog[___], Infinity] > 0 := "Log";
reconcileXScales[___] := "Linear";

Attributes[reconcileYScales] = {HoldAll};
reconcileYScales[geoms___] /; Count[{geoms}, scaleYDate[___], Infinity] > 0 := "Date";
reconcileYScales[geoms___] /; Count[{geoms}, scaleYLog[___], Infinity] > 0 := "Log";
reconcileYScales[___] := "Linear";

End[];

EndPackage[];
