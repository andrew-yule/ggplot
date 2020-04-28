(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"];

(* Helper functions for aesthetics *)

isDiscreteDataQ[data_]      := If[MatchQ[DeleteDuplicates[data], {_?StringQ ..}], True, False];
getAllKeys[data_]           := data // Keys /* Flatten /* DeleteDuplicates;
getDiscreteKeys[data_]      := Sort[DeleteDuplicates[data]];
getContinuousRange[data_]   := MinMax[data];
keyExistsQAll[data_, key_]  := Lookup[data, Key[key], False] // ContainsAny[{False}] // Not;


End[];

EndPackage[];
