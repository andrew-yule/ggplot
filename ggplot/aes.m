(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"];

(* Helper functions for aesthetics *)

isDiscreteDataQ[data_]      := MatchQ[DeleteDuplicates[data], {_?StringQ ..} | {_?BooleanQ ..}];
getAllKeys[data_]           := data // Keys /* Flatten /* DeleteDuplicates;
getDiscreteKeys[data_]      := Sort[DeleteDuplicates[data]];
getContinuousRange[data_]   := MinMax[data];
keyExistsQAll[data_, key_]  := data // Map[KeyExistsQ["XyleneQ"]] // MatchQ[{True ..}];

End[];

EndPackage[];
