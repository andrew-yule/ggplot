(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* TODO: Need much more error trapping in all of this and need to define more explicity what is classified into discrete vs. continuous data *)

Attributes[reconcileXScales] = {HoldAll};
(*reconcileXScales[args___] /; discreteScaleQ["x", args] := "Discrete";*) (* Want discrete checking to be caught first as it's the most restrictive *)
reconcileXScales[args___] /; Count[args, scaleXDate[___], Infinity] > 0 := "Date";
reconcileXScales[args___] /; Count[args, scaleXLog[___], Infinity] > 0 := "Log";
reconcileXScales[___] := "Linear";

Attributes[reconcileYScales] = {HoldAll};
(*reconcileXScales[args___] /; discreteScaleQ["y", args] := "Discrete";*) (* Want discrete checking to be caught first as it's the most restrictive *)
reconcileYScales[args___] /; Count[args, scaleYDate[___], Infinity] > 0 := "Date";
reconcileYScales[args___] /; Count[args, scaleYLog[___], Infinity] > 0 := "Log";
reconcileYScales[___] := "Linear";

(* Logic to determine if a scale needs to be discrete *)
(* Note: only looks for NumericQ.. to validate discrete data or not. TODO: improve this*)
Attributes[discreteScaleQ] = {HoldAll};
discreteScaleQ[xOrY_, args___] := Module[{dataset, key, discreteQ},
    dataset = First@Cases[args, ("data" -> ds_) :> ds, Infinity];
    key = First@Cases[args, (xOrY -> val_) :> val, Infinity];
    discreteQ = !MatchQ[dataset[[All, key]], {_?NumericQ..}];
    discreteQ
];

(* Returns an association which will ultimately be used a pseudo scaling function. The keys are the labels and the values are numeric integers *)
(* for example <| discreteLbl1 -> 1, discreteLbl2 -> 2, ... discreteLblN -> N |> *)
(* The keys will be passed as labels into ticks["Discrete", lbls], and the association will scale the labels into X or Y values. *)
createDiscreteScaleFunc[xOrY_, args___] := Module[{dataset, key, func},
    dataset = First@Cases[args, ("data" -> ds_) :> ds, Infinity];
    key = First@Cases[args, (xOrY -> val_) :> val, Infinity];
    func = Association@MapIndexed[Function[{value, index}, value -> First@index], DeleteDuplicates[dataset[[All, key]]]];
    func
];

End[];

EndPackage[];
