(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* TODO: Need much more error trapping in all of this and need to define more explicity what is classified into discrete vs. continuous data *)

Attributes[reconcileXScales] = {HoldAll};
reconcileXScales[args___] /; discreteScaleQ["x", args] := "Discrete"; (* Want discrete checking to be caught first as it's the most restrictive *)
reconcileXScales[args___] /; Count[args, scaleXDate[___], Infinity] > 0 := "Date";
reconcileXScales[args___] /; Count[args, scaleXLog[___], Infinity] > 0 := "Log";
reconcileXScales[___] := "Linear";

Attributes[reconcileYScales] = {HoldAll};
reconcileYScales[args___] /; discreteScaleQ["y", args] := "Discrete"; (* Want discrete checking to be caught first as it's the most restrictive *)
reconcileYScales[args___] /; Count[args, scaleYDate[___], Infinity] > 0 := "Date";
reconcileYScales[args___] /; Count[args, scaleYLog[___], Infinity] > 0 := "Log";
reconcileYScales[___] := "Linear";

(* Logic to determine if a scale needs to be discrete *)
(* Note: only looks for NumericQ.. to validate discrete data or not. TODO: improve this*)
Attributes[discreteScaleQ] = {HoldAll};
discreteScaleQ[xOrY_, args___] /; Count[args, (xOrY -> _), Infinity] == 0 := False; (* Catch all for situations like geomHistogram where y may not be given. TODO: make this more robust*)
discreteScaleQ[xOrY_, args___] := Module[{dataset, key, discreteQ},
    dataset = First@Cases[args, ("data" -> ds_) :> ds, Infinity];
    dataset = Replace[dataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];
    key = First@Cases[args, (xOrY -> val_) :> val, Infinity];
    discreteQ = !MatchQ[dataset[[All, key]], {_?NumericQ..}];
    discreteQ
];

(* Get the labels associated with a discrete data set *)
Attributes[createDiscreteScaleLabels] = {HoldAll};
createDiscreteScaleLabels[xOrY_, args___] := Module[{dataset, key, allValues, func},
    dataset = First@Cases[args, ("data" -> ds_) :> ds, Infinity];
    dataset = Replace[dataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];
    key = First@Cases[args, (xOrY -> val_) :> val, Infinity];
    allValues = DeleteDuplicates[dataset[[All, key]]];
    allValues
];

(* Returns an association which will ultimately be used a pseudo scaling function. The keys are the labels and the values are numeric integers *)
(* for example <| discreteLbl1 -> 1, discreteLbl2 -> 2, ... discreteLblN -> N |> *)
(* The keys will be passed as labels into ticks["Discrete", lbls], and the association will scale the labels into X or Y values. *)
Attributes[createDiscreteScaleFunc] = {HoldAll};
createDiscreteScaleFunc[xOrY_, args___] := Module[{dataset, key, allValues, func},
    dataset = First@Cases[args, ("data" -> ds_) :> ds, Infinity];
    dataset = Replace[dataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];
    key = First@Cases[args, (xOrY -> val_) :> val, Infinity];
    (* Using some "hacky" code here to create a function that contains a switch statement *)
    allValues = DeleteDuplicates[dataset[[All, key]]];
    func = With[{expr = Append[#]@Append[_?NumericQ]@Prepend[#]@Flatten@Table[{allValues[[i]], i}, {i, Length@allValues}]},
        Function[Switch @@ expr]
    ];
    func
];

End[];

EndPackage[];
