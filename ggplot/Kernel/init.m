(* Mathematica Init file    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

Get["ggplot`ggplotSymbolDeclaration`"];
Get["ggplot`ggplot`"];

Get["ggplot`ticks`"];
Get["ggplot`gridLines`"];

Get["ggplot`aes`"];
Get["ggplot`aesColor`"];
Get["ggplot`aesSize`"];
Get["ggplot`aesAlpha`"];
Get["ggplot`aesShape`"];
Get["ggplot`aesThickness`"];

Get["ggplot`geomPoint`"];
Get["ggplot`geomLine`"];
Get["ggplot`geomSmooth`"];
Get["ggplot`geomCol`"];
Get["ggplot`geomParityLine`"];
Get["ggplot`geomHLine`"];
Get["ggplot`geomVLine`"];
Get["ggplot`geomHistogram`"];

Get["ggplot`scale`"];
Get["ggplot`scaleDate`"];
Get["ggplot`scaleLog`"];

Get["ggplot`legend`"];

Echo[Style["ggplot v" <> ToString[PacletFind["ggplot"][[1]]["Version"]]]];