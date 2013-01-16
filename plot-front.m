(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



<<".mathrc";


colorData = ColorData[3];


dpi = 72;


myMarkerSize = .4 dpi;


circle[] := Graphics[Circle[], ImageSize -> myMarkerSize]


box[] := Graphics[{EdgeForm[Black], Transparent, Rectangle[]}, ImageSize->myMarkerSize]


plotFront[points_, index_:1] := Module[{p1, p2, p3},
p1 = ListPlot[points, Joined ->True, PlotLegends -> LineLegend[{"front"}], PlotRangePadding -> Scaled[.1]];
p2 = ListPlot[Map[List, points], PlotStyle -> Map[colorData, Range[Length[points]]],
PlotMarkers -> {Automatic, Medium}, PlotLegends -> Map[ToString,Range[Length[points]]]];
p3 = ListPlot[{points[[index]]},
PlotMarkers -> {circle[]}, PlotLegends -> {"current"}, PlotStyle -> Black];
Show[p1,p2, p3]]


plotPoint[point_, opts:OptionsPattern[Options[ListPlot]]] := ListPlot[{point},PlotMarkers -> {Automatic, Medium}, Evaluate[FilterRules[{opts},Options[ListPlot]]]]


plotFrontAndPoint[points_, index_, lastPoint_] :=
Show[{plotFront[points, index], plotPoint[lastPoint, PlotLegends ->{"previous"}, PlotStyle -> {colorData[ 1+Length[points]]}]}, PlotRange -> All]


exportPDF[filename_, expr_] := Export[filename, expr,"PDF", ImageSize ->2  pdfImageSize]


mathematicaToSexp[x_RGBColor] := ("#"<>mathematicaToSexp[Apply[List,Append[x,1.]]])


mathematicaToSexp[x_List] := ("(" <> StringJoin[Riffle[Map[mathematicaToSexp,x], " "]]<> ")")


mathematicaToSexp[x_Real] := ToString[x]


mathematicaToSexp[x_Integer] := ToString[x]


path[points_] := Line[points]


box[pos_, dims_] := {Transparent, Cuboid[pos - dims/2, pos + dims/2]}


plotRobotPathAndObstacles[points_, obstacles_] :=
Graphics3D[{path[points], Map[box@@#&,obstacles], {PointSize[Large],Green, Opacity[0.5], Point[points[[-1]]]}},Axes->{True, False, True},AxesLabel->{"x","y","z"},ViewPoint->{0,Infinity,0},ViewVertical->{0,0,-1}, PlotRangePadding -> 2]


plotFitnessTimeSeries[results_] := ListPlot[Transpose[Map[Function[{input}, Map[ {input[[1]], #}&,input[[2]]]],results[[All,{1,3}]]]], Joined -> True, PlotRange -> All, AxesLabel -> {"generation", "fitness"}, AxesOrigin -> {1, 0}]



