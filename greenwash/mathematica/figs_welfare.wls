(* Figure W1: change in total welfare (realized CS + all profits) from blockchain, lambda = 0.2, g = 0.6; same settings as CS3 *)
$OnlyPaperFigure2 = True;
Get["figlib.wl"];
basic = AssociationMap[solveCase[#] &, cases];
par = {a -> 1, b -> 0.2, rho -> 0.3, th -> 1.4, cg -> 0.02, k -> 0.4, gg -> 0.6, lam -> 0.2};
dW[f_] := (welfare[basic["B" <> f], "B" <> f] - welfare[basic["N" <> f], "N" <> f]) /. par;
sty = {Directive[blue, AbsoluteThickness[1.8]], Directive[blue, AbsoluteThickness[1.8], AbsoluteDashing[{2, 3}]],
   Directive[orange, AbsoluteThickness[1.6], AbsoluteDashing[{7, 3, 1.5, 3, 1.5, 3}]], Directive[orange, AbsoluteThickness[1.8], AbsoluteDashing[{12, 5}]]};
Print["dW zero crossings: ", Table[{f, gam /. FindRoot[dW[f] == 0, {gam, 0.88}]}, {f, {"RR", "RA", "AR", "AA"}}]];
Print["perceived = true (delta=0) points: ", Table[{f, gam /. FindRoot[(gam (1 + basic["N" <> f]["be"]) - 1) /. par, {gam, 0.8}]}, {f, {"RR", "RA", "AR", "AA"}}]];
Export["out/W1.pdf", Plot[Evaluate[dW /@ {"RR", "RA", "AR", "AA"}], {gam, 0, 1}, PlotStyle -> sty,
   PlotLegends -> Placed[LineLegend[sty, {"RR", "RA", "AR", "AA"}, LabelStyle -> fnt, LegendMarkerSize -> {35, 15}], {0.8, 0.8}],
   Epilog -> {GrayLevel[0.6], AbsoluteDashing[{1, 2}], Line[{{0, 0}, {1, 0}}]},
   Frame -> True, FrameLabel -> {Style["\[Gamma]", Italic, lblSize, Black], Style["\[CapitalDelta]W", 15, Black]}, FrameStyle -> Black,
   FrameTicksStyle -> Directive[Black, tickSize], PlotRange -> All, AspectRatio -> 1, ImageSize -> 400]];
