Get["figlib.wl"];
basic = AssociationMap[solveCase[#] /. gg -> g &, cases];
par[gv_] := {a -> 1, b -> 0.2, rho -> 0.3, th -> 1.4, cg -> 0.02, k -> 0.4, g -> gv};
out = "out/";
(* Figure 1: platform's optimal format and blockchain choice; labels mark regions with (lower) and without (upper) blockchain *)
ev3 = mkEval[basic, par[0.3], #["pip"] &, False];
ev6 = mkEval[basic, par[0.6], #["pip"] &, False];
Export[out <> "Fig1a.pdf", regionPlot[ev3, {{"RR", {0.13, 0.5}}, {"RA", {0.37, 0.52}}, {"AA", {0.66, 0.5}}, {"AR", {0.969, 0.52}, 12},
    {"RR", {0.1, 0.95}}, {"RA", {0.37, 0.95}}, {"AA", {0.7, 0.95}}, {"AR", {0.969, 0.95}, 12}}, {0.3, 0.05}]];
Export[out <> "Fig1b.pdf", regionPlot[ev6, {{"RR", {0.12, 0.43}}, {"RA", {0.39, 0.44}}, {"AA", {0.66, 0.44}}, {"AR", {0.92, 0.43}},
    {"RR", {0.1, 0.95}}, {"RA", {0.43, 0.95}}, {"AA", {0.75, 0.95}}, {"AR", {0.94, 0.95}, 12}}, {0.3, 0.05}]];
(* Figure CS1: configuration maximizing perceived CS (feasible configurations only) *)
evc = mkEval[basic, par[0.6], cs, False];
Export[out <> "CS1.pdf", regionPlot[evc, {{"NAA", {0.2, 0.9}}, {"BAA", {0.2, 0.47}}, {"NAR", {0.92, 0.92}}, {"BAR", {0.92, 0.5}}}, {0.06, 0.955}, 0.03]];
(* Figure CS2: change in perceived CS from blockchain, lambda = 0.2, g = 0.6 *)
dcs[f_] := (cs[basic["B" <> f]] - cs[basic["N" <> f]]) /. par[0.6] /. lam -> 0.2;
sty = {Directive[blue, AbsoluteThickness[1.8]], Directive[blue, AbsoluteThickness[1.8], AbsoluteDashing[{2, 3}]],
   Directive[orange, AbsoluteThickness[1.6], AbsoluteDashing[{7, 3, 1.5, 3, 1.5, 3}]], Directive[orange, AbsoluteThickness[1.8], AbsoluteDashing[{12, 5}]]};
Export[out <> "CS2.pdf", Plot[Evaluate[dcs /@ {"RR", "RA", "AR", "AA"}], {gam, 0, 1}, PlotStyle -> sty,
   PlotLegends -> Placed[LineLegend[sty, {"RR", "RA", "AR", "AA"}, LabelStyle -> fnt, LegendMarkerSize -> {35, 15}], {0.85, 0.8}],
   Epilog -> {GrayLevel[0.6], AbsoluteDashing[{1, 2}], Line[{{0, 0}, {1, 0}}]},
   Frame -> True, FrameLabel -> {Style["\[Gamma]", Italic, lblSize, Black], Style["\[CapitalDelta]CS", 15, Black]}, FrameStyle -> Black,
   FrameTicksStyle -> Directive[Black, tickSize], PlotRange -> All, AspectRatio -> 1, ImageSize -> 400]];
(* Figure CS3: change in realized CS from blockchain (utility evaluated at true greenness g), lambda = 0.2, g = 0.6 *)
realCS[c_] := With[{r = basic[c]}, If[StringTake[c, 1] == "B", cs[r],
    cs[r] - (gam (1 + r["be"]) g - g) (r["Dg"] + b r["Do"])/(1 - b^2)]];
dcsR[f_] := (realCS["B" <> f] - realCS["N" <> f]) /. par[0.6] /. lam -> 0.2;
Print["realized dCS at gamma = 0.3, 0.95: ", Table[{f, dcsR[f] /. gam -> 0.3, dcsR[f] /. gam -> 0.95}, {f, {"RR", "RA", "AR", "AA"}}]];
Export[out <> "CS3.pdf", Plot[Evaluate[dcsR /@ {"RR", "RA", "AR", "AA"}], {gam, 0, 1}, PlotStyle -> sty,
   PlotLegends -> Placed[LineLegend[sty, {"RR", "RA", "AR", "AA"}, LabelStyle -> fnt, LegendMarkerSize -> {35, 15}], {0.2, 0.8}],
   Epilog -> {GrayLevel[0.6], AbsoluteDashing[{1, 2}], Line[{{0, 0}, {1, 0}}]},
   Frame -> True, FrameLabel -> {Style["\[Gamma]", Italic, lblSize, Black], Style["\!\(\*SuperscriptBox[\(\[CapitalDelta]CS\), \(R\)]\)", 15, Black]}, FrameStyle -> Black,
   FrameTicksStyle -> Directive[Black, tickSize], PlotRange -> All, AspectRatio -> 1, ImageSize -> 400]];
(* duibi: platform-optimal format (no blockchain) vs greenwashing-minimizing format, g = 0.3 *)
nb = KeyTake[basic, Take[cases, 4]];
evP = mkEval[nb, par[0.3], #["pip"] &, False]; evB = mkEval[nb, par[0.3], -#["be"] &, False];
idxOf[ev_][l_?NumericQ, gm_?NumericQ] := With[{c = bestCase[ev, l, gm]}, If[c === None, 0, order[fmt[c]]]];
lab[t_, s_, p_, fs_: 14] := {Text[Style[t, FontFamily -> "Arial", FontSize -> fs, FontColor -> Black], p + {0, 0.028}],
   Text[Style["(" <> s <> ")", FontFamily -> "Arial", FontSize -> fs, FontColor -> Gray], p - {0, 0.028}]};
labs = Table[{l, bestCase[evP, l, 0.62], bestCase[evB, l, 0.62]}, {l, 0.01, 0.99, 0.005}];
Print["duibi scan at gamma=0.62: ", DeleteDuplicatesBy[labs, Rest]];
Export[out <> "duibi.pdf", Show[
   Graphics[{Black, AbsoluteThickness[2], boundaryLines[idxOf[evP]]}],
   Graphics[{GrayLevel[0.65], AbsoluteThickness[2], AbsoluteDashing[{5, 3}], boundaryLines[idxOf[evB]]}],
   Graphics[{lab @@@ {{"RR", "RR", {0.06, 0.88}, 12}, {"RR", "AR", {0.2, 0.43}}, {"RA", "AR", {0.37, 0.9}}, {"AA", "AR", {0.45, 0.25}}, {"AA", "AA", {0.7, 0.47}}, {"AR", "AR", {0.945, 0.15}, 12}},
     Black, AbsoluteThickness[2], Line[{{0.55, 0.97}, {0.61, 0.97}}], Text[Style["Platform", Sequence @@ fnt], {0.63, 0.97}, {-1, 0}],
     GrayLevel[0.65], AbsoluteDashing[{5, 3}], Line[{{0.55, 0.89}, {0.61, 0.89}}], Text[Style["Greenwashing", Sequence @@ fnt], {0.63, 0.89}, {-1, 0}]}],
   Frame -> True, FrameLabel -> {Style["\[Lambda]", Italic, lblSize, Black], Style["\[Gamma]", Italic, lblSize, Black]}, FrameStyle -> Black,
   FrameTicksStyle -> Directive[Black, tickSize], PlotRange -> {{0, 1}, {0, 1}}, AspectRatio -> 1, ImageSize -> 400]];
