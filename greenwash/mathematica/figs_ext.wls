Get["figlib.wl"];
ext = AssociationMap[solveCase[#, True] &, cases];
par = {a -> 1, b -> 0.2, rho -> 0.3, cg -> 0.02, k -> 1.2, th -> 1.4};
gHat = Sqrt[2 th/(k + 2 th)] /. par;
Print["gamma hat = ", gHat, "  beta* = ", k/(2 th) /. par];
out = "out/";
ev = mkEval[ext, par, #["pip"] &, True];
Print["scan gamma=0.5: ", DeleteDuplicatesBy[Table[{l, bestCase[ev, l, 0.5]}, {l, 0.005, 0.995, 0.005}], Last]];
Print["scan gamma=0.9: ", DeleteDuplicatesBy[Table[{l, bestCase[ev, l, 0.9]}, {l, 0.005, 0.995, 0.005}], Last]];
(* Figure 5: endogenous greenness, platform's optimal format and blockchain choice; dotted line = costless-verification threshold *)
fig2 = regionPlot[ev, {{"RR", {0.08, 0.45}}, {"RA", {0.3, 0.45}}, {"AA", {0.65, 0.45}}, {"RR", {0.06, 0.91}}, {"RA", {0.36, 0.91}}, {"AA", {0.78, 0.96}}},
   {0.3, 0.05}, 0.001,
   Epilog -> {GrayLevel[0.3], AbsoluteDashing[{2, 3}], AbsoluteThickness[1], Line[{{0, gHat}, {1, gHat}}],
     Text[Style["\!\(\*OverscriptBox[\(\[Gamma]\), \(^\)]\) (\!\(\*SubscriptBox[\(c\), \(g\)]\)=0)", FontFamily -> "Arial", FontSize -> 14, Black], {0.64, gHat + 0.035}, {-1, 0}]}];
Export[out <> "Fig2.pdf", fig2];
(* Figure 5: greenwashing e = beta g and greenness g along gamma under the platform's optimal configuration *)
lams = {0.05, 0.3, 0.7}; dash = {{}, {3, 3}, {10, 4}};
val[l_, gm_, key_] := With[{c = bestCase[ev, l, gm]}, {c, ext[c][key] /. par /. {lam -> l, gam -> gm}}];
seg[l_, key_] := Module[{pts = Table[{gm, val[l, gm, key]}, {gm, 0.001, 0.999, 0.002}]},
   {#[[1, 2, 1]], {#[[1]], #[[2, 2]]} & /@ #} & /@ SplitBy[pts, #[[2, 1]] &]];
eKey[r_] := r["g"] r["be"];
ext = Map[Append[#, "e" -> #["g"] #["be"]] &, ext];
mk5[key_, ylab_, labpos_] := Graphics[{
    Table[{AbsoluteThickness[1.8], AbsoluteDashing[dash[[i]]],
      {If[StringTake[#[[1]], 1] == "B", blue, orange], Line[#[[2]]]} & /@ seg[lams[[i]], key]}, {i, 3}],
    Table[Text[Style["\[Lambda]=" <> ToString[lams[[i]]], FontFamily -> "Arial", FontSize -> 14, Black], labpos[[i]]], {i, 3}]},
   Axes -> True, AxesLabel -> {Style["\[Gamma]", Italic, lblSize, Black], Style[ylab, Italic, lblSize, Black]}, AxesStyle -> Black,
   TicksStyle -> Directive[Black, tickSize], AspectRatio -> 1/GoldenRatio, ImageSize -> 400, AxesOrigin -> {0, 0}, PlotRange -> {{0, 1.02}, {0, All}}];
Print["configs along gamma: ", Table[DeleteDuplicates[bestCase[ev, l, #] & /@ Range[0.001, 0.999, 0.002]], {l, lams}]];
Print["e at gamma=0.95: ", Table[val[l, 0.95, "e"], {l, lams}], "  g: ", Table[val[l, 0.95, "g"], {l, lams}], "  g at 0.3: ", Table[val[l, 0.3, "g"], {l, lams}]];
Export[out <> "fig51.pdf", mk5["e", "\[Beta]g", {{0.9, 0.104}, {0.88, 0.157}, {0.9, 0.024}}]];
Export[out <> "fig52.pdf", mk5["g", "g", {{0.3, 0.2}, {0.3, 0.333}, {0.3, 0.118}}]];
