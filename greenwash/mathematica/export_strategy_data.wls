Get["figlib.wl"];

basic = AssociationMap[solveCase[#] /. gg -> g &, cases];
ext = AssociationMap[solveCase[#, True] &, cases];
basicPar[gv_] := {a -> 1, b -> 0.2, rho -> 0.3, th -> 1.4, cg -> 0.02, k -> 0.4, g -> gv};
extPar = {a -> 1, b -> 0.2, rho -> 0.3, th -> 1.4, cg -> 0.02, k -> 1.2};

ev3 = mkEval[basic, basicPar[0.3], #["pip"] &, False];
ev6 = mkEval[basic, basicPar[0.6], #["pip"] &, False];
evExt = mkEval[ext, extPar, #["pip"] &, True];

data = <|
  "fig1a" -> regionData[ev3],
  "fig1b" -> regionData[ev6],
  "fig2" -> regionData[evExt],
  "gammaHat" -> N[Sqrt[2 th/(k + 2 th)] /. extPar]
|>;

Export["out/strategy_data.json", data, "JSON"];
Print["Exported strategy data for Fig1a, Fig1b, and Fig2."];
