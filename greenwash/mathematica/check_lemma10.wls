Get["figlib.wl"];
ext = AssociationMap[solveCase[#, True] &, cases];
Do[Module[{r = ext[c], den, num, kt, ct, cnt = 0, tot = 0},
  {num, den} = NumeratorDenominator[Together[r["g"]]];
  kt = k /. First@Solve[den == 0, k]; ct = cg /. First@Solve[num == 0, cg];
  Print[c, ": k~ = ", Simplify[kt], "   c~ = ", Simplify[ct]];
  Do[With[{p = {a -> 1, b -> bb, rho -> rr, lam -> ll}},
     With[{kk = (kt /. p) f1, cc = (ct /. p) f2},
      If[And @@ (socCond[r["hess"]] /. p /. k -> kk), tot++;
       If[And @@ ({r["g"] > 0, r["po"] > 0, r["pg"] > 0, r["Do"] > 0, r["Dg"] > 0, r["pig"] >= 0, r["pip"] >= 0, r["pio"] >= 0} /. p /. {k -> kk, cg -> cc}), cnt++]]]],
   {bb, {0.1, 0.4, 0.7}}, {rr, {0.2, 0.5, 0.8}}, {ll, {0.1, 0.5, 0.9}}, {f1, {0.5, 0.8, 0.95}}, {f2, {1.05, 1.5, 3}}];
  Print["   second branch (k<k~, cg>c~, SOC holds): ", tot, " points, fully feasible: ", cnt]], {c, {"BRR", "BRA", "BAR"}}];
