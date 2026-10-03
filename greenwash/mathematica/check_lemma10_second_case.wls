Get["figlib.wl"];
ext = AssociationMap[solveCase[#, True] &, {"BRR", "BRA", "BAR"}];
Do[Module[{r = ext[c], num, den, kt, ks, ct, tot = 0, okP = 0, okAll = 0, failp = 0},
  {num, den} = NumeratorDenominator[Together[r["g"]]];
  kt = k /. First@Solve[den == 0, k]; ct = cg /. First@Solve[num == 0, cg];
  ks = Switch[c, "BRA", (1 - lam)/(2 - b^2 (1 + lam)), _, 1/4];
  SeedRandom[7];
  Do[Module[{p = {a -> 1, b -> RandomReal[{0.02, 0.98}], rho -> RandomReal[{0.01, 0.99}], lam -> RandomReal[{0.01, 0.99}]}, kk, cc, prices, profs},
     kk = RandomReal[{ks, kt} /. p]; cc = (ct /. p) Exp[RandomReal[{0.001, 4}]]; tot++;
     prices = And @@ ({r["g"] > 0, r["po"] > 0, r["pg"] > 0, r["Do"] > 0, r["Dg"] > 0} /. p /. {k -> kk, cg -> cc});
     profs = And @@ ({r["pio"] >= 0, r["pig"] >= 0, r["pip"] >= 0} /. p /. {k -> kk, cg -> cc});
     If[prices, okP++; If[profs, okAll++, If[(r["pip"] /. p /. {k -> kk, cg -> cc}) < 0, failp++]]]], {5000}];
  Print[c, ": prices/demands ok ", okP, "/", tot, "; also all profits >= 0: ", okAll, "; platform profit < 0 among price-ok: ", failp]], {c, Keys[ext]}];
