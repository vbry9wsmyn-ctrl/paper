Get["model.wl"];
basic = AssociationMap[solveCase[#] /. gg -> g &, Take[cases, 4]];
thr = <|"NRR" -> gam^2 g^2/(2 (4 - b^2)), "NRA" -> 2 gam^2 (1 - lam) g^2/(8 - b^2 (3 lam + 5)),
   "NAR" -> gam^2 g^2 (4 - b^2 (3 lam + 1))/(4 (8 - b^2 (3 lam + 5))), "NAA" -> gam^2 (1 - lam) g^2/(4 - b^2)|>;
SeedRandom[3];
Do[Module[{r = basic[c], up = 0, dn = 0, n = 3000},
  Do[Module[{p = {a -> 1, b -> RandomReal[{0, 0.95}], rho -> RandomReal[{0.02, 0.98}], g -> RandomReal[{0.02, 2}], gam -> RandomReal[{0.02, 0.99}], lam -> RandomReal[{0.01, 0.99}]}, t0, vars},
     t0 = thr[c] /. p; vars = DeleteMissing[{r["wo"], r["wg"], r["po"], r["pg"], r["be"], r["Do"], r["Dg"]}];
     If[And @@ Thread[(vars /. p /. th -> t0 Exp[RandomReal[{0.001, 6}]]) > 0], up++];
     If[(r["be"] /. p /. th -> t0 RandomReal[{0.01, 0.999}]) < 0, dn++]], {n}];
  Print[c, ": above threshold all positive ", up, "/", n, " ; below threshold beta<0 ", dn, "/", n]], {c, Keys[basic]}];
