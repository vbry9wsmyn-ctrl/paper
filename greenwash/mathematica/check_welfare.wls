Get["figlib.wl"];
basic = AssociationMap[solveCase[#] &, cases];
par = {a -> 1, b -> 0.2, rho -> 0.3, th -> 0.5, k -> 0.4, cg -> 0.02, gg -> 0.6, lam -> 0.2};
dW[f_] := (welfare[basic["B" <> f], "B" <> f] - welfare[basic["N" <> f], "N" <> f]) /. par;
dCS[f_] := (csReal[basic["B" <> f], "B" <> f] - csReal[basic["N" <> f], "N" <> f]) /. par;
dPi[f_] := (Total[basic["B" <> f] /@ {"pio", "pig", "pip"}] - Total[basic["N" <> f] /@ {"pio", "pig", "pip"}]) /. par;
Do[Print[f, " gamma: ", Table[{gm, Round[{dCS[f], dPi[f], dW[f]} /. gam -> gm, 0.0001]}, {gm, {0.1, 0.4, 0.6, 0.8, 0.9, 1}}]], {f, {"RR", "RA", "AR", "AA"}}];
(* c_g = 0 : sign of welfare change on random draws *)
SeedRandom[31]; pos = neg = 0; negOver = negUnder = 0;
Do[Module[{f = RandomChoice[{"RR", "RA", "AR", "AA"}], p, nN, nB, w, d},
  p = {a -> 1, b -> RandomReal[{0.02, 0.95}], rho -> RandomReal[{0.02, 0.98}], gg -> RandomReal[{0.05, 1.5}], gam -> RandomReal[{0.05, 0.99}], lam -> RandomReal[{0.01, 0.99}], cg -> 0, k -> 0};
  nN = basic["N" <> f]; nB = basic["B" <> f];
  p = Append[p, th -> (th /. First@Solve[Denominator[Together[nN["be"]]] == 0, th] /. p) Exp[RandomReal[{0.05, 5}]]];
  If[And @@ ({nN["Do"] > 0, nN["Dg"] > 0, nB["Do"] > 0, nB["Dg"] > 0, nN["pg"] > 0, nN["po"] > 0, nB["pg"] > 0, nB["po"] > 0} /. p),
   w = (welfare[nB, "B" <> f] - welfare[nN, "N" <> f]) /. p; d = (gam (1 + nN["be"]) - 1) /. p;
   If[w > 0, pos++, neg++; If[d > 0, negOver++, negUnder++]]]], {5000}];
Print["cg=0 welfare change: positive ", pos, " negative ", neg, " (of which overstatement ", negOver, ", understatement ", negUnder, ")"];
