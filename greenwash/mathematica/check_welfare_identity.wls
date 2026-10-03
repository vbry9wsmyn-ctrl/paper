Get["figlib.wl"];
basic = AssociationMap[solveCase[#] &, cases];
Minv = Inverse[{{1, -b}, {-b, 1}}];
Do[Module[{nN = basic["N" <> f], nB = basic["B" <> f], dvec, pB, delta, lhs, rhs},
   dvec = D[{nB["Do"], nB["Dg"]}, gg];
   pB = {nB["po"], nB["pg"]};
   delta = gam (1 + nN["be"]) gg - gg;
   lhs = welfare[nB, "B" <> f] - welfare[nN, "N" <> f] /. cg -> 0;
   rhs = th nN["be"]^2 - delta dvec.pB + delta^2 dvec.Minv.dvec/2 /. cg -> 0;
   Print[f, ": identity residual = ", Simplify[lhs - rhs], "   d = ", Simplify[dvec]]], {f, {"RR", "RA", "AR", "AA"}}];
