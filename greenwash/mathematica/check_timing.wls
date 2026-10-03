Get["figlib.wl"];
std = AssociationMap[solveCaseStd[#] /. gg -> g &, cases];
stdE = AssociationMap[solveCaseStd[#, True] &, cases];
curE = AssociationMap[solveCase[#, True] &, cases];
thrOf[r_] := Module[{d = Denominator[Together[r["be"]]]}, th /. Solve[d == 0, th]];
(* (a) greenwashing ranking *)
SeedRandom[21]; n = 0; cnt = <|"RAmin" -> 0, "RAnotmax" -> 0, "minHighLam" -> 0, "multiSwitch" -> 0|>; lowSeq = {};
Do[Module[{p, t0, bs, mn, mx, l, seq},
  p = {a -> 1, b -> RandomReal[{0.02, 0.95}], rho -> RandomReal[{0.02, 0.98}], g -> RandomReal[{0.05, 1.5}], gam -> RandomReal[{0.05, 0.99}]};
  t0 = Max[Flatten[Table[thrOf[std[c]] /. p /. lam -> l, {c, {"NRR", "NRA", "NAR", "NAA"}}, {l, {0.01, 0.5, 0.99}}]]];
  p = Append[p, th -> t0 Exp[RandomReal[{0.05, 4}]]];
  seq = Table[Module[{v = Map[(#["be"] /. p /. lam -> l) &, KeyTake[std, {"NRR", "NRA", "NAR", "NAA"}]]},
      If[Min[v] <= 0, Nothing, {l, First@Keys@TakeSmallest[v, 1], First@Keys@TakeLargest[v, 1]}]], {l, 0.01, 0.99, 0.02}];
  n++;
  If[MemberQ[seq[[All, 2]], "NRA"], cnt["RAmin"]++];
  If[AnyTrue[Select[seq, #[[1]] <= 0.5 &], #[[3]] =!= "NRA" &], cnt["RAnotmax"]++];
  If[AnyTrue[Select[seq, #[[1]] >= 0.5 &], ! MemberQ[{"NAA", "NAR"}, #[[2]]] &], cnt["minHighLam"]++];
  If[Length[Split[Select[seq, #[[1]] <= 0.5 &][[All, 2]]]] > 2, cnt["multiSwitch"]++]], {1500}];
Print["(a) std timing ranking, ", n, " parameter draws x 50 lambdas: ", cnt];
(* (b) realized CS proposition *)
SeedRandom[22]; o = u = bo = bu = 0;
Do[Module[{f = RandomChoice[{"RA", "AR"}], p, nN, nB, d, dcs},
  p = {a -> 1, b -> RandomReal[{0.02, 0.95}], rho -> RandomReal[{0.02, 0.98}], g -> RandomReal[{0.05, 1.5}], gam -> RandomReal[{0.05, 0.99}], lam -> RandomReal[{0.01, 0.99}], cg -> 0, k -> 0};
  nN = std["N" <> f]; nB = std["B" <> f];
  p = Append[p, th -> (thrOf[nN] /. p)[[1]] Exp[RandomReal[{0.05, 5}]]];
  If[And @@ ({nN["Do"] > 0, nN["Dg"] > 0, nB["Do"] > 0, nB["Dg"] > 0, nN["pg"] > 0, nN["po"] > 0, nB["pg"] > 0, nB["po"] > 0, nN["be"] > 0} /. p),
   d = (gam (1 + nN["be"]) - 1) /. p; dcs = (cs[nB] - csReal[nN /. gg -> g, "N"] /. gg -> g) /. p;
   If[d > 0, o++; If[dcs <= 0, bo++]]; If[d < 0, u++; If[dcs < 0, bu++]]]], {4000}];
Print["(b) std timing realized CS (RA/AR): overstatement ", o, " with dCS<=0: ", bo, "; understatement ", u, " with dCS<0: ", bu];
(* (c) greenness ranking under blockchain *)
SeedRandom[23]; tot = bad = 0;
Do[Module[{p, v},
  p = {a -> 1, b -> RandomReal[{0.02, 0.95}], rho -> RandomReal[{0.02, 0.98}], lam -> RandomReal[{0.01, 0.5}], cg -> RandomReal[{0, 0.3}], k -> RandomReal[{0.3, 5}]};
  v = Map[(#["g"] /. p) &, KeyTake[stdE, {"BRR", "BRA", "BAR", "BAA"}]];
  If[Min[Values[v]] > 0 && AllTrue[{"BRR", "BRA", "BAR", "BAA"}, And @@ (socCond[stdE[#]["hess"]] /. p) &], tot++; If[First@Keys@TakeLargest[v, 1] =!= "BRA", bad++]]], {4000}];
Print["(c) std timing greenness: feasible ", tot, ", RA not highest ", bad];
(* (d) composition and equivalence *)
kap = 2 k th/(k + 2 th);
Print["(d) std timing beta^N (ext): ", Union[Simplify[stdE[#]["be"]] & /@ {"NRR", "NRA", "NAR", "NAA"}],
  "  equivalence diffs: ", Simplify[Table[stdE["N" <> f]["pip"] - (stdE["B" <> f]["pip"] /. {cg -> 0, k -> kap/gam^2}), {f, {"RA", "AR"}}]]];
