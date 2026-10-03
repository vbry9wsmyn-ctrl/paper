Get["model.wl"];
ext = AssociationMap[solveCase[#, True] &, cases];
kap = 2 k th/(k + 2 th);
Do[Module[{n = ext["N" <> f], bb = ext["B" <> f] /. {cg -> 0, k -> kap/gam^2}},
   Print[f, ": pi_p,pi_o,pi_g,CS  N - B(k'=kappa/gam^2): ",
    Simplify[{n["pip"] - bb["pip"], n["pio"] - bb["pio"], n["pig"] - bb["pig"], cs[n] - cs[bb]}],
    "   perceived G: ", Simplify[gam (n["g"] (1 + n["be"])) - bb["g"]]]], {f, {"RR", "RA", "AR", "AA"}}];
(* monotonicity of blockchain-case profits in k on a grid satisfying feasibility *)
socCond[h_] := Table[(-1)^n Det[h[[;; n, ;; n]]] > 0, {n, Length[h]}];
Do[Module[{r = ext["B" <> f], bad = 0, tot = 0},
   Do[With[{p = {a -> 1, b -> bb, rho -> rr, lam -> ll, k -> kk, cg -> 0}},
     If[And @@ Join[{r["g"] > 0, r["pio"] >= 0, r["pig"] >= 0, r["pip"] >= 0, r["po"] > 0, r["pg"] > 0, r["Do"] > 0, r["Dg"] > 0}, socCond[r["hess"]]] /. p,
      tot++; If[Max[D[{r["pip"], r["pio"], r["pig"]}, k] /. p] >= 0, bad++]]],
    {bb, {0.1, 0.3, 0.5, 0.7, 0.9}}, {rr, {0.2, 0.4, 0.6, 0.8}}, {ll, {0.05, 0.2, 0.4, 0.6, 0.8, 0.95}}, {kk, {0.3, 0.6, 1, 2, 4}}];
   Print["B", f, ": feasible points ", tot, ", with some d(profit)/dk >= 0: ", bad]], {f, {"RR", "RA", "AR", "AA"}}];
Print["d pi_p^BAA/dk sign check symbolic: ", Simplify[D[ext["BAA"]["pip"] /. cg -> 0, k]]];
