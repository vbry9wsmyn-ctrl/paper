Get["model.wl"];
basic = AssociationMap[solveCase[#] /. gg -> g &, cases];
par[gv_] := {a -> 1, b -> 0.2, rho -> 0.3, th -> 0.5, cg -> 0.02, k -> 0.4, g -> gv};
feas[r_] := And[r["pio"] >= 0, r["pig"] >= 0, r["pip"] >= 0, r["po"] > 0, r["pg"] > 0, r["Do"] > 0, r["Dg"] > 0];
bestF[gv_, fn_, cs_: cases] := Function[{l, gm}, Module[{ok = Select[cs, feas[basic[#] /. par[gv] /. {lam -> l, gam -> gm}] &]},
   First@MaximalBy[ok, (fn[basic[#]] /. par[gv] /. {lam -> l, gam -> gm}) &]]];
f3 = bestF[0.3, #["pip"] &]; f6 = bestF[0.6, #["pip"] &]; c6 = bestF[0.6, cs];
Print["Fig1a gam=0.5: ", Table[{l, f3[l, 0.5]}, {l, {0.92, 0.935, 0.95}}], " lam=0.97 gam: ", Table[{gm, f3[0.97, gm]}, {gm, {0.88, 0.9}}]];
Print["Fig1b gam=0.5: ", Table[{l, f6[l, 0.5]}, {l, {0.83, 0.85}}], " lam=0.9: ", Table[{gm, f6[0.9, gm]}, {gm, {0.83, 0.85}}]];
Print["CS1 gam=0.5: ", Table[{l, c6[l, 0.5]}, {l, {0.83, 0.85}}], " lam=0.9: ", Table[{gm, c6[0.9, gm]}, {gm, {0.83, 0.85}}]];
fN = bestF[0.3, -#["be"] &, Take[cases, 4]]; pN = bestF[0.3, #["pip"] &, Take[cases, 4]];
Print["duibi gam=0.5 minbeta: ", Table[{l, fN[l, 0.5]}, {l, {0.87, 0.9}}], " platform: ", Table[{l, pN[l, 0.5]}, {l, {0.87, 0.9, 0.93}}]];
