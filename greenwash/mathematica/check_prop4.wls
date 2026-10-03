Get["figlib.wl"];
ext = AssociationMap[solveCase[#, True] &, {"BRR", "BRA", "BAR", "BAA"}];
gB = (#["g"] &) /@ ext;
d = Factor[Together[gB["BRR"] - gB["BAR"]]]; Print["g^BRR - g^BAR = ", d];
Print["at cg=0 zero lam: ", Solve[Numerator[Factor[d /. cg -> 0]] == 0, lam]];
SeedRandom[5]; bad = {0, 0, 0}; tot = 0;
Do[Module[{p = {a -> 1, b -> RandomReal[{0.05, 0.9}], rho -> RandomReal[{0.05, 0.95}], lam -> RandomReal[{0.01, 0.5}], k -> RandomReal[{0.3, 5}], cg -> RandomReal[{0, 0.3}]}, v, feas},
   feas = AllTrue[Keys[ext], And @@ Join[{ext[#]["g"] > 0, ext[#]["pg"] > 0, ext[#]["po"] > 0, ext[#]["Dg"] > 0, ext[#]["Do"] > 0}, socCond[ext[#]["hess"]]] /. p &];
   If[feas, tot++; v = Map[(# /. p) &, gB];
    If[First@Keys@TakeLargest[v, 1] =!= "BRA", bad[[1]]++];
    If[First@Keys@TakeSmallest[v, 1] === "BAA", bad[[2]]++];
    If[((lam - b^2/(4 - 3 b^2)) /. p) <= 0, If[First@Keys@TakeSmallest[v, 1] =!= "BRR", bad[[3]]++]]]], {4000}];
Print["feasible ", tot, "; RA not highest: ", bad[[1]], "; AA lowest: ", bad[[2]], "; lam<=b^2/(4-3b^2) but RR not lowest: ", bad[[3]]];
