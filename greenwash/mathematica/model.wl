(* ::Package:: *)
(* Equilibrium solver for the greenwashing platform model.
   Timing (matches Appendix A):
     Stage 1: suppliers simultaneously choose wholesale prices (reselling) or retail prices (agency);
              the green supplier chooses greenwashing (and greenness in the extension) at the same time.
     Stage 2: the platform sets retail prices of resold products.
   Basic model:  perceived greenness gam (1+be) g, greenwashing cost th (be g)^2, fixed greenness cost k g^2/2.
   Extension:    g endogenous; absolute exaggeration e = be g, perceived greenness gam (g+e),
                 greenwashing cost th e^2 (= th be^2 g^2), greenness cost k g^2/2.
   Blockchain:   perceived greenness g, no greenwashing, per-unit cost cg borne by the platform. *)

ClearAll[solveCase];
solveCase[info_String, ext_:False] := Module[
  {i, fo, fg, Dg, Do, pio, pig, pip, gw, green, plat, s1o, s1g, sol2, sol1, res, G, vars1},
  {i, fo, fg} = Characters[info];
  G = Which[i == "B", gg, ext, gam (gg + ee), True, gam (1 + be) gg];
  Dg = (1 - rho) a - pg + b po + G;  Do = rho a - po + b pg;
  gw = Which[i == "B", 0, ext, th ee^2, True, th (be gg)^2];
  pio = If[fo == "A", (1 - lam) po Do, wo Do];
  pig = If[fg == "A", (1 - lam) pg Dg, wg Dg] - gw - k gg^2/2;
  pip = If[fo == "A", lam po Do, (po - wo) Do] +
        If[fg == "A", (lam pg - If[i == "B", cg, 0]) Dg, (pg - wg - If[i == "B", cg, 0]) Dg];
  plat = Join[If[fo == "R", {po}, {}], If[fg == "R", {pg}, {}]];
  sol2 = If[plat === {}, {}, First@Solve[D[pip, #] == 0 & /@ plat, plat]];
  s1o = If[fo == "R", wo, po]; s1g = If[fg == "R", wg, pg];
  vars1 = Join[{s1o, s1g}, If[i == "N", If[ext, {ee}, {be}], {}], If[ext, {gg}, {}]];
  sol1 = First@Solve[
     Join[{D[pio /. sol2, s1o] == 0}, (D[pig /. sol2, #] == 0) & /@ Rest[vars1]], vars1];
  res = Join[sol2 /. sol1, sol1];
  <|"wo" -> If[fo == "R", wo /. res, Missing[]], "wg" -> If[fg == "R", wg /. res, Missing[]],
    "po" -> po /. res, "pg" -> pg /. res,
    "be" -> Which[i == "B", 0, ext, (ee/gg) /. res, True, be /. res],
    "g" -> (gg /. res),
    "Do" -> Simplify[Do /. res], "Dg" -> Simplify[Dg /. res],
    "pio" -> Simplify[pio /. res], "pig" -> Simplify[pig /. res], "pip" -> Simplify[pip /. res],
    "hess" -> Simplify[D[pig /. sol2, {Rest[vars1], 2}]] |>
];

cases = {"NRR", "NRA", "NAR", "NAA", "BRR", "BRA", "BAR", "BAA"};
fmt[c_] := StringTake[c, -2];
(* perceived consumer surplus for the linear demand system *)
cs[r_] := (r["Do"]^2 + 2 b r["Do"] r["Dg"] + r["Dg"]^2)/(2 (1 - b^2));

(* Alternative (standard) timing for robustness:
   Stage 1: resold suppliers set wholesale prices;
   Stage 2: the platform sets resale prices and agency suppliers set their retail prices simultaneously.
   The green supplier chooses beta (and g in the extension) together with his own price term. *)
ClearAll[solveCaseStd];
solveCaseStd[info_String, ext_:False] := Module[
  {i, fo, fg, Dg, Do, pio, pig, pip, gw, G, inv, st2vars, st2eqs, sol2, st1vars, st1eqs, sol1, res},
  {i, fo, fg} = Characters[info];
  G = Which[i == "B", gg, ext, gam (gg + ee), True, gam (1 + be) gg];
  Dg = (1 - rho) a - pg + b po + G;  Do = rho a - po + b pg;
  gw = Which[i == "B", 0, ext, th ee^2, True, th (be gg)^2];
  pio = If[fo == "A", (1 - lam) po Do, wo Do];
  pig = If[fg == "A", (1 - lam) pg Dg, wg Dg] - gw - k gg^2/2;
  pip = If[fo == "A", lam po Do, (po - wo) Do] +
        If[fg == "A", (lam pg - If[i == "B", cg, 0]) Dg, (pg - wg - If[i == "B", cg, 0]) Dg];
  inv = Join[If[i == "N", If[ext, {ee}, {be}], {}], If[ext, {gg}, {}]];
  st2vars = {po, pg}; st2eqs = {D[If[fo == "A", pio, pip], po] == 0, D[If[fg == "A", pig, pip], pg] == 0};
  If[fg == "A", st2vars = Join[st2vars, inv]; st2eqs = Join[st2eqs, (D[pig, #] == 0) & /@ inv]];
  sol2 = First@Solve[st2eqs, st2vars];
  st1vars = Join[If[fo == "R", {wo}, {}], If[fg == "R", Join[{wg}, inv], {}]];
  st1eqs = Join[If[fo == "R", {D[pio /. sol2, wo] == 0}, {}], If[fg == "R", (D[pig /. sol2, #] == 0) & /@ Join[{wg}, inv], {}]];
  sol1 = If[st1vars === {}, {}, First@Solve[st1eqs, st1vars]];
  res = Join[sol2 /. sol1, sol1];
  <|"wo" -> If[fo == "R", wo /. res, Missing[]], "wg" -> If[fg == "R", wg /. res, Missing[]],
    "po" -> po /. res, "pg" -> pg /. res,
    "be" -> Which[i == "B", 0, ext, (ee/gg) /. res, True, be /. res], "g" -> (gg /. res),
    "Do" -> Simplify[Do /. res], "Dg" -> Simplify[Dg /. res],
    "pio" -> Simplify[pio /. res], "pig" -> Simplify[pig /. res], "pip" -> Simplify[pip /. res],
    "hess" -> Simplify[If[fg == "A", D[pig, {Join[{pg}, inv], 2}], D[pig /. sol2, {Join[{wg}, inv], 2}]]] |>];

(* realized consumer surplus (utility at true greenness) and total welfare *)
csReal[r_, info_String] := cs[r] - If[StringTake[info, 1] == "N", (gam (1 + r["be"]) gg - gg) (r["Dg"] + b r["Do"])/(1 - b^2), 0];
welfare[r_, info_String] := csReal[r, info] + r["pio"] + r["pig"] + r["pip"];
