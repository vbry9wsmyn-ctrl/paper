(* verifies the hand-derived numerators of d beta^NAR/db and d beta^NAR/dlambda used in the proof of Lemma lem:beta *)
Get["model.wl"];
r = solveCase["NAR"] /. gg -> g; s = gam g; x = a (1 - rho) + s; y = a rho;
T = 4 - b^2 (3 lam + 1); L = 8 - b^2 (3 lam + 5);
Nn = T x + 2 b (1 - lam) y; Dn = 4 L th - s^2 T;
Print["beta^NAR form: ", Simplify[r["be"] - s Nn/Dn]];
numB = D[Nn, b] Dn - Nn D[Dn, b];
claimB = 96 b th (1 - lam) x + (2 (1 - lam) Dn + 4 b^2 (1 - lam) (4 th (3 lam + 5) - s^2 (3 lam + 1))) y;
Print["d/db numerator identity: ", Simplify[numB - claimB]];
numL = D[Nn, lam] Dn - Nn D[Dn, lam];
claimL = -48 b^2 (1 - b^2) th x - 8 b (1 - b^2) (8 th - s^2) y;
Print["d/dlam numerator identity: ", Simplify[numL - claimL]];
Print["T - L = ", Simplify[T - L], ";  2T - L = ", Simplify[2 T - L]];
