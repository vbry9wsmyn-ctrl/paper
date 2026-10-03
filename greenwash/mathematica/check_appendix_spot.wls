Get["model.wl"];
basic = AssociationMap[solveCase[#] /. gg -> g &, cases];
(* spot-check against Appendix A: beta^NAA, p_g^BRA, pi_p^BAA *)
Print[Simplify[basic["NAA"]["be"] - gam g (1-lam)(a (2-(2-b) rho)+2 gam g)/(2 (4-b^2) th - 2 gam^2 g^2 (1-lam))]];
Print[Simplify[basic["BRA"]["pg"] - (4 (a+g)-a (4-3 b) rho-b^2 cg)/(8-b^2 (3 lam+5))]];
Print[Simplify[basic["NAA"]["pig"] - (th (1-lam)(4 th-gam^2 g^2 (1-lam))(a (2-(2-b) rho)+2 gam g)^2/(4 (gam^2 g^2 (1-lam)-(4-b^2) th)^2) - k g^2/2)]];
