Get["figlib.wl"];
basic = AssociationMap[solveCase[#] /. gg -> g &, cases];
ext = AssociationMap[solveCase[#, True] &, cases];
basePar[gv_, theta_] := {a -> 1, b -> 0.2, rho -> 0.3, th -> theta, cg -> 0.02, k -> 0.4, g -> gv};
extPar[theta_] := {a -> 1, b -> 0.2, rho -> 0.3, th -> theta, cg -> 0.02, k -> 1.2};

transitions[ev_, gm_, lmin_:0.001, lmax_:0.999, step_:0.001] := Module[
 {ls, row, out = {}, lo, hi, left, mid},
 ls = N[Range[lmin, lmax, step]];
 row = bestCase[ev, #, gm] & /@ ls;
 Do[If[row[[i]] =!= row[[i+1]],
   lo = ls[[i]]; hi = ls[[i+1]]; left = row[[i]];
   Do[mid = (lo+hi)/2; If[bestCase[ev,mid,gm] === left, lo=mid,hi=mid], {12}];
   AppendTo[out, {(lo+hi)/2, left, row[[i+1]]}]], {i,Length[ls]-1}];
 out];

adoptAt[ev_, l_] := Module[{isB, lo=0.001,hi=0.999,mid},
 isB[gm_] := StringTake[bestCase[ev,l,gm],1] == "B";
 If[!isB[lo],Return[0.]]; If[isB[hi],Return[1.]];
 Do[mid=(lo+hi)/2;If[isB[mid],lo=mid,hi=mid],{16}];(lo+hi)/2];

eOld3 = mkEval[basic, basePar[0.3, 0.5/0.3^2], #["pip"]&, False];
eNew3 = mkEval[basic, basePar[0.3, 1.4], #["pip"]&, False];
eOld6 = mkEval[basic, basePar[0.6, 0.5/0.6^2], #["pip"]&, False];
eNew6 = mkEval[basic, basePar[0.6, 1.4], #["pip"]&, False];
eOldX = mkEval[ext, extPar[1], #["pip"]&, True];
eNewX = mkEval[ext, extPar[1.4], #["pip"]&, True];

formats = {"RR","RA","AR","AA"};
trustRoot[expr_, guess_] := Quiet[Check[N[gam /. FindRoot[expr==0,{gam,guess}]],Missing["NoRoot"]]];
gw[theta_,gval_,l_,f_] := (gam (1+basic["N"<>f]["be"])-1) /. basePar[gval,theta] /. lam->l;
deltaW[theta_,gval_,l_,f_] := (welfare[basic["B"<>f],"B"<>f]-welfare[basic["N"<>f],"N"<>f]) /. basePar[gval,theta] /. {lam->l, gg->gval};
deltaCSR[theta_,gval_,l_,f_] := (csReal[basic["B"<>f],"B"<>f]-csReal[basic["N"<>f],"N"<>f]) /. basePar[gval,theta] /. {lam->l, gg->gval};

report = <|
 "effectiveCostOldAtG06" -> 0.5,
 "effectiveCostNewAtG06" -> 1.4 0.6^2,
 "effectiveCostNewAtG03" -> 1.4 0.3^2,
 "betaEndogenousOld" -> 1.2/2,
 "betaEndogenousNew" -> 1.2/(2 1.4),
 "gammaHatOld" -> Sqrt[2/(1.2+2.)],
 "gammaHatNew" -> Sqrt[2 1.4/(1.2+2 1.4)],
 "adoptionG03" -> Table[{l, adoptAt[eOld3,l],adoptAt[eNew3,l]}, {l,{0.05,0.2,0.5,0.8,0.95,0.98}}],
 "adoptionG06" -> Table[{l, adoptAt[eOld6,l],adoptAt[eNew6,l]}, {l,{0.05,0.2,0.5,0.8,0.95,0.98}}],
 "adoptionExt" -> Table[{l, adoptAt[eOldX,l],adoptAt[eNewX,l]}, {l,{0.05,0.2,0.5,0.8,0.95,0.98}}],
 "formatTransitionsExt" -> Table[{gm, transitions[eOldX,gm,0.8,0.999,0.0005],transitions[eNewX,gm,0.8,0.999,0.0005]}, {gm,{0.3,0.5,0.7,0.85,0.9,0.95,0.99}}],
 "formatTransitionsG03" -> Table[{gm, transitions[eNew3,gm]}, {gm,{0.3,0.6,0.9}}],
 "formatTransitionsG06" -> Table[{gm, transitions[eNew6,gm]}, {gm,{0.3,0.6,0.9}}],
 "highCommissionOldNewG03" -> Table[{gm, transitions[eOld3,gm,0.8,0.999,0.0005],transitions[eNew3,gm,0.8,0.999,0.0005]}, {gm,{0.3,0.6,0.9}}],
 "highCommissionOldNewG06" -> Table[{gm, transitions[eOld6,gm,0.8,0.999,0.0005],transitions[eNew6,gm,0.8,0.999,0.0005]}, {gm,{0.3,0.6,0.9}}],
 "truthTrustOld" -> Table[{f,trustRoot[gw[0.5/0.6^2,0.6,0.2,f],0.8]}, {f,formats}],
 "truthTrustNew" -> Table[{f,trustRoot[gw[1.4,0.6,0.2,f],0.8]}, {f,formats}],
 "welfareTrustOld" -> Table[{f,trustRoot[deltaW[0.5/0.6^2,0.6,0.2,f],0.88]}, {f,formats}],
 "welfareTrustNew" -> Table[{f,trustRoot[deltaW[1.4,0.6,0.2,f],0.88]}, {f,formats}],
 "csRealTrustNew" -> Table[{f,trustRoot[deltaCSR[1.4,0.6,0.2,f],0.8]}, {f,formats}]
 |>;
Export["out/diagnostics.json", N[report],"JSON"];
