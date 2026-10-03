(* ::Package:: *)
Get[FileNameJoin[{DirectoryName[$InputFileName], "model.wl"}]];
blue = RGBColor[0.368417, 0.506779, 0.709798]; orange = RGBColor[0.880722, 0.611041, 0.142051];
shade = RGBColor[0.80, 0.84, 0.91];
fnt = {FontFamily -> "Arial", FontSize -> 14, FontColor -> Black};
(* shared frame styling: axis-label and tick font sizes *)
lblSize = 17; tickSize = 13;
order = <|"RR" -> 1, "RA" -> 2, "AA" -> 3, "AR" -> 4|>;
(* negative definiteness of the green supplier's stage-1 Hessian via leading principal minors *)
socCond[h_] := Table[(-1)^n Det[h[[;; n, ;; n]]] > 0, {n, Length[h]}];
feasCond[r_, ext_] := Join[{r["pio"] >= 0, r["pig"] >= 0, r["pip"] >= 0, r["po"] > 0, r["pg"] > 0, r["Do"] > 0, r["Dg"] > 0},
   If[ext, {r["g"] > 0}, {}], socCond[r["hess"]]];
(* compiled evaluator: returns {objective, feasible(1/0)} for each case *)
mkEval[res_, par_, obj_, ext_] := AssociationMap[
   With[{o = obj[res[#]] /. par, f = Boole[And @@ (feasCond[res[#], ext] /. par)]},
     Compile[{{lam, _Real}, {gam, _Real}}, {o, f} // Evaluate]] &, Keys[res]];
bestCase[ev_, l_, gm_, sign_: 1] := Module[{v = KeyValueMap[{#1, #2[l, gm]} &, ev], ok},
   ok = Select[v, #[[2, 2]] == 1 &];
   If[ok === {}, None, First@MaximalBy[ok, sign #[[2, 1]] &][[1]]]];

(* Connect only transitions between feasible optimal formats. *)
boundaryLines[idx_, lmin_: 0.001, n_: 64, m_: 180] := Module[
  {segments = <||>, lastRow = <||>, row,
   ls = Join[Subdivide[lmin, 0.8, m], Rest[Subdivide[0.8, 0.999, m]]],
   gm, key, pair, counts, pt, seg, lo, hi, mid, left},
  Do[
   gm = 0.001 + (i - 1) 0.998/n;
   row = idx[#, gm] & /@ ls;
   counts = <||>;
   Do[If[row[[j]] =!= row[[j + 1]] && row[[j]] > 0 && row[[j + 1]] > 0,
      pair = ToString[Sort[{row[[j]], row[[j + 1]]}]];
      counts[pair] = Lookup[counts, pair, 0] + 1;
      key = pair <> "_" <> ToString[counts[pair]];
      lo = ls[[j]]; hi = ls[[j + 1]]; left = row[[j]];
      Do[mid = (lo + hi)/2; If[idx[mid, gm] === left, lo = mid, hi = mid], {8}];
      pt = {(lo + hi)/2, gm};
      seg = Lookup[segments, key, {}];
      If[Lookup[lastRow, key, -1] == i - 1 && seg =!= {},
       seg[[-1]] = Append[Last[seg], pt],
       seg = Append[seg, {pt}]];
      segments[key] = seg;
      lastRow[key] = i],
    {j, Length[ls] - 1}],
   {i, 1, n + 1}];
  Line /@ Select[Flatten[Values[segments], 1], Length[#] > 1 &]];

regionData[ev_, lmin_: 0.001] := Module[{isB, idx, adoptionLimit, ls},
  isB[l_?NumericQ, gm_?NumericQ] := With[{c = bestCase[ev, l, gm]}, If[c =!= None && StringTake[c, 1] == "B", 1., 0.]];
  idx[l_?NumericQ, gm_?NumericQ] := With[{c = bestCase[ev, l, gm]}, If[c === None, 0, order[fmt[c]]]];
  (* The plotted calibrations have one adoption cutoff in trust at each commission. *)
  adoptionLimit[l_?NumericQ] := Module[{lo = 0.001, hi = 0.999, mid},
    If[isB[l, lo] < 0.5, Return[0.]];
    If[isB[l, hi] > 0.5, Return[1.]];
    Do[mid = (lo + hi)/2; If[isB[l, mid] > 0.5, lo = mid, hi = mid], {10}];
    (lo + hi)/2];
  ls = Join[Subdivide[lmin, 0.8, 60], Rest[Subdivide[0.8, 0.999, 40]]];
  <|"adoption" -> ({#, adoptionLimit[#]} & /@ ls),
    "boundaries" -> (#[[1]] & /@ boundaryLines[idx, lmin])|>];

regionPlot[ev_, labels_, legendPos_, lmin_: 0.001, opts___] := Module[{data = regionData[ev, lmin]},
  Show[
   Graphics[{shade, EdgeForm[None], Polygon[Join[{{lmin, 0.}}, data["adoption"], {{0.999, 0.}}]]}],
   Graphics[{blue, AbsoluteThickness[0.9], Line /@ data["boundaries"]}],
   (* a label may carry an optional third element: a smaller font size for narrow regions *)
   Graphics[{Text[Style[#[[1]], FontFamily -> "Arial", FontColor -> Black, FontSize -> If[Length[#] > 2, #[[3]], 14]], #[[2]]] & /@ labels,
     {RGBColor[0.66, 0.73, 0.85], EdgeForm[RGBColor[0.55, 0.62, 0.76]], Rectangle[legendPos, legendPos + {0.04, 0.04}]},
     Text[Style["Blockchain adoption", Sequence @@ fnt], legendPos + {0.055, 0.02}, {-1, 0}]}],
   Frame -> True, FrameLabel -> {Style["\[Lambda]", Italic, lblSize, Black], Style["\[Gamma]", Italic, lblSize, Black]},
   FrameStyle -> Black, FrameTicksStyle -> Directive[Black, tickSize], PlotRange -> {{lmin - 0.001, 1}, {0, 1}},
   AspectRatio -> 1, ImageSize -> 400, opts]];
