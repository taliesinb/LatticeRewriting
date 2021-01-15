Package["LatticeRewriting`"]

PackageImport["GeneralUtilities`"]


PackageExport["WithExtraHyperedges"]
PackageExport["HasDegree"]
PackageExport["Cycle"]

WithExtraHyperedges::usage = "WithExtraHyperedges[]";
HasDegree::usage = "HasDegree[vertex, degree]";
Cycle::usage = "Cycle[v1, v2, ...]";

canonEdge[HasDegree[f_, n_Integer]] := ConstantArray[f, n];
canonEdge[UndirectedEdge[a_, b:UndirectedEdge[b1_, _]]] := Splice[{UndirectedEdge[a, b1], canonEdge[b]}];
canonEdge[a_ -> b:Rule[b1_, _]] := Splice[{UndirectedEdge[a, b1], canonEdge[b]}];
canonEdge[Path[p__Integer]] := Splice[UndirectedEdge @@@ Partition[{p}, 2, 1]]
canonEdge[Cycle[f_, r__]] := Splice[UndirectedEdge @@@ Partition[{f, r, f}, 2, 1]];
canonEdge[e_UndirectedEdge] := e;
canonEdge[a_ -> b_] := a \[UndirectedEdge] b;

toEdgeList[e_] := List @@@ (Map[canonEdge, Developer`ToList[e]] /. UndirectedEdge[a_, b_ + c_] :> Splice[{UndirectedEdge[a, b], UndirectedEdge[a, c]}]);
toEdgeList[g_Graph] := List @@@ EdgeList[g];
toEdgeList[WithExtraHyperedges[g_, e_]] := Join[toEdgeList[g], toEdgeList[e]];


PackageExport["LatticeToHypergraph"]
PackageExport["LatticeToIndexHypergraph"]

LatticeToHypergraph[graph_] :=
  JoinMap[If[Length[#] == 2, {#, Reverse @ #}, {#}]&, toEdgeList @ graph];

LatticeToIndexHypergraph[graph_] := ArrayComponents[LatticeToHypergraph[graph], 2];


PackageExport["HypergraphToLattice"]

HypergraphToLattice[hyperedges_] :=
  Graph[Cases[hyperedges, {a_, b_} ? OrderedQ :> UndirectedEdge[a, b]], $baseLatticeTheme];


PackageExport["LatticeRuleToHypergraphRule"]
PackageExport["LatticeRuleToIndexHypergraphRule"]

LatticeRuleToHypergraphRule[lhs_ -> rhs_] := LatticeToHypergraph[lhs] -> LatticeToHypergraph[rhs];
LatticeRuleToIndexHypergraphRule[rule_] := Rule @@ ArrayComponents[List @@ LatticeRuleToHypergraphRule[rule], 3];


PackageExport["LatticeRewritingSystem"]

WM := Block[{$ContextPath = {"System`"}}, Needs["SetReplace`"]; WM = Symbol["SetReplace`WolframModel"]];

LatticeRewritingEvolutionGraph[wmeo_] := GraphRelabel[wmeo["EvolutionGraph"], HypergraphToLattice];
LatticeRewritingStatesList[wmeo_] := Map[HypergraphToLattice, wmeo["StatesList"]];

LatticeRewritingSystem[rule_, init_, steps_, prop_:"EvolutionObject"] := Scope[
  rule = Map[LatticeRuleToHypergraphRule, Developer`ToList[rule]];
  init = LatticeToHypergraph @ init;
  result = WM[rule, init, <|"MaxEvents" -> steps|>, prop, "EventOrderingFunction" -> "Random"]
];

LatticeRewritingSystem[rule_, init_, steps_, "EvolutionGraph"] :=
  LatticeRewritingEvolutionGraph @ LatticeRewritingSystem[rule, init, steps];

LatticeRewritingSystem[rule_, init_, steps_, "StatesList"] :=
  LatticeRewritingStatesList @ LatticeRewritingSystem[rule, init, steps];


