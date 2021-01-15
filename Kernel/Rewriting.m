Package["LatticeRewriting`"]

PackageImport["GeneralUtilities`"]



Clear[toEdgeList, canonEdge];
toEdgeList[g_Graph] := List @@@ EdgeList[g];
toEdgeList[WithExtraHyperedges[g_, e_]] := Join[toEdgeList[g], toEdgeList[e]];
canonEdge[a_ \[UndirectedEdge] b_ \[UndirectedEdge] c_] := Splice[{a \[UndirectedEdge] b, b \[UndirectedEdge] c}];
canonEdge[a_ -> b_ -> c_] := Splice[{a \[UndirectedEdge] b, b \[UndirectedEdge] c}];
canonEdge[Path[p__Integer]] := Splice[UndirectedEdge @@@ Partition[{p}, 2, 1]]


PackageExport["WithExtraHyperedges"]
PackageExport["HasDegree"]
PackageExport["Cycle"]

WithExtraHyperedges::usage = "WithExtraHyperedges[]";
HasDegree::usage = "HasDegree[vertex, degree]";
Cycle::usage = "Cycle[v1, v2, ...]";

canonEdge[HasDegree[f_, n_Integer]] := ConstantArray[f, n];
canonEdge[Cycle[f_, r__]] := Splice[UndirectedEdge @@@ Partition[{f, r, f}, 2, 1]];
canonEdge[e_UndirectedEdge] := e;
canonEdge[a_ -> b_] := a \[UndirectedEdge] b;
toEdgeList[e_] := List @@@ (Map[canonEdge, Developer`ToList[e]] /. (a_ \[UndirectedEdge] (b_ + c_)) :> Splice[{a \[UndirectedEdge] b, a \[UndirectedEdge] c}]);
graphToHypergraph[graph_] := JoinMap[If[Length[#] == 2, {#, Reverse @ #}, {#}]&, toEdgeList @ graph];
hypergraphToGraph[hyperedges_] := Graph[Cases[hyperedges, {a_, b_} ? OrderedQ :> a \[UndirectedEdge] b]];


PackageScope["graphRuleToHypergraphRule"]

graphRuleToHypergraphRule[lhs_ -> rhs_] := graphToHypergraph[lhs] -> graphToHypergraph[rhs];


PackageExport["GraphRewritingSystem"]

GraphRewritingEvolutionGraph[wmeo_] := GraphRelabel[wmeo["EvolutionGraph"], hypergraphToGraph];
GraphRewritingStatesList[wmeo_] := Map[hypergraphToGraph, wmeo["StatesList"]];

GraphRewritingSystem[rule_, init_, steps_, prop_:"EvolutionObject"] := Scope[
  rule = Map[graphRuleToHypergraphRule, Developer`ToList[rule]];
  result = WolframModel[rule, graphToHypergraph @ init, <|"MaxEvents" -> steps, "MaxVertices" -> 100|>, prop, "EventOrderingFunction" -> "Random"]
];

GraphRewritingSystem[rule_, init_, steps_, "EvolutionGraph"] :=
  GraphRewritingEvolutionGraph @ GraphRewritingSystem[rule, init, steps];

GraphRewritingSystem[rule_, init_, steps_, "StatesList"] :=
  GraphRewritingStatesList @ GraphRewritingSystem[rule, init, steps];


