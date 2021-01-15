Package["LatticeRewriting`"]

PackageImport["GeneralUtilities`"]


PackageExport["Vertex"]

Vertex::usage = "Vertex[...]";

fmtNum[a_] := If[Negative[a], UnderBar[Abs[a]], a];
formatPV[args___] := Row[fmtNum /@ {args}, Style["\[CenterDot]", Gray], BaseStyle -> {FontFamily -> "Avenir"}]

Format[Vertex[args___], StandardForm] := formatPV[args];
Format[Vertex[args___], TraditionalForm] := formatPV[args];


PackageScope["toRange"]

toRange[n_Integer] := Range[-n, n];
toRange[m_Integer ;; n_Integer] := Range[m, n];


PackageExport["ProductVertices"]

ProductVertices[a_, b_] := Vertex @@@ Tuples[{toRange[a], toRange[b]}];


PackageExport["DiscardVertices"]
PackageExport["SelectVertices"]

DiscardVertices[graph_Graph, filter_] := Subgraph[graph, Discard[VertexList[graph], filter]];
SelectVertices[graph_Graph, filter_] := Subgraph[graph, Select[VertexList[graph], filter]];


PackageExport["GraphVertexNeighborAssociation"]

GraphVertexNeighborAssociation[graph_] := Scope[
  vertices = VertexList[graph];
  n = Length[vertices];
  adjacency = Normal @ AdjacencyMatrix[graph];
  succ = <||>;
  Do[succ[vertices[[i]]] = Pick[vertices, adjacency[[i]], 1], {i, n}];
  succ
];


PackageExport["VertexNeighbors"]

VertexNeighbors[graph_, v_] := VertexOutComponent[graph, {v}, {1}];


