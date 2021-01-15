Package["LatticeRewriting`"]

PackageImport["GeneralUtilities`"]


PackageExport["GraphRelabel"]

GraphRelabel[graph_Graph, f_] := VertexReplace[graph, Map[# -> f[#]&, VertexList[graph]]];


PackageExport["GraphSum"]

relabelComponent[graph_, {i_}] := GraphRelabel[graph, SubVertex[i]];

GraphSum[graphs__Graph, opts:OptionsPattern[Graph]] :=
  GraphUnion[Sequence @@ MapIndexed[relabelComponent, {graphs}], opts];


PackageExport["SubVertex"]

SubVertex[i_][e_] := SubVertex[e, i];
Format[SubVertex[a_, i_]] := Overscript[a, i];


PackageExport["VertexContractSymbolic"]

VertexContractSymbolic[g_, vertices:{__List}] :=
  VertexReplace[
    VertexContract[g, vertices],
    First[#] -> ContractedVertex[#]& /@ vertices
   ];

VertexContractSymbolic[g_, vertices_List] :=
  VertexContractSymbolic[g, {vertices}];


PackageExport["ContractedVertex"]

Format[ContractedVertex[v_]] := Column[v];


PackageExport["StitchGraphs"]

Format[PathVertex[i_], StandardForm] := OverBar[i];
Format[PathVertex[i_], TraditionalForm] := OverVector[i];

StitchGraphs[graphVerticesPairs__] := Scope[
  graphVerticesPairs = {graphVerticesPairs} /. {g_Graph, gd_Geodesic} :> {g, GeodesicBipath[g, gd]};
  {graphs, bipaths} = Transpose[graphVerticesPairs];
  bipaths = MapIndexed[BipathMap[SubVertex[First[#2]], #]&, bipaths];
  sum = GraphSum[Sequence @@ graphs];
  StitchPaths[sum, bipaths]
]

PackageExport["StitchPaths"]

StitchPaths[graph_Graph, paths_List] := Scope[
  paths = paths /. gd_Geodesic :> GeodesicBipath[graph, gd];
  vertexSets = BipathToCycle /@ paths;
  If[!AllSameBy[vertexSets, Length], Return[$Failed]];
  matchingVertices = Transpose @ vertexSets;
  firstPath = First[paths]; n = BipathLength[firstPath];
  indices = Mod[Range[n]-1, n, -(n-BipathOffset[firstPath])-1];
  contractedGraph = VertexContract[graph, matchingVertices];
  renaming = Flatten @ MapThread[Thread[#1 -> PathVertex[#2]]&, {matchingVertices, indices}];
  VertexReplace[contractedGraph, renaming, $baseLatticeTheme]
]


PackageExport["ToVertexSet"]

ToVertexSet[graph_, spec_] := Switch[spec,
  _Geodesic,  Normal @ GeodesicBipath[graph, spec];
  _Bipath,    Normal @ spec,
  _List,      spec
  _Graph,     VertexList @ spec,
  _,          {spec}
];

vertexMembershipMask[original_, new_] := Map[Boole[MemberQ[new, #]]&, original];

graphSplit[graph_, cut_, orderBy_] := Scope[
  vertexSet = ToVertexSet[graph, cut];
  pieces = ConnectedGraphComponents[VertexDelete[graph, vertexSet]];
  If[orderBy === "Stable",
    vlist = VertexList[graph];
    orderBy = vertexMembershipMask[vlist, VertexList[#]]&];
  SortBy[pieces, orderBy] (* the sort makes it stable *)
];

takeGraphPiece[graph_, pieces_, n_, vertexSet_] := Scope[
  other = Delete[pieces, n];
  droppedVertices = Union @@ Map[VertexList, other];
  droppedEdges = Union @@ Map[EdgeList, other];
  droppedEdges = Union[droppedEdges, UndirectedEdge @@@ Complement[Subsets[vertexSet, {2}], Partition[vertexSet, 2, 1]]];
  graph = VertexDelete[graph, droppedVertices];
  EdgeDelete[graph, Intersection[droppedEdges, EdgeList[graph]]]
];


PackageExport["GraphCut"]

Options[GraphCut] = {
  "IncludeCutPath" -> True,
  "OrderBy" -> "Stable"
};

GraphCut[graph_, cut_, OptionsPattern[]] := Scope[
  vertexSet = ToVertexSet[graph, cut];
  graph = FixVertexCoordinates @ graph;
  pieces = graphSplit[graph, vertexSet, OptionValue["OrderBy"]];
  If[OptionValue["IncludeCutPath"],
    Table[takeGraphPiece[graph, pieces, i, vertexSet], {i, Length[pieces]}],
    pieces
  ]
]


PackageExport["GraphCopyPaste"]

GraphCopyPaste[lattice_, from_, to_] := Scope[
  target = Last @ SortBy[VertexCount] @ GraphCut[lattice, to];
  source = First @ SortBy[VertexCount] @ GraphCut[lattice, from];
  StitchGraphs[{target, to}, {source, from}]
]