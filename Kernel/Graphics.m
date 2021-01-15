Package["LatticeRewriting`"]

PackageImport["GeneralUtilities`"]


PackageExport["EmbeddingGallery"]

$layouts = {"GravityEmbedding", "HighDimensionalEmbedding", "PlanarEmbedding", "SpectralEmbedding", "SpringElectricalEmbedding", "SpringEmbedding", "TutteEmbedding"};
EmbeddingGallery[g_Graph] := Table[Graph[g, GraphLayout -> layout, PlotLabel -> layout], {layout, $layouts}]



PackageExport["ShowLabels"]

ShowLabels[g_Graph] := Graph[g, VertexLabels->Automatic];


PackageExport["$baseLatticeTheme"]

$baseLatticeTheme = Sequence[EdgeStyle -> Gray, VertexStyle -> Directive[EdgeForm[None],Gray], EdgeShapeFunction -> "Line"];


PackageExport["FixVertexCoordinates"]

FixVertexCoordinates[graph_, embedding_:Automatic, initial_:Automatic] :=
  Graph[
    VertexList @ graph, EdgeList @ graph,
    DeleteCases[Options[graph], (VertexCoordinates | GraphLayout) -> _],
    VertexCoordinates -> GraphEmbeddingWithInitialValue[graph, embedding, initial]
  ];


PackageExport["ExtractVertexCoordinates"]

ExtractVertexCoordinates[graph_] := Scope[
  coords = {};
  GraphPlot[graph, VertexShapeFunction -> Function[{xy, v, wh}, AppendTo[coords, v -> xy]]];
  coords
];

PackageExport["GraphEmbeddingWithInitialValue"]

GraphEmbeddingWithInitialValue[graph_, embedding_, initialCoords_] := Scope[
  coords = {};
  GraphPlot[
    Graph[graph, VertexCoordinates -> initialCoords, GraphLayout -> embedding], GraphLayout -> embedding,
    VertexShapeFunction -> Function[{xy, v, wh}, AppendTo[coords, v -> xy]]
  ];
  coords
];


PackageExport["FixConsistentGraphCoordinates"]

FixConsistentGraphCoordinates[graphList_, embedding_:Automatic, n_:5] := Scope[
  initial = Automatic;
  periph = GraphPeriphery[First @ graphList];
  Map[graph |-> (
    newGraph = FixVertexCoordinates[graph, embedding, initial];
    coords = ExtractVertexCoordinates[newGraph];
    initial = Union[RandomSample[Normal @ KeyTake[coords, periph], n], RandomSample[coords, n]];
    newGraph
  ), graphList]
];


PackageExport["LatticeRuleVisualization"]

toEdge[l:{a_, b_}] /; OrderedQ[l] := Style[UndirectedEdge[a, b], getStyle[l]];

toEdge[l:{a_, a_, Repeated[a_]}] /; ListQ[$vertexLabels] := (
    AppendTo[$vertexLabels, a -> Placed[Style[Pane @ Length[l], getStyle[l], Background -> White], Below]];
    Nothing
  );

toEdge[_] := Nothing;

getStyle[e_] := Which[
  !ContainsQ[$rhs, e], RGBColor[0.81, 0, 0],
  !ContainsQ[$lhs, e], RGBColor[0.22, 0.74, 0.2],
  True, LightGray
];

makeCompass[str_, frac_] := Thread[
  StringSplit[str] ->
  Map[AngleVector /* N, -(2 Pi*Range[0, 1 - frac, frac]) + Pi/2]
];

$compass = Join[
  {"Z" -> {0, 0}},
  makeCompass["N NE E SE S SW W NW", 1/8],
  makeCompass["H1 H2 H3 H4 H5 H6", 1/6],
  makeCompass["T1 T2 T3", 1/3]
];

vertexToCoordinate[s_] := Scope[
  If[!SymbolQ[s], Return @ Nothing];
  name = ToUpperCase @ SymbolName[s];
  If[!KeyExistsQ[$compass, name], Return @ Nothing];
  s -> name
];

Options[LatticeRuleVisualization] = {
  VertexLabels -> "Degree",
  VertexCoordinates -> Automatic
};

LatticeRuleVisualization[rule_Rule, OptionsPattern[]] := Scope[
  {lhs, rhs} = List @@ LatticeRuleToHypergraphRule[rule];
  UnpackOptions[$vertexLabels, vertexCoordinates];
  all = Union[lhs, rhs];
  $rhs = rhs; $lhs = lhs;
  vertices = Flatten[{lhs, rhs}];
  If[$vertexLabels === "Degree", $vertexLabels = {}];
  styles = Thread[vertices -> Map[Directive[getStyle[#], EdgeForm[Opacity[0.1]]]&, vertices]];
  edges = Map[toEdge, all];
  If[vertexCoordinates === Automatic, vertexCoordinates = Map[vertexToCoordinate, vertices]];
  Graph[
    edges,
    EdgeStyle -> AbsoluteThickness[3],
    VertexSize -> .2,
    VertexStyle -> styles,
    $baseLatticeTheme,
    VertexLabels -> $vertexLabels,
    VertexCoordinates -> (vertexCoordinates /. $compass),
    ImageSize -> Small
  ]
]


PackageExport["LatticeHistoryGrid"]

Options[LatticeHistoryGrid] = {
  ItemSize -> 80,
  GraphLayout -> Automatic
};

LatticeHistoryGrid[hist_, n_Integer:5, OptionsPattern[]] := Scope[
  UnpackOptions[itemSize, graphLayout];
  If[graphLayout =!= Automatic, hist = GraphPlot[#, GraphLayout -> graphLayout]& /@ hist];
  Multicolumn[Map[Pane[#, itemSize]&, hist], n, Appearance->"Horizontal"]
];
