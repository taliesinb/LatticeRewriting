Package["LatticeRewriting`"]

PackageImport["GeneralUtilities`"]


PackageExport["EmbeddingGallery"]

$layouts = {"GravityEmbedding", "HighDimensionalEmbedding", "PlanarEmbedding", "SpectralEmbedding", "SpringElectricalEmbedding", "SpringEmbedding", "TutteEmbedding"};
EmbeddingGallery[g_Graph] := Table[Graph[g, GraphLayout -> layout, PlotLabel -> layout], {layout, $layouts}]



PackageExport["ShowLabels"]

ShowLabels[g_Graph] := Graph[g, VertexLabels->Automatic];


PackageExport["$baseLatticeTheme"]

$baseLatticeTheme = Sequence[EdgeStyle -> Gray, VertexStyle -> Directive[EdgeForm[None],Gray]];


PackageExport["FixVertexCoordinates"]

FixVertexCoordinates[graph_] := Graph[graph, VertexCoordinates -> GraphEmbedding[graph]];


PackageExport["GraphRuleVisualization"]

Clear[toEdge];
toEdge[l:{a_, b_}] /; OrderedQ[l] := Style[a \[UndirectedEdge] b, getStyle[l]];
toEdge[l:{a_, a_, a_}] := (AppendTo[$labels, a -> Placed[Style[Pane @ 3, getStyle[l], Background -> White], Below]]; Nothing);
toEdge[_] := Nothing;

getStyle[e_] := Which[
  !ContainsQ[$rhs, e], RGBColor[0.81, 0, 0],
  !ContainsQ[$lhs, e], RGBColor[0.22, 0.74, 0.2],
  True, LightGray
];

compiledGraphRuleVisualization[lhs_ -> rhs_] := Scope[
  all = Union[lhs, rhs];
  $rhs = rhs; $lhs = lhs;
  vertices = Flatten[{lhs, rhs}];
  $labels = {};
  Graph[
    Map[toEdge, all],
    EdgeStyle -> AbsoluteThickness[3],
    VertexSize -> .2,
    VertexStyle -> Thread[vertices -> Map[Directive[getStyle[#],EdgeForm[Opacity[0.1]]]&, vertices]],
    $baseLatticeTheme,
    VertexLabels -> $labels,
    ImageSize -> Small
  ]
]

GraphRuleVisualization[rule_Rule] :=
  compiledGraphRuleVisualization @ graphRuleToHypergraphRule[rule]