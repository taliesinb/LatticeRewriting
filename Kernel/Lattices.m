Package["LatticeRewriting`"]

PackageImport["GeneralUtilities`"]


PackageExport["HexagonalLattice"]

HexagonalLattice[n_Integer, opts:OptionsPattern[Graph]] :=
  HexagonalLattice[{n, n}, opts]

$downOffsets = {{-1, 1, 0}, {0, 1, -1}, {1, 0, -1}};
HexagonalLattice[{n_, m_}, opts:OptionsPattern[Graph]] := Scope[
  grid = Join @@ Table[{i,j,-i-j}, {i, toRange[n]}, {j, toRange[m]}];
  grid = Select[grid, Apply[Abs[#1 + #2] <= n&]];
  edges = Flatten @ Table[
    destination = vertex + offset;
    If[!MemberQ[grid, destination], Nothing,
      UndirectedEdge[Vertex @@ vertex, Vertex @@ (vertex + offset)]],
    {vertex, grid},
    {offset, $downOffsets}
  ];
  Graph[edges, opts, $baseLatticeTheme]
];


PackageExport["SquareLattice"]

SquareLattice[n_Integer, opts:OptionsPattern[Graph]] := SquareLattice[{n, n}, opts];

gridNeighborsQ[Vertex[x1_, y1_], Vertex[x2_, y2_]] := Abs[x1 - x2] + Abs[y1 - y2] == 1;

SquareLattice[{a_, b_}, opts:OptionsPattern[Graph]] :=
  RelationGraph[gridNeighborsQ, ProductVertices[a, b], opts, $baseLatticeTheme]


PackageExport["$HexOrigin"]

$HexOrigin = V[0,0,0]


PackageExport["$SquareOrigin"]

$SquareOrigin = V[0,0]
