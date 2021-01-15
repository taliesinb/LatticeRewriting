Package["LatticeRewriting`"]

PackageImport["GeneralUtilities`"]


PackageExport["HexagonalLatticeWithNegativeDisclination"]

HexagonalLatticeWithNegativeDisclination[n_Integer] :=
  GraphCopyPaste[HexagonalLattice[n], Geodesic[V[1,-1,0],V[0,0,0],V[0,1,-1]], Geodesic[V[1,-1,0],V[0,0,0],V[1,0,-1]]]


PackageExport["SquareLatticeWithNegativeDisclination"]

SquareLatticeWithNegativeDisclination[n_Integer] :=
  GraphCopyPaste[SquareLattice[n], Geodesic[V[0,-1], V[0,0], V[0,1]], Geodesic[V[0,1], V[0,0], V[1,0]]]


PackageExport["HexagonalLatticeWithPositiveDisclination"]

HexagonalLatticeWithPositiveDisclination[n_Integer] := Scope[
  cut = Geodesic[V[1, -1, 0], V[0, 0, 0], V[1, 0, -1]];
  big = Last @ GraphCut[HexagonalLattice[n], cut, "OrderBy" -> VertexCount];
  Graph[StitchPaths[big, {cut, Reverse[cut]}, VertexCombinerFunction -> "First"], VertexCoordinates->None]
]


PackageExport["SquareLatticeWithPositiveDisclination"]

SquareLatticeWithPositiveDisclination[n_Integer] := Scope[
  cut = Geodesic[V[1,0], V[0,0], V[0,1]];
  big = Last @ GraphCut[SquareLattice[n], cut, "OrderBy" -> VertexCount];
  Graph[StitchPaths[big, {cut, Reverse[cut]}, VertexCombinerFunction -> "First"], VertexCoordinates->None]
]