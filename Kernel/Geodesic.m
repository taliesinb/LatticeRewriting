Package["LatticeRewriting`"]

PackageImport["GeneralUtilities`"]


PackageExport["Geodesic"]

SetUsage @ "
Geodesic[a$, b$] represents a geodesic through a$ and b$.
"


PackageExport["GeodesicPath"]

GeodesicPath[graph_, edge:(_Geodesic | _Rule | _DirectedEdge), maxVertices_:Infinity] := Scope[
  succ = GraphVertexNeighborAssociation[graph];
  path = {prev, curr} = List @@ edge;
  While[Length[path] <= maxVertices,
    nextList = Discard[succ[curr], EqualTo[prev]];
    If[nextList === {}, Break[]];
    bestLen = Infinity; bestNext = None;
    Do[
      allPaths = FindPath[graph, prev, next, 3, All];
      minPaths = MinimalBy[allPaths, Length];
      If[!AllTrue[minPaths, ContainsQ[curr]], Continue[]];
      len = Length @ First @ minPaths;
      If[len < bestLen && Length[minPaths] === 1,
        bestLen = len; bestNext = next];
    ,
      {next, nextList}
    ];
    If[bestNext === None, Break[]];
    If[MemberQ[path, bestNext], Break[]];
    AppendTo[path, bestNext];
    {prev, curr} = {curr, bestNext}
  ];
  path
];


PackageExport["GeodesicBipath"]

GeodesicBipath[graph_, Geodesic[a_, b_], maxVertices_:Infinity] := Scope[
  forward = GeodesicPath[graph, a -> b, maxVertices];
  backward = GeodesicPath[graph, b -> a, maxVertices];
  BipathFromPair[Drop[backward, 2], forward]
];

GeodesicBipath[graph_, Geodesic[a_, b_, c_], maxVertices_:Infinity] := Scope[
  forward = GeodesicPath[graph, b -> c, maxVertices];
  backward = GeodesicPath[graph, b -> a, maxVertices];
  BipathFromPair[Drop[backward, 1], forward]
]

GeodesicBipath[graph_, Geodesic[vertex_], maxVertices_:Infinity] := Scope[
  results = {};
  visited = <||>;
  Do[
    If[KeyExistsQ[visited, next], Continue[]];
    geodesic = GeodesicBipath[graph, Geodesic[vertex, next], maxVertices];
    AppendTo[results, geodesic];
    AppendTo[visited, Thread[Normal[geodesic] -> True]];
  ,
    {next, VertexNeighbors[graph, vertex]}
  ];
  results
]

