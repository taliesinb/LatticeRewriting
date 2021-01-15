Package["LatticeRewriting`"]

PackageImport["GeneralUtilities`"]


HighlightGraph; Unprotect[HighlightGraph];

DownValues[HighlightGraph] = Discard[DownValues[HighlightGraph], ContainsQ[Bipath | Geodesic]];

$highlightNormalizationRules = {
  b_Bipath :> PathGraph[b],
  g_Geodesic :> Map[PathGraph, Developer`ToList @ GeodesicBipath[$graph, g]]
};

HighlightGraph[g_ ? GraphQ, r_ /; ContainsQ[r, _Bipath | _Geodesic], opts:OptionsPattern[]] := Block[
  {$graph = g},
  HighlightGraph[g, r /. $highlightNormalizationRules, opts]
];

DownValues[HighlightGraph] = SortBy[DownValues[HighlightGraph], FreeQ[Bipath]];