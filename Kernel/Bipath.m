Package["LatticeRewriting`"]

PackageImport["GeneralUtilities`"]


PackageExport["Bipath"]

SetUsage @ "
Bipath[list$, i$] represents a path of vertices where the 'origin' is at position i$ in the list$.
"

Bipath /: Normal[Bipath[a_List, _Integer]] := a;

PathGraph; Unprotect[PathGraph];
Bipath /: PathGraph[bi_Bipath] := Graph[UndirectedEdge @@@ Partition[Normal[bi], 2, 1], $baseLatticeTheme];

underscript[a_, b_] := Underscript[a, Style[b, Gray]]

mapUnderscripted[f_, list_] := MapIndexed[underscript[#1, f[First[#2]]]&, list];
takeUpto[a_List, n_, f_] := If[Length[a] > n, Append["\[Ellipsis]"], Identity] @ mapUnderscripted[f, Take[a, UpTo @ n]];

formatBipath[before_, after_] := AngleBracket @ Apply[VerticalSeparator] @ Join[
  Reverse @ takeUpto[before, 3, Style[-#, Red]&],
  List @ underscript[First[after], Style[0, Black]],
  takeUpto[Rest @ after, 3, Style[#, RGBColor[0, 0.76, 0]]&]]

formatBipath[Bipath[a_List, i_]] := formatBipath[Reverse @ a[[1;; i-1]], a[[i;;]]];

Format[z:Bipath[a_List, i_Integer] /; 1 < i < Length[a], StandardForm] := formatBipath[z];


PackageExport["BipathMap"]

BipathMap[f_, Bipath[list_, i_]] := Bipath[Map[f, list], i];


PackageExport["BipathFromPair"]

BipathFromPair[before_, after_] := Bipath[Join[Reverse @ before, after], Length[before]+1];


PackageExport["BipathToPair"]

BipathToPair[Bipath[a_List, i_]] := {Reverse @ Take[a, i - 1], Drop[a, i-1]};


PackageExport["BipathToCycle"]

BipathToCycle[Bipath[a_List, i_]] := RotateLeft[a, i-1];


PackageExport["BipathOffset"]

BipathOffset[Bipath[_, i_]] := i;
Bipath /: RotateLeft[Bipath[a_List, i_], j_] := Bipath[a, i + j];
Bipath /: RotateRight[Bipath[a_List, i_], j_] := Bipath[a, i - j];
Bipath /: RotateRight[Bipath[a_List, i_]] := Bipath[a, 0]


PackageExport["BipathLength"]

BipathLength[Bipath[e_, _]] := Length[e];