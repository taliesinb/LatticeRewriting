$dir = FileNameDrop[$InputFileName];

ClearAll["LatticeRewriting`*"]

Block[
  (* Temporarily overrule some of the more exotic features of the macro system.
  TODO: Fix this upstream when GU is open sourced. *)
  {GeneralUtilities`Control`PackagePrivate`$DesugaringRules =
    Dispatch[Normal[GeneralUtilities`Control`PackagePrivate`$DesugaringRules] /. EchoRaw -> Echo]},
  Get[FileNameJoin[{$dir, "Graphs.m"}]];
];


