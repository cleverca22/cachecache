with import <nixpkgs> {};

let
  ghc = haskellPackages.ghcWithPackages (ps: with ps; [ aeson-compat servant-server wai warp wreq flow ]);
in stdenv.mkDerivation {
  name = "cachecache";
  buildInputs = [ ghc ];
}
