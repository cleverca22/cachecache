with import <nixpkgs> {};

let
  ghc = haskellPackages.ghcWithPackages (ps: with ps; [ aeson-compat servant-server wai warp wreq flow bits-bytestring ]);
in stdenv.mkDerivation {
  name = "cachecache";
  buildInputs = [ ghc ];
  shellHook = ''
    buildAll() {
      ghc cachecache.hs -o cachecache -Wall
    }
  '';
  src = ./.;
  installPhase = ''
    mkdir -p $out/bin/
    ghc -Wall ./cachecache.hs -o $out/bin/cachecache
  '';
}
