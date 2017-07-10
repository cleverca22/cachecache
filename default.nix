with import <nixpkgs> {};

let
  ghc = haskellPackages.ghcWithPackages (ps: with ps; [ aeson-compat servant-server wai warp wreq flow bits-bytestring pipes-wai pipes-bytestring pipes-concurrency pipes-async async pipes cryptonite haxl timeit ]);
in stdenv.mkDerivation {
  name = "cachecache";
  buildInputs = [ ghc ] ++ (with haskellPackages; [ stylish-haskell ghcid ]);
  shellHook = ''
    buildAll() {
      ghc cachecache.hs -o cachecache -Wall
    }
  '';
  src = lib.cleanSource ./.;
  installPhase = ''
    mkdir -p $out/bin/
    ghc -Wall -threaded ./cachecache.hs -o $out/bin/cachecache
  '';
}
