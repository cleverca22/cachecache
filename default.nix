with import <nixpkgs> {};

let
  ghc = haskellPackages.ghcWithPackages (ps: with ps; [ aeson-compat servant-server wai warp flow bits-bytestring pipes-wai pipes-bytestring pipes-concurrency pipes-async async pipes cryptonite haxl timeit extra http-client-tls lens ]);
in stdenv.mkDerivation {
  name = "cachecache";
  buildInputs = [ ghc ] ++ (with haskellPackages; [ stylish-haskell ghcid hlint ]);
  shellHook = ''
    buildAll() {
      ghc cachecache.hs -o cachecache -Wall -threaded -rtsopts
    }
  '';
  src = lib.cleanSource ./.;
  installPhase = ''
    mkdir -p $out/bin/
    ghc -Wall -threaded ./cachecache.hs -o $out/bin/cachecache
  '';
}
