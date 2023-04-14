{ pkgs }:

let
  myHsPkgs = pkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      prometheus = pkgs.haskell.lib.doJailbreak hsuper.prometheus;
    };
  };
  ghc = myHsPkgs.ghcWithPackages (ps: with ps; [
    #aeson-compat
    #bits-bytestring
    #cryptonite
    #flow
    #haxl
    #pipes-async
    #pipes-wai
    #wai
    #warp
    async
    extra
    http-client-tls
    lens
    pipes
    pipes-bytestring
    pipes-concurrency
    prometheus
    servant-server
    timeit
  ]);
in pkgs.stdenv.mkDerivation {
  name = "cachecache";
  buildInputs = [ ghc ] ++ (with pkgs.haskellPackages; [ stylish-haskell ghcid hlint ]);
  shellHook = ''
    buildAll() {
      ghc cachecache.hs -o cachecache -Wall -threaded -rtsopts
    }
  '';
  src = pkgs.lib.cleanSource ./.;
  installPhase = ''
    mkdir -p $out/bin/
    ghc -Wall -threaded ./cachecache.hs -o $out/bin/cachecache
  '';
}
