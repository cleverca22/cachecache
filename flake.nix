{
  description = "a nix binary cache cache";
  inputs = {
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, utils }:
  utils.lib.eachSystem [ "x86_64-linux" ] (system:
  let
    pkgs = import nixpkgs { inherit system; };
  in {
    packages.cachecache = pkgs.callPackage ./. {};
  });
}
