{
  description = "a nix binary cache cache";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, utils }:
  utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
  let
    pkgs = import nixpkgs { inherit system; };
  in {
    packages.cachecache = pkgs.callPackage ./. {};
  });
}
