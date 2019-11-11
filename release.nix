with import <nixpkgs> { config.allowBroken = true; };

{
  cachecache = callPackage ./. {};
}
