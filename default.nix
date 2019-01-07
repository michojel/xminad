{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:

let
  xminad = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./xminad.nix { };
in xminad.overrideDerivation (attrs: rec {
  runtimeDependencies = [nixpkgs.haskellPackages.xmobar];
})
