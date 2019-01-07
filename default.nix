{ nixpkgs     ? import <nixpkgs> {}
, lib         ? nixpkgs.lib
, compiler    ? "ghc844"
, makeWrapper ? nixpkgs.makeWrapper
}:

let
  xminad = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./xminad.nix { };
in xminad.overrideDerivation (attrs: rec {
  runtimeDependencies = xminad.runtimeDependencies ++ [nixpkgs.haskellPackages.xmobar];
  buildInputs = xminad.buildInputs ++ [makeWrapper];
  postInstall = ''
    rm $out/bin/checkrc
    # TODO: set the path to xmobar in the configure phase
    wrapProgram $out/bin/xminad --prefix PATH : ${lib.makeBinPath [nixpkgs.haskellPackages.xmobar]};
    # TODO: install pictures
  '';
})
