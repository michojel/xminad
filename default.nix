{ nixpkgs     ? import <nixpkgs> {}
, lib         ? nixpkgs.lib
, compiler    ? "ghc864"
, makeWrapper ? nixpkgs.makeWrapper
, imagemagick ? nixpkgs.imagemagick
}:

let
  xminad = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./xminad.nix { };
in xminad.overrideDerivation (attrs: rec {
  buildDependencies = [imagemagick];
  runtimeDependencies = [
    nixpkgs.haskellPackages.xmobar
        nixpkgs.pcmanfm-qt
        nixpkgs.terminator
        nixpkgs.tmux
        nixpkgs.vim
        nixpkgs.xorg.xbacklight
        nixpkgs.xdg_utils   # xdg-screensaver
        nixpkgs.xsel
     ];

  buildInputs = attrs.buildInputs ++ [makeWrapper imagemagick];
  configureFlags = attrs.configureFlags ++ ["--datasubdir=" "--flags" "nixos-user-binary"];

  postInstall = ''
    rm $out/bin/checkrc
    # TODO: set the path to xmobar in the configure phase
    wrapProgram $out/bin/xminad --prefix PATH : ${lib.makeBinPath [nixpkgs.haskellPackages.xmobar]};

    datadir="$out/share"
    DESTDIR="$datadir/pixmaps" make -C pic install

    mkdir -p $datadir/applications
    sed "s!{{DATA_DIR}}!$datadir/pixmaps!g" config/xmobar.config \
          > $datadir/xmobarrc
    sed "s!{{BIN_DIR}}!$out/bin!" config/xminad.desktop \
          > $datadir/applications/xminad.desktop
  '';
})

# ex: set et ts=4 sw=4 :
