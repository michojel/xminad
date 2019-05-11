{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, alsa-mixer, base, containers, dbus, directory
      , filepath, libmpd, MissingH, mtl, process, regex-compat
      , regex-posix, stack, stdenv, temporary, text, turtle, unix, utf8-string, X11
      , xmonad, xmonad-contrib, xmonad-extras
      }:
      mkDerivation {
        pname = "xminad";
        version = "0.15.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          alsa-mixer base containers dbus directory filepath libmpd MissingH
          mtl process regex-compat regex-posix stack temporary turtle unix utf8-string
          xmonad xmonad-contrib xmonad-extras
        ];
        executableHaskellDepends = [
          base containers dbus directory libmpd MissingH regex-compat
          regex-posix text utf8-string X11 xmonad xmonad-contrib
        ];
        doHaddock = false;
        homepage = "https://github.com/michojel/xminad";
        description = "Minar's XMonad window manager";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
