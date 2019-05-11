{ mkDerivation, alsa-mixer, base, containers, cpphs, dbus, directory, filepath,
  hscolour, libmpd, MissingH, mtl, process, regex-compat , regex-posix, shell-escape,
  stdenv, temporary, text, turtle, unix, utf8-string, X11 , xmonad,
  xmonad-contrib, xmonad-extras
}:
mkDerivation {
  pname = "xminad";
  version = "0.15.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  configureFlags = ["--flags" "nixos-user-binary"];
  libraryHaskellDepends = [
    alsa-mixer base containers cpphs dbus directory filepath hscolour libmpd MissingH
    mtl process regex-compat regex-posix shell-escape temporary turtle unix
    utf8-string xmonad xmonad-contrib xmonad-extras
  ];
  executableHaskellDepends = [
    base containers dbus directory libmpd MissingH regex-compat
    regex-posix text utf8-string X11 xmonad xmonad-contrib
  ];
  doHaddock = false;
  homepage = "https://github.com/michojel/xminad";
  description = "Minar's XMonad window manager";
  license = stdenv.lib.licenses.bsd3;
}
