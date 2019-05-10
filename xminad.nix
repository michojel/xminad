{ mkDerivation, alsa-mixer, base, containers, dbus, directory
, filepath, HSH, libmpd, MissingH, mtl, process, regex-compat
, regex-posix, shell-escape, stdenv, temporary, text, unix, utf8-string, X11
, xmonad, xmonad-contrib, xmonad-extras
}:
mkDerivation {
  pname = "xminad";
  version = "0.15.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    alsa-mixer base containers dbus directory filepath HSH libmpd
    MissingH mtl process regex-compat regex-posix shell-escape temporary unix
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
