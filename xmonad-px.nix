{ mkDerivation, base, containers, stdenv, taffybar, xmonad
, xmonad-contrib, xmonad-extras
}:
mkDerivation {
  pname = "xmonad";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers taffybar xmonad xmonad-contrib xmonad-extras
  ];
  license = stdenv.lib.licenses.bsd3;
}
