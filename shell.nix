with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "xmonadBuildEnv";
  buildInputs = [
    alsaLib
    haskellPackages.cabal2nix
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    stack
    x11
    xorg.libXrandr
    zlib
  ];
}
