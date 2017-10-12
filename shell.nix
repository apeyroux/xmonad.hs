with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "xmonadBuildEnv";
  buildInputs = [
    alsaLib
    haskellPackages.cabal2nix
    stack
    x11
    xorg.libXrandr
    zlib
  ];
}
