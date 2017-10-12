with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "xmonadBuildEnv";
  buildInputs = [ stack haskellPackages.cabal2nix zlib x11 alsaLib ];
}
