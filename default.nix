with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "xmonad-px";
  ghc = haskell.compiler.ghc802;
  buildInputs = [ stack ];
  isExecutable = true;
  src = ./.;
}
