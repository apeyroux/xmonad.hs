with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "sharpbar";
  ghc = haskell.compiler.ghc802;
  buildInputs = [ stack ];
  isExecutable = true;
  src = ./.;
}
