with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "xmonad-px";
  ghc = haskell.compiler.ghc802;
  buildInputs = [
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    stack
  ];
  isExecutable = true;
  src = ./.;
}
