with import <nixpkgs> { };

haskell.lib.buildStackProject rec {
  name = "xmonad-px-${version}";
  version = "1.0";
  ghc = haskell.compiler.ghc802;
  buildInputs = [
    # haskellPackages.xmobar
    # haskellPackages.xmonad
    # haskellPackages.xmonad-contrib
    # haskellPackages.xmonad-extras
    stack
  ];
  isExecutable = true;
  src = ./.;
}
