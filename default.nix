let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./xmonad-px.nix { }
