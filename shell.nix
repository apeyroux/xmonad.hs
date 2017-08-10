{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  resolver = haskellPackages.ghc;
  native_libs = [
      alsaLib
      gmp
      ncurses
      pcre
      pkgconfig
      xorg.libX11
      xorg.libXext
      xorg.libXft
      xorg.libXrandr
      zlib
    ];
in stdenv.mkDerivation {
  name = "hbattery";
  buildInputs = [ resolver ] ++ native_libs;

  STACK_IN_NIX_EXTRA_ARGS = builtins.foldl'
    (acc: lib:
      " --extra-lib-dirs=${lib}/lib --extra-include-dirs=${lib}/include" + acc)
    "" native_libs;

    # Needed if one wants to use ghci, due to https://ghc.haskell.org/trac/ghc/ticket/11042
    LD_LIBRARY_PATH = builtins.concatStringsSep ":" (map (lib: lib.out + "/lib") native_libs);
}
