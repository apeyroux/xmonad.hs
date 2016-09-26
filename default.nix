with import <nixpkgs> {}; {

hsEnv = stdenv.mkDerivation {
  name = "hs";
  buildInputs = [ pkgs.stack haskellPackages.cabal-install ghc
                  xorg.libXrandr xorg.libXext xorg.libXpm xorg.libXft pkgconfig];
  shellHook = ''
      export PS1="λ \w $ "
  '';
  };
}
