with import <nixpkgs> {}; {
  hsEnv = stdenv.mkDerivation {
    name = "prog-in-hs";
    buildInputs = [ cabal-install haskellPackages.ghc-mod haskellPackages.stylish-haskell ];
  };
}
