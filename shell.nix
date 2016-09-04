with import <nixpkgs> {}; {
  hsEnv = stdenv.mkDerivation {
    name = "prog-in-hs";
    buildInputs = [ haskellPackages.ghc-mod haskellPackages.stylish-haskell ];
  };
}
