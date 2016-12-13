{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "hs-prog";
  buildInputs = [ zlib binutils ];
  LANG = "en_US.UTF-8";
}
