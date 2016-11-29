with import <nixpkgs> { };
haskell.lib.buildStackProject {
   name = "fp-course-solutions";
   ghc = haskell.packages.ghc801.ghc;
#   buildInputs =
#     [ zlib glib git cabal-install openssh autoreconfHook stack openssl
#       sshpass gmp ];
   LANG = "en_US.UTF-8";
}
