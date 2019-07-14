{ compiler ? "ghc865" }:

(import ./release.nix {inherit compiler;}).doorbell