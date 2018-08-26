{ compiler ? "ghc843" }:

(import ./release.nix {inherit compiler;}).doorbell