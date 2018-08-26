{ compiler ? "ghc843" }:

let
  release = (import ./release.nix {inherit compiler;});
  pkgs = release.pkgs;
  scripts = [
    (pkgs.writeScriptBin "rebuild-nix" ''
      #!/usr/bin/env bash
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/nix
      ${pkgs.haskellPackages.cabal2nix}/bin/cabal2nix --extra-arguments rtl_433 --extra-arguments mpg123 .. > doorbell.nix
    '')
    (pkgs.writeScriptBin "ghcid-watch" ''
      #!/usr/bin/env bash
      ${pkgs.haskellPackages.ghcid}/bin/ghcid --command 'cabal new-repl all'
    '')
  ];
in pkgs.stdenv.lib.overrideDerivation release.doorbell.env (oldAttrs: rec {
  nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [
    (pkgs.stdenv.mkDerivation {
      name = "scripts";
      phases = "installPhase";
      installPhase = ''
        mkdir -p $out/bin
      '' + (builtins.concatStringsSep "" (builtins.map (script: ''
        for f in $(ls -d ${script}/bin/*); do ln -s $f $out/bin; done
      '') scripts));
    })
    release.cabal
    pkgs.watchexec
    pkgs.haskellPackages.cabal2nix
    pkgs.haskellPackages.ghcid
  ];
})
