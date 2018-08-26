{ compiler ? "ghc843" }:

let
  # Disable tests for these packages
  dontCheckPackages = [
  ];

  # Jailbreak these packages
  doJailbreakPackages = [
  ];

  # Disable haddocks for these packages
  dontHaddockPackages = [
  ];

  generatedOverrides = haskellPackagesNew: haskellPackagesOld:
    let
      toPackage = file: _: {
        name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;
        value = haskellPackagesNew.callPackage (./. + "/nix/${file}") { };
      };
    in
      pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

  makeOverrides =
    function: names: haskellPackagesNew: haskellPackagesOld:
      let
        toPackage = name: {
          inherit name;
          value = function haskellPackagesOld.${name};
        };
      in
        builtins.listToAttrs (map toPackage names);

  composeExtensionsList =
    pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

  # More exotic overrides go here
  manualOverrides = haskellPackagesNew: haskellPackagesOld: {
    doorbell = pkgs.haskell.lib.addBuildDepend haskellPackagesOld.doorbell [pkgs.rtl_433 pkgs.ffmpeg-full];
  };

  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = composeExtensionsList [
              generatedOverrides
              (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages  )
              (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
              (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
              manualOverrides
            ];
          };
        };
      };
    };
  };

  bootstrap = import <nixpkgs> { };
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  nixpkgssrc = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };
  pkgs = import nixpkgssrc { inherit config; };

in
  { doorbell = pkgs.haskell.packages.${compiler}.doorbell;
    cabal = pkgs.haskellPackages.cabal-install;
    pkgs = pkgs;
  }
