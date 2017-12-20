{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs.haskell.lib) enableCabalFlag;
  hans = pkgs.haskellPackages.callPackage ./hans.nix {};
in
  enableCabalFlag (enableCabalFlag hans "raw-ethernet") "examples"
