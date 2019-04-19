{ nixpkgs ? import <nixpkgs> {} }:
  nixpkgs.haskellPackages.callPackage ./tmo-bill.nix {}
