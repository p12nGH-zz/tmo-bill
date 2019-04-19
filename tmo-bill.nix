{ mkDerivation, base, stdenv
, nixpkgs ? import <nixpkgs> {} }:
let
  report-parser-github = nixpkgs.fetchFromGitHub {
    owner = "p12nGH";
    repo = "report-parser";
    rev = "280a882447b1ceb101e1f226033cae50c733d9cb";
    sha256 = "0g88d3x7198zvi2gl196mik1fr31glawqm0dh4d6fji06r35aina";
  };
  report-parser = nixpkgs.haskellPackages.callPackage "${report-parser-github}/report-parser.nix" {};
in
  mkDerivation {
    pname = "tmo-bill";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends = [ base report-parser ];
    description = "Utility for parsing T-Mobile PDF bills";
    license = stdenv.lib.licenses.bsd3;
  }
