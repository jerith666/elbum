{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
  #ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          friday-juicypixels elm-bridge friday regex-compat parallel
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "elbum-haskell-env-0";
  buildInputs = [ ghc pkgs.elmPackages.elm ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
