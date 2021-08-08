{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8106" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
  #ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          async friday-juicypixels elm-bridge friday regex-compat parallel
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "elbum-haskell-env-0";
  buildInputs = with pkgs.haskellPackages;
                [ ghc
                  pkgs.elmPackages.elm

                  ormolu

                  ghcide
                  haskell-language-server
                  
                  pkgs.vscodium ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
