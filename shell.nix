{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc902" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
  #ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          async JuicyPixels elm-bridge regex-compat parallel extra safe
          tasty tasty-golden
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
                  haskell-dap
                  ghci-dap
                  haskell-debug-adapter
                  
                  pkgs.vscodium ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
