{ sources ? import nix/sources.nix, compiler ? "ghc927" }:

let
  nixpkgs = import sources.nixpkgs {};
in

let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
  #ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          async JuicyPixels elm-bridge regex-compat parallel extra safe
          tasty tasty-golden
        ]);

  lamdera = with nixpkgs.pkgs; import ./nix/lamdera.nix {inherit fetchurl stdenv lib ncurses5 gmp5 zlib autoPatchelfHook;};

  # pinned to recent (but cached) ancestor of
  # dadc08be jetbrains.idea-{community,ultimate}: 2021.3.2 → 2022.1
  olderIdea = (import sources.olderIdeaNixpkgs {}).jetbrains.idea-community;

  elmPlugin = pkgs.fetchurl {
    url = "https://github.com/utiliteez/intellij-elm/releases/download/v5.0.0-beta21/Elm.IntelliJ-5.0.0-beta21.zip";
    hash = "sha256-JkYNZG/H4BMxJDBxlZ14BB7I92qqDo2zcbd90/kILIg=";
  };

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

                  olderIdea
                  lamdera
                  
                  pkgs.vscodium

                  niv ];

  shellHook = "echo elm plugin: ${elmPlugin}; eval $(egrep ^export ${ghc}/bin/ghc)";
}
