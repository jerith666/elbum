{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc902" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
  #ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          async JuicyPixels elm-bridge regex-compat parallel extra safe
          tasty tasty-golden
        ]);

  lamdera = with nixpkgs.pkgs; import ./lamdera.nix {inherit fetchurl stdenv lib ncurses5 gmp5 zlib autoPatchelfHook;};

  olderIdeaPkgs = nixpkgs.pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    # parent of dadc08be jetbrains.idea-{community,ultimate}: 2021.3.2 → 2022.1
    rev = "764c88feaca0b3768176ae907740a53f7ac42af6";
    hash = "sha256-OS4ISDQokcxaSW0ywnq2Q3nZ93UaaT73t2iYunRdBzE=";
  };
  olderIdea = (import olderIdeaPkgs {}).jetbrains.idea-community;

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
                  pkgs.elmPackages.elm-coverage

                  ormolu

                  ghcide
                  haskell-language-server
                  haskell-dap
                  ghci-dap
                  haskell-debug-adapter

                  olderIdea
                  lamdera
                  
                  pkgs.vscodium ];

  shellHook = "echo elm plugin: ${elmPlugin}; eval $(egrep ^export ${ghc}/bin/ghc)";
}
