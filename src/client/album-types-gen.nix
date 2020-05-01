#to produce Album.elm, run:
#nix-build -o Album.elm.dir album-types-gen.nix

{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          elm-bridge parallel
        ]);

in
stdenv.mkDerivation {
  name = "Album.elm";
  src = ../generator/AlbumTypes.hs;
  gen = ./album-types-gen.hs;

  buildInputs = [ ghc ];

  unpackPhase = ''
    mkdir src;
    cp -v $src src/AlbumTypes.hs;
    cp -iv $gen src/album-types-gen.hs;
  '';

  installPhase = ''
    cd src;
    mkdir $out;
    ${ghc}/bin/runhaskell album-types-gen.hs > Album.elm;
    cp -iv Album.elm $out/;
  '';
}
