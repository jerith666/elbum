#to produce Album.elm, run:
#nix-build -o Album.elm.dir album-types-gen.nix

with import <nixpkgs> {};

{} :

let
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          elm-bridge
        ]);

in
stdenv.mkDerivation {
  name = "Album.elm";
  src = ../shared/AlbumTypes.hs;
  gen = ./album-types-gen.hs;
  fix = ./Album.elm.patch;

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
    patch Album.elm < $fix;
    cp -iv Album.elm $out/;
  '';
}
