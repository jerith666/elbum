#to produce index.html, run:
#nix-build -o index.html.dir client.nix

with import <nixpkgs> {};

{} :

stdenv.mkDerivation {
  name = "index.html";
  src = lib.cleanSource ./.;
  albumTypes = import ./album-types-gen.nix {};

  buildInputs = [ pkgs.elmPackages.elm ];

  installPhase = ''
    ls -l;
    rm Album.elm;
    cp -iv $albumTypes/Album.elm .;
    ${elmPackages.elm}/bin/elm-make Main.elm;

    mkdir $out;
    cp -iv index.html $out/;
  '';
}
