{ config ? {}
, nixpkgs ? import <nixpkgs> config }:

with nixpkgs;

let
  albumTypes = import ./album-types-gen.nix { inherit nixpkgs; };

  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , name
    , srcdir ? "./src"
    , targets ? []
    }:
    let
      sources = import srcs { inherit fetchzip; };
      exactDependencies = builtins.toFile "exact-dependencies.json"
        (builtins.toJSON (lib.mapAttrs (name: value: value.version) sources));
    in stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ elmPackages.elm ];

      preConfigurePhases = ["setupElmStuffPhase"];

      setupElmStuffPhase = ''
        runHook preSetupElmStuffPhase

        rm -rf elm-stuff
        mkdir elm-stuff
        ln -s ${exactDependencies} elm-stuff/exact-dependencies.json

        ${lib.concatStrings (lib.mapAttrsToList (name: src: ''
          mkdir -p elm-stuff/packages/${name}
          ln -s ${src.src} elm-stuff/packages/${name}/${src.version}
        '') sources)}

        runHook postSetupElmStuffPhase
      '';

      buildPhase = let
        elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
      in ''
        mkdir -p $out/share/doc
        cp -iv ${albumTypes}/Album.elm .;
        ${lib.concatStrings (map (module: ''
          elm make --warn ${elmfile module} --output $out/${module}.js --docs $out/share/doc/${module}.json
        '') targets)}
      '';

      installPhase = ''
        mv -iv $out/Main.js $out/elbum.js;
        cp -iv index.html $out;
        cp -iv .htaccess $out;
      '';
    };
in mkDerivation {
  name = "jerith666-elbum-0.1.0";
  srcs = ./elm-srcs.nix;
  src = lib.cleanSourceWith {
    src = ./.;
    filter = path: type:
      type == "regular" && (
        pkgs.lib.hasSuffix ".elm" path ||
        pkgs.lib.hasSuffix "elm-package.json" path ||
        pkgs.lib.hasSuffix "index.html" path ||
        pkgs.lib.hasSuffix ".htaccess" path
      );
  };
  srcdir = ".";
  targets = [ "Main" ];
}

