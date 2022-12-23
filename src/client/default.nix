{ config ? {}
, nixpkgs ? import <nixpkgs> config }:

with nixpkgs;

let
  albumTypes = import ./album-types-gen.nix { inherit nixpkgs; };

  nodePackagesRaw = (pkgs.callPackage ./nix/package.nix {});
  nodeDependencies = (nodePackagesRaw // {
    nodeDependencies = nodePackagesRaw.nodeDependencies.override {
      npmFlags = "--ignore-scripts";
    };
  }).nodeDependencies;

  versionsDat = ./nix/versions.dat;
  elmStuffElmReview = ./nix/elm-stuff/generated-code/jfmengels/elm-review;

  mkDerivation =
    { srcs ? ./nix/elm-srcs.nix
    , src
    , name
    , srcdir ? "./src"
    , targets ? []
    }:
    stdenv.mkDerivation rec {
      inherit name src;

      buildInputs = [
        elmPackages.elm
        nodejs-16_x
      ];

      postUnpack = (elmPackages.fetchElmDeps {
        elmVersion = "0.19.1";
        elmPackages = import ./nix/elm-srcs.nix;
        registryDat = ./nix/registry.dat;
      });

      buildPhase = let
        elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
      in ''
        mkdir -p $out/share/doc
        cp -iv ${albumTypes}/Album.elm src;
        ${lib.concatStrings (map (module: ''
          elm make ${elmfile module} --output $out/${module}.js --docs $out/share/doc/${module}.json --optimize
        '') targets)}
      '';

      installPhase = ''
        mv -iv $out/src/Main.js $out/elbum.js;
        cp -iv index.html $out;
        cp -iv .htaccess $out;
      '';

      # running the elm-review check is tricky.  elm-review uses
      # elm-tooling to install elm-json.  then it uses elm-json to
      # inject its dependencies alongside those of the project.
      # finally it runs elm to compile a review-application.  all of
      # this of course expects to download various things from
      # package.elm-lang.org and other places.
      doCheck = true;
      checkPhase = ''
        ln -vs ${nodeDependencies}/lib/node_modules ./node_modules

        # do elm-tooling's work for it by pre-populating an elm-json binary in the location it wants
        # note: we fib about its version
        export NO_ELM_TOOLING_INSTALL=true;
        mkdir -p $ELM_HOME/elm-tooling/elm-json/0.2.10
        ln -s ${elmPackages.elm-json}/bin/elm-json $ELM_HOME/elm-tooling/elm-json/0.2.10/elm-json
        mkdir -p $ELM_HOME/elm-tooling/elm-json/0.2.13
        ln -s ${elmPackages.elm-json}/bin/elm-json $ELM_HOME/elm-tooling/elm-json/0.2.13/elm-json

        # do some of elm-json's work for it by pre-populating its versions.dat file
        cp -R $ELM_HOME/0.19.1 $ELM_HOME/elm-json
        cp -R ${versionsDat} $ELM_HOME/elm-json/versions.dat
        chmod ugo+w $ELM_HOME/elm-json/versions.dat

        # pre-populate some more elm-review -> elm-json caches,
        # including the generated review application.  if any *.elm or
        # elm.json file changes, new versions of this stuff will need
        # to be copied into nix/elm-stuff.
        mkdir -p elm-stuff/generated-code/jfmengels/;
        cp -R ${elmStuffElmReview} elm-stuff/generated-code/jfmengels/elm-review/;
        chmod -R ugo+w elm-stuff/generated-code/;

        echo; echo running elm-review ...
        ./node_modules/.bin/elm-review

        echo; echo running elm-test ...
        ./node_modules/.bin/elm-test --seed 20221126
      '';
    };
in mkDerivation {
  name = "jerith666-elbum-0.1.0";
  srcs = ./elm-srcs.nix;
  src = lib.cleanSourceWith {
    src = ./.;
    filter = path: type:
      (type == "regular" && (
        pkgs.lib.hasSuffix ".elm" path ||
        pkgs.lib.hasSuffix "elm.json" path ||
        pkgs.lib.hasSuffix "index.html" path ||
        pkgs.lib.hasSuffix "package.json" path ||
        pkgs.lib.hasSuffix "package-lock.json" path ||
        pkgs.lib.hasSuffix ".htaccess" path
      )) ||
      (type == "directory" && (
        pkgs.lib.hasSuffix "vendor" path ||
        pkgs.lib.hasSuffix "elm-route-url" path ||
        pkgs.lib.hasSuffix "touch-events" path ||
        pkgs.lib.hasSuffix "src" path ||
        pkgs.lib.hasSuffix "tests" path ||
        pkgs.lib.hasSuffix "review" path ||
        pkgs.lib.hasSuffix "Sandbox" path ||
        pkgs.lib.hasSuffix "Utils" path
      ));
  };
  srcdir = ".";
  targets = [ "src/Main" ];
}
