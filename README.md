# Elbum is a web photo album generator

Just organize your photos in a directory tree and identify a thumbnail for each directory.

Elbum generates size-optimized images for fast downloads and a dynamic webapp to render the album, laid out optimally for any screen size.

Host the result anywhere that can serve a static website.

Try out the example album at https://jerith666.github.io/elbum-demo/.

# Using Elbum

## Organize Your Photos

Names of directories and images will be used when rendering the album.

Create a symlink named `thumbnail` in each directory, pointing to the thumbnail that should be used when rendering a preview of that directory in its parent.

## Generate The Album

### Build the Code

Clone this repository and run [`nix-build`](https://nixos.org/download.html).

### Run the Generator

Run `./result/elbum /path/to/your/directory/of/images /path/to/generated/album`.

### Serve the Result

When copying `/path/to/generated/album` to your hosting provider, be sure that symlinks are resolved.

# Hacking on Elbum

Elbum's front-end is written in [Elm](https://www.elm-lang.org). The code that generates the album is written in [Haskell](https://www.haskell.org).

Run [`nix-shell`](https://nixos.org/guides/ad-hoc-developer-environments.html) to get all the tools you need to hack on Elbum. In particular, this provides `idea-community` with the Elm plugin for editing the Elm code and `vscodium` for with the Haskell Language Server for editing the Haskell code.

The first thing to look at is [`AlbumTypes.hs`](./src/generator/AlbumTypes.hs), which defines the basic datatypes at the core of Elbum. `elm-bridge` is used to derive Elm types and JSON codecs for these types.

## Generator

The code lives in [`src/generator/`](./src/generator/).

[`gen-album.hs`](./src/generator/gen-album.hs) contains `main`, and relies on [`ShrinkImage.hs`](./src/generator/ShrinkImage.hs) to create smaller versions of the album's images.

## Web App

The code lives in [`src/client/`](./src/client/).

Before hacking on the Elm code, you must run `nix-build -o Album.elm.dir album-types-gen.nix` in the `src/client/` directory. This derives `Album.elm` from [`AlbumTypes.hs`](./src/generator/AlbumTypes.hs).

# Credits

Elbum's general approach is inspired by http://bins.sautret.org/, a now-defunct photo album generator written in Perl.
