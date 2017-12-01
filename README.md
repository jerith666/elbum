Elbum is a web photo album generator

basic operation:

generate album data from a directory of pictures
 - build and run `src/generator/gen-album.hs`
   - give it two arguments:
     - a source directory of pictures, each dir representing an album
       - each album (except the root) must have a `thumbnail` symlink
         - thumbnails may point into subdirs
       - albums can be nested
     - a target directory
   - it populates the target directory with:
     - a directory of resized pictures
     - an `album.json` file describing the structure and contents of the albums

generate client code to display the album
 - run `src/client/album-types-gen.hs`:
   - it produces `Album.elm`, client-side types and JSON en/decoders corresponding to `AlbumTypes.hs`
   - use `src/client/album-types-gen.nix` to do this (it includes a necessary patch to avoid https://github.com/elm-lang/elm-compiler/issues/1591).
 - compile `src/client/Main.elm`:
   - it produces an `index.html` file to render an album described by `album.json`
   - use `src/client/client.nix` to do this

use the [Nix](https://nixos.org) project's [`nix-shell`](https://nixos.org/nix/manual/#sec-nix-shell) with the provided `shell.nix` to automatically create a development environment with all the necessary tools available.

general approach inspired by http://bins.sautret.org/
