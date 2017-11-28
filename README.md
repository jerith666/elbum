Elbum is a web photo album generator

basic operation:

run `src/client/album-types-gen.hs`, it produces:
 - `Album.elm`, client-side types and JSON en/decoders corresponding to
   `AlbumTypes.hs`
 - use `src/client/album-types-gen.nix` to do this, it includes a necessary patch to avoid https://github.com/elm-lang/elm-compiler/issues/1591.

build and run `gen-album.hs`
 - give it two arguments:
   - a source directory of pictures, each dir representing an album
     - each album (except the root) must have a `thumbnail` symlink
       - thumbnails may point into subdirs
     - albums can be nested
   - a target directory
 - it populates the target directory with:
   - a directory of resized pictures
   - an `album.json` file describing the structure and contents of the albums

compile `src/client/Main.elm`, it produces:
 - an `index.html` file to serve up an album described by `album.json`

general approach inspired by http://bins.sautret.org/
