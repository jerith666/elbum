elbum is a web photo album generator

basic operation:

run album-types-gen.hs, it produces:
 - Album.elm, client-side types and JSON en/decoders corresponding to
   AlbumTypes.hs
 - use src/client/album-types-gen.nix to do this

run gen-album.hs, giving it:
 - a source directory of pictures, each dir representing an album
   - albums can be nested
 - a target directory

it populates the target directory with:
 - a directory of resized pictures
 - an album.json file describing the structure and contents of the albums

compile Main.elm, it produces:
 - an index.html file to serve up an album described by album.json

general approach inspired by http://bins.sautret.org/
