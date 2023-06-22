{ mkDerivation
, aeson
, async
, base
, bytestring
, elm-bridge
, filepath
, JuicyPixels
, regex-compat
, parallel
, extra
, safe
, tasty
, tasty-golden
, lib
}:
mkDerivation {
  pname = "elbum";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    async
    base
    bytestring
    elm-bridge
    filepath
    JuicyPixels
    regex-compat
    parallel
    extra
    safe
    tasty
    tasty-golden
  ];
  homepage = "http://matt.mchenryfamily.org";
  description = "a web photo album generator";
  license = lib.licenses.gpl3;
}
