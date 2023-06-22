{ mkDerivation
, lib
, aeson
, async
, base
, bytestring
, elm-bridge
, extra
, filepath
, JuicyPixels
, parallel
, regex-compat
, safe
, tasty
, tasty-golden
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
    extra
    filepath
    JuicyPixels
    parallel
    regex-compat
    safe
    tasty
    tasty-golden
  ];
  homepage = "http://matt.mchenryfamily.org";
  description = "a web photo album generator";
  license = lib.licenses.gpl3;
}
