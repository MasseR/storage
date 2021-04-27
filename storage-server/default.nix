{ mkDerivation, aeson, base, comonad, ekg-core, generic-lens
, genvalidity, genvalidity-bytestring, genvalidity-hspec
, genvalidity-hspec-aeson, genvalidity-text, hspec
, hspec-golden-aeson, http-client, katip, lens, masse-prelude
, merkle, mtl, pipes, pipes-bytestring, QuickCheck, servant
, servant-client, servant-pipes, servant-server, servant-swagger
, servant-swagger-ui, stdenv, storage-api, storage-client
, storage-core, temporary, transformers, unliftio, wai
, wai-middleware-metrics, warp, yaml
}:
mkDerivation {
  pname = "storage-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base comonad ekg-core generic-lens genvalidity
    genvalidity-text katip lens masse-prelude merkle mtl pipes
    pipes-bytestring QuickCheck servant servant-pipes servant-server
    servant-swagger servant-swagger-ui storage-api storage-core
    transformers unliftio wai wai-middleware-metrics warp yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base generic-lens genvalidity genvalidity-bytestring
    genvalidity-hspec genvalidity-hspec-aeson hspec hspec-golden-aeson
    http-client lens masse-prelude merkle mtl pipes pipes-bytestring
    QuickCheck servant-client servant-server storage-client
    storage-core temporary transformers warp
  ];
  license = stdenv.lib.licenses.mit;
}
