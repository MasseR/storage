{ mkDerivation, base, deepseq, hspec, http-client, lens
, masse-prelude, merkle, pipes, servant, servant-client
, servant-client-core, servant-pipes, stdenv, storage-api
}:
mkDerivation {
  pname = "storage-client";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base deepseq http-client lens masse-prelude merkle pipes servant
    servant-client servant-client-core servant-pipes storage-api
  ];
  testHaskellDepends = [
    base deepseq hspec http-client lens masse-prelude merkle pipes
    servant servant-client servant-client-core servant-pipes
    storage-api
  ];
  license = stdenv.lib.licenses.mit;
}
