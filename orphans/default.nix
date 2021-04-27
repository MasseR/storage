{ mkDerivation, base, binary, cereal, masse-prelude, stdenv }:
mkDerivation {
  pname = "orphans";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base binary cereal masse-prelude ];
  license = stdenv.lib.licenses.mit;
}
