with (builtins.fromJSON (builtins.readFile ./nixpkgs.ghc.json));


let
  masse-prelude-src = pkgs.fetchFromGitHub {
    owner = "MasseR";
    repo = "masse-prelude";
    rev = "52fdf7cd5b70e399d6279083757b1ea0806aa68a";
    sha256 = "0pzp97899r2wxsbz5s8mn6h1hyjbq786aq83i4dcmgqv68g8v0ql";
  };
  overrides = pkgs: with pkgs.haskell.lib; {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc865 = pkgs.haskell.packages.ghc865.override {
          overrides = self: super: with pkgs.haskell.lib; rec {
            # Get rid of this once nix has 0.2.0 or newer
            ekg-influxdb = dontCheck ((super.callHackageDirect {
              pkg = "ekg-influxdb";
              ver = "0.2.0.0";
              sha256 = "1x9ynlgr9lbnbpjkzrfi5zmycbrsckn9zsxf1q199xwkp1k7iif8";
            }) {});
            masse-prelude = super.callPackage masse-prelude-src {};
            merkle = super.callPackage ./merkle {};
            storage-core = super.callPackage ./storage-core {};
            storage-server = super.callPackage ./storage-server {};
            storage-api = super.callPackage ./storage-api {};
            storage-client = super.callPackage ./storage-client {};
            orphans = super.callPackage ./orphans {};
          };
        };
      };
    };
  };
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) { config.packageOverrides = overrides; config.allowUnfree = true; };

in

rec {
  inherit (pkgs.haskell.packages.ghc865) merkle storage-core storage-server storage-api storage-client;
  shell = pkgs.buildEnv {
    name = "shell";
    paths = [];
    buildInputs = with pkgs; [
      entr
      ghcid
      haskell.packages.ghc865.cabal-install
      haskell.packages.ghc865.hasktags
      stylish-haskell
      hlint
      binutils
      (haskell.packages.ghc865.ghcWithHoogle
        (_: builtins.concatMap
          (p: p.buildInputs ++ p.propagatedBuildInputs)
          [ merkle storage-core storage-server storage-client ]))
    ];
  };
}
