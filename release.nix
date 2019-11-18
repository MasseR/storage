with (builtins.fromJSON (builtins.readFile ./nixpkgs.ghc.json));


let
  overrides = pkgs: with pkgs.haskell.lib; {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc865 = pkgs.haskell.packages.ghc865.override {
          overrides = self: super: with pkgs.haskell.lib; rec {
            prelude = super.callPackage ./prelude {};
            merkle = super.callPackage ./merkle {};
            storage-core = super.callPackage ./storage-core {};
            storage-server = super.callPackage ./storage-server {};
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
  inherit (pkgs.haskell.packages.ghc865) prelude merkle storage-core storage-server;
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
          [ prelude merkle storage-core storage-server ]))
    ];
  };
}
