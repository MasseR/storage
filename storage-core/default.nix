{ lib, haskellPackages }:

let cleanSourceFilter = name: type: let baseName = baseNameOf (toString name); in ! (
      # Filter out version control software files/directories
      (baseName == ".git" || type == "directory" && (baseName == ".svn" || baseName == "CVS" || baseName == ".hg")) ||
      # Filter out editor backup / swap files.
      lib.hasSuffix "~" baseName ||
      builtins.match "^\\.sw[a-z]$" baseName != null ||
      builtins.match "^\\..*\\.sw[a-z]$" baseName != null ||

      # Filter out generates files.
      lib.hasSuffix ".o" baseName ||
      lib.hasSuffix ".so" baseName ||
      baseName == "tags" ||
      # Filter out nix files
      lib.hasSuffix ".nix" baseName ||
      # Filter out nix-build result symlinks
      (type == "symlink" && lib.hasPrefix "result" baseName) ||
      # Filter out dist
      (lib.hasPrefix "dist" baseName)
  );


in

haskellPackages.callCabal2nix "storage-core" (lib.cleanSourceWith { src = ./.; filter = cleanSourceFilter; }) {}
