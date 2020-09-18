{ lib }:

with lib;

src:

cleanSourceWith {
  filter = name: type:
    let baseName = baseNameOf (builtins.toString name);
    in cleanSourceFilter name type && ! (
         hasPrefix "cabal.project" baseName
         || hasPrefix "ghcid.txt" baseName
         || hasPrefix "hie.yaml" baseName
         || hasPrefix "Makefile" baseName
         || hasSuffix ".cabal" baseName
         || hasSuffix ".sublime-project" baseName
         || hasSuffix ".md" baseName
         || (type == "directory" && baseName == "scripts")
         || (type == "directory" && baseName == "API-Spec")
         || (type == "directory" && baseName == "docs")
         || (type == "directory" && baseName == "benchmark")
         || (type == "directory" && hasPrefix "dist" baseName)
         || (type == "binary")
       );
  inherit src;
}
