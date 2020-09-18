{ pkgs ? import <nixpkgs> {}
, ...
}:

rec {
  inherit pkgs;

  hsPkgs = pkgs.haskellPackages;

  mkSource = import ./.nix/mk-source.nix { inherit (pkgs) lib; };

  pump = hsPkgs.callCabal2nix "pump" ./. {}; #(mkSource ./.) {};
}
