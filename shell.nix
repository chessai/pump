with (import ./default.nix {});

hsPkgs.shellFor {
  packages = _: [
    pump
  ];

  withHoogle = false;

  buildInputs = with pkgs; [
    cabal-install
    cabal2nix
    hsPkgs.bench
    hsPkgs.ghcid
    hsPkgs.json-autotype
    coreutils
  ];
}
