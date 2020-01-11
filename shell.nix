{ pkgs ? import ./nix/nixpkgs.nix}:

(pkgs.haskellPackages.reffit).env
