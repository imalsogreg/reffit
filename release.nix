{ pkgs ? import ./nix/nixpkgs.nix}:

{
reffit-exe = pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.reffit;
pandoc     = pkgs.pandoc;
}
