{ pkgs, ... }: {

# Overlay packages that use nix to use lix
nixpkgs.overlays = [ (final: _: {
  inherit (final.lixPackageSets.stable)
    nixpkgs-review
    nix-direnv
    nix-eval-jobs
    nix-fast-build
    colmena;
})];

nix.package = pkgs.lixPackageSets.stable.lix;

}
