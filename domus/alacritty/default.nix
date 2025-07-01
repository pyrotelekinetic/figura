{ pkgs, lib, inputs, ... }: {

wrappers.alacritty = {
  basePackage = pkgs.alacritty;
  prependFlags = let
    colors = lib.mapAttrs (_: x: "#" + x) inputs.pyroscheme.lib.colors;
    config = pkgs.writeText "alacritty.toml"
      (import ./config.nix colors);
  in [ "--config-file" config ];
};

}
