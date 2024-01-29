{ pkgs, inputs, ... }: {

wrappers.alacritty = {
  basePackage = pkgs.alacritty;
  flags = let
    inherit (inputs.pyroscheme.lib) colors;
    config = pkgs.writeText "alacritty.toml"
      (import ./config.nix colors);
  in [ "--config-file" config ];
};

}
