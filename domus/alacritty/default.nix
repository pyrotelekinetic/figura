{ pkgs, inputs, ... }: {

wrappers.alacritty = {
  basePackage = pkgs.alacritty;
  flags = let
    inherit (inputs.pyroscheme.lib) colors;
    config = pkgs.writeText "alacritty.yml"
      (import ./alacritty.nix colors);
  in [ "--config-file" config ];
};

}
