{ config, lib, ... }: with lib; {

imports = [
  ./sway.nix
  ./kitty.nix
  ./games.nix
];

options.graphical = {
  enable = mkEnableOption (mdDoc "graphical user environment");
  games = mkEnableOption (mdDoc "games :)");
};

config = with config.graphical; {
  wayland.windowManager.sway.enable = enable;
  programs.kitty.enable = enable;
  programs.mako.enable = enable;
  gtk.enable = enable;
  qt.enable = enable;

  assertions = [
    {
      assertion = games -> enable;
      message = "games need a graphical environment to run in";
    }
  ];
};

}
