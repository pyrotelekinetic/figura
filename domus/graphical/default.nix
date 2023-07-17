{ config, lib, pkgs, ... }: with lib; {

imports = [
  ./sway.nix
  ./kitty.nix
  ./games.nix
];

options.graphical = {
  enable = mkEnableOption (mdDoc "graphical user environment");
  games = mkEnableOption (mdDoc "games :)");
};

config = mkMerge [
  ( mkIf config.graphical.enable {
    wayland.windowManager.sway.enable = true;
    programs.kitty.enable = true;
    services.mako.enable = true;
    gtk.enable = true;
    qt.enable = true;

    # Use librsvg's gdk-pixbuf loader cache file as it enables gdk-pixbuf to load
    # SVG files (important for icons)
    home.sessionVariables.GDK_PIXBUF_MODULE_FILE = "$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)";

    home.packages = with pkgs; [
      # Social
      signal-desktop-beta

      # Tools
      firefox-wayland
      gimp
      kicad-small
      pavucontrol
      pulsemixer
      pamixer

      # Media
      plex-media-player
      cider
      playerctl
      mpris-scrobbler
      mpd
      mpdris2
      mpc-cli
      ncmpcpp
    ];
  } )

  {
    assertions = with config.graphical; [
      {
        assertion = games -> enable;
        message = "games need a graphical environment to run in";
      }
    ];
  }
];

}
