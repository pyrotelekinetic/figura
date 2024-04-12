{ config, lib, pkgs, ... }: with lib; {

imports = [
  ./sway.nix
  ./games.nix
];

options.graphical = {
  enable = mkEnableOption (mdDoc "graphical user environment");
  games = mkEnableOption (mdDoc "games :)");
};

config = mkMerge [
  ( mkIf config.graphical.enable {
    wayland.windowManager.sway.enable = true;
    services.mako.enable = true;

    # Use librsvg's gdk-pixbuf loader cache file as it enables gdk-pixbuf to load
    # SVG files (important for icons)
    home.sessionVariables.GDK_PIXBUF_MODULE_FILE =
      "$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)";

    # Tell things to use wayland
    home.sessionVariables = {
      MOZ_ENABLE_WAYLAND = 1;
      XDG_CURRENT_DESKTOP = "sway";
      NIXOS_OZONE_WL = 1;
    };

    # Set mouse cursor for gtk and x11
    home.pointerCursor = {
      name = "breeze_cursors";
      package = pkgs.breeze-gtk;
      size = 24;
      gtk.enable = true;
      x11.enable = true;
    };

    qt = {
      platformTheme = "gtk";
      enable = true;
      style = {
        package = pkgs.adwaita-qt;
        name = "adwaita-dark";
      };
    };

    gtk = {
      enable = true;
      gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
    };

    # libadwaita dark theme
    dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";

    home.packages = let
      blex-mono = pkgs.nerdfonts.override { fonts = [ "IBMPlexMono" ]; };
    in with pkgs; [
      # Fonts
      blex-mono
      noto-fonts-emoji

      # Social
      signal-desktop-beta

      # Tools
      firefox-wayland
      gimp
      inkscape
      kicad-small

      # Audio
      pavucontrol
      helvum
      easyeffects

      # Media
      plex-media-player
      jellyfin-media-player
      mpv
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
