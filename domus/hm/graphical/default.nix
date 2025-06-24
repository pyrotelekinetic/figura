{ config, lib, pkgs, ... }: with lib; {

imports = [
  ./mako.nix
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
      package = pkgs.kdePackages.breeze-gtk;
      size = 24;
      gtk.enable = true;
      x11.enable = true;
    };

    gtk = {
      enable = true;
      gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
    };

    # libadwaita dark theme
    dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";

    home.packages = with pkgs; [
      # Fonts
      nerd-fonts.blex-mono
      noto-fonts-emoji

      # Social
      signal-desktop

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
      plex-desktop
      jellyfin-media-player
      mpv
      cider
      playerctl
      mpris-scrobbler
      mpd
      mpdris2
      mpc-cli
      ncmpcpp

      # Utils
      wl-clipboard
      xdg-utils
      bemenu
      imv
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
