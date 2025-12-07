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
    services.mako.enable = true;

    home.packages = with pkgs; [
      # Fonts
      nerd-fonts.blex-mono
      noto-fonts-color-emoji
      # Social
      signal-desktop

      # Tools
      firefox
      gimp
      inkscape
      kicad-small

      # Audio
      pwvucontrol
      helvum
      easyeffects

      # Media
      plex-desktop
      mpv
      cider
      playerctl
      mpris-scrobbler
      mpd
      mpdris2
      mpc
      ncmpcpp

      # Utils
      wl-clipboard
      xdg-utils
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
