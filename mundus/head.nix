{ config, pkgs, lib, modulesPath, ... }@args: let
  cfg = config.head;
in {

options.head = with lib; {
  headless = mkEnableOption (mdDoc "remote only access");
  graphical = mkEnableOption (mdDoc "graphical user environment");
};

config = lib.mkMerge [
  (
    lib.mkIf cfg.headless (
      (import (modulesPath + "/profiles/headless.nix") args)
    // {
      # screen is nice for leaving a session running while disconnecting ssh
      environment.systemPackages = [ pkgs.screen ];
      # I can just use man from local system
      documentation.man.enable = false;
    })
  )

  (
    lib.mkIf cfg.graphical {
      home-manager.users.cison.graphical.enable = true;

      hardware.opengl = {
        enable = true;
        driSupport = true;
      };

      xdg.portal = {
        enable = true;
        wlr.enable = true;
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      };

      systemd.packages = [ pkgs.mpris-scrobbler ];
      systemd.user.services.mpris-scrobbler.wantedBy = [ "default.target" ];

      programs = {
        sway = {
          enable = true;
          extraPackages = [ ];
          wrapperFeatures.gtk = true;
        };
        dconf.enable = true;
        kdeconnect.enable = true;
        steam = {
          enable = true;
          remotePlay.openFirewall = false;
          dedicatedServer.openFirewall = false;
        };
        gamemode = {
          enable = true;
          enableRenice = true;
          settings = {
            general.inhibit_screensaver = 0;
            custom = {
              start = "${pkgs.libnotify}/bin/notify-send 'GameMode Start'";
              end = "${pkgs.libnotify}/bin/notify-send 'GameMode End'";
            };
          };
        };
      };

      services.pipewire = {
        enable = true;
        alsa = {
          enable = true;
          support32Bit = true;
        };
        pulse.enable = true;
        wireplumber.enable = true;
      };

    }
  )

  {
    assertions = [
      {
        assertion = cfg.graphical -> !cfg.headless;
        message = "system cannot be both headless and graphical";
      }
    ];
  }
];

}
