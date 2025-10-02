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

      users.users.cison.packages = [
        (pkgs.webcord.overrideAttrs (_: _: {
          patches = [ (pkgs.fetchurl {
            url = "https://github.com/SpacingBat3/WebCord/commit/6b29ae702e7eeab598b3da42a99475cd2094c01b.patch";
            hash = "sha256-+ztTidzgbGX/CPJgf3jDPmS+riIzypXl5yAVALJLKqo=";
          }) ];
        }))
        pkgs.netflix
        pkgs.adwaita-qt6
        pkgs.phinger-cursors
        pkgs.adwaita-icon-theme
      ];
      environment.systemPackages = [ pkgs.xwayland-satellite ];

      qt = {
        enable = true;
        style = "adwaita-dark";
        platformTheme = "gnome";
      };

      xdg.portal = {
        enable = true;
        wlr.enable = lib.mkForce true;
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
        config.common.default = [ "wlr" "gtk" ];
      };

      # gtk polkit auth agent
      security.soteria.enable = true;

      systemd.packages = [ pkgs.mpris-scrobbler ];

      systemd.user.services = {
        mpris-scrobbler.wantedBy = [ "default.target" ];
        waybar.path = let
          toggle-headphones = pkgs.writeShellApplication {
            name = "toggle-headphones";
            runtimeInputs = [ pkgs.ripgrep pkgs.pulseaudio ];
            text = ''
              CURRENT=$(pactl list sinks | rg -o "Active Port: [a-z\-]+")

              if [ "$CURRENT" = "Active Port: analog-output-lineout" ]; then
                pactl set-sink-port alsa_output.pci-0000_0b_00.4.analog-stereo analog-output-headphones
              elif [ "$CURRENT" = "Active Port: analog-output-headphones" ]; then
                pactl set-sink-port alsa_output.pci-0000_0b_00.4.analog-stereo analog-output-lineout
              else echo "wtf"; false
              fi
            '';
          };
        in [ toggle-headphones pkgs.pwvucontrol pkgs.killall ];
      };

      programs = {
        niri.enable = true;
        waybar.enable = true;
        dconf = {
          enable = true;
          profiles.user.databases = [ {
            settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
          } ];
        };
        kdeconnect.enable = true;
        steam = {
          enable = true;
          extraCompatPackages = [
            pkgs.proton-ge-bin
            (pkgs.callPackage ./northstarproton.nix {})
          ];
          package = pkgs.steam.override {
            extraPkgs = pkgs: [ pkgs.libkrb5 pkgs.keyutils ];
          };
        };
        gamescope = {
          enable = true;
          args = [
            "--expose-wayland"
            "--steam"
            "--fullscreen"
            "--rt"
          ];
          capSysNice = false; # causes a permissions error
        };
        gamemode = {
          enable = true;
          enableRenice = true;
          settings = {
            general = {
              renice = 10;
              inhibit_screensaver = 0;
            };
            custom = {
              start = "${pkgs.libnotify}/bin/notify-send 'GameMode Start'";
              end = "${pkgs.libnotify}/bin/notify-send 'GameMode End'";
            };
          };
        };
      };

      services = {
        gnome.gnome-keyring.enable = false; # enabled by niri module but I don't use it
        greetd = {
          enable = true;
          settings = {
            default_session = {
              command = lib.getExe' pkgs.greetd "agreety" + " --cmd 'niri-session'";
            };
          };
        };
        pipewire = {
          enable = true;
          alsa = {
            enable = true;
            support32Bit = true;
          };
          pulse.enable = true;
          jack.enable = true;
          wireplumber.enable = true;
        };
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
