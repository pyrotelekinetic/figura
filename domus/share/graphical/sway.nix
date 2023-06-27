{ config, lib, pkgs, colors, ... }: {

options.sway.barStatus = with lib; mkOption {
  type = with types; either str package;
  default = "date +'%Y-%m-%d %H:%M:%S'";
  description = mdDoc "Command to run for the status section of bar. Update every second.";
};

config = {
  wayland.windowManager.sway = {
    config = {
      defaultWorkspace = "workspace number 1";
      workspaceOutputAssign =
        let
          primary = "DP-3";
          secondary = "HDMI-A-1";
        in [
          {
            output = primary;
            workspace = "1";
          }
          {
            output = primary;
            workspace = "3";
          }
          {
            output = primary;
            workspace = "5";
          }
          {
            output = primary;
            workspace = "7";
          }
          {
            output = primary;
            workspace = "9";
          }
          {
            output = secondary;
            workspace = "2";
          }
          {
            output = secondary;
            workspace = "4";
          }
          {
            output = secondary;
            workspace = "6";
          }
          {
            output = secondary;
            workspace = "8";
          }
          {
            output = secondary;
            workspace = "0";
          }
        ];
      bars = [ {
        command = "swaybar";
        colors = with colors; {
          background = black;
          statusline = white;
          focusedWorkspace = {
            background = green;
            border = black;
            text = white;
          };
          inactiveWorkspace = {
            background = black;
            border = black;
            text = white;
          };
          urgentWorkspace = {
            background = red;
            border = black;
            text = white;
          };
        };
        fonts = {
          names = [ "IBM Plex Mono" ];
          style = "Regular";
          size = 12.0;
        };
        mode = "hide";
        hiddenState = "hide";
        position = "bottom";
        statusCommand = "while ${config.sway.barStatus}; do sleep 1; done;";
      } ];

      colors = with colors; {
        focused = {
          border = green;
          background = green;
          text = green;
          indicator = greenBright;
          childBorder = green;
        };
        focusedInactive = {
          border = magentaBright;
          background = magentaBright;
          text = magentaBright;
          indicator = magenta;
          childBorder = magentaBright;
        };
        unfocused = {
          border = magentaBright;
          background = magentaBright;
          text = magentaBright;
          indicator = magenta;
          childBorder = magentaBright;
        };
      };

      floating = {
        border = 1;
        titlebar = false;
        criteria = [
          { app_id = "pinentry.*"; }
        ];
      };

      focus = {
        followMouse = true;
        mouseWarping = true;
      };

      fonts = {
        names = [ "IBM Plex Mono" ];
        style = "Regular";
        size = 12.0;
      };

      input = {
        "type:keyboard" = {
          xkb_layout = "us";
          xkb_options = "compose:ralt";
        };
        "type:touchpad" = {
          natural_scroll = "enabled";
        };
      };

      keybindings =
        let
          mod = "Mod4";
          left = "h";
          down = "j";
          up = "k";
          right = "l";
        in {
          # Movement
          "${mod}+${left}" = "focus left";
          "${mod}+${down}" = "focus down";
          "${mod}+${up}" = "focus up";
          "${mod}+${right}" = "focus right";
          "${mod}+Shift+${left}" = "move left";
          "${mod}+Shift+${down}" = "move down";
          "${mod}+Shift+${up}" = "move up";
          "${mod}+Shift+${right}" = "move right";
          "${mod}+Space" = "focus mode_toggle";
          "${mod}+Shift+Space" = "floating toggle";
          "${mod}+Shift+minus" = "move scratchpad";
          "${mod}+minus" = "scratchpad show";
          # Workspaces
          "${mod}+1" = "workspace number 1";
          "${mod}+2" = "workspace number 2";
          "${mod}+3" = "workspace number 3";
          "${mod}+4" = "workspace number 4";
          "${mod}+5" = "workspace number 5";
          "${mod}+6" = "workspace number 6";
          "${mod}+7" = "workspace number 7";
          "${mod}+8" = "workspace number 8";
          "${mod}+9" = "workspace number 9";
          "${mod}+0" = "workspace number 0";
          "${mod}+Shift+1" = "move container to workspace number 1";
          "${mod}+Shift+2" = "move container to workspace number 2";
          "${mod}+Shift+3" = "move container to workspace number 3";
          "${mod}+Shift+4" = "move container to workspace number 4";
          "${mod}+Shift+5" = "move container to workspace number 5";
          "${mod}+Shift+6" = "move container to workspace number 6";
          "${mod}+Shift+7" = "move container to workspace number 7";
          "${mod}+Shift+8" = "move container to workspace number 8";
          "${mod}+Shift+9" = "move container to workspace number 9";
          "${mod}+Shift+0" = "move container to workspace number 0";
          # Control
          "${mod}+x" = "kill";
          "${mod}+f" = "fullscreen toggle";
          "${mod}+Semicolon" = "exec bemenu-run -p '>' | xargs swaymsg exec --";
          "${mod}+Shift+c" = "reload";
          "${mod}+Shift+e" = "exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -B 'Yes, exit sway' 'swaymsg exit'";
          # Apps
          "${mod}+Return" = "exec kitty";
          "${mod}+d" = "exec firefox --new-window https://discord.com/app";
          "${mod}+b" = "exec firefox";
          "${mod}+s" = "exec steam";
          "${mod}+u" = "exec cider";
          # Music
          "${mod}+Left" = "exec playerctl previous";
          "${mod}+Right" = "exec playerctl next";
          "${mod}+Delete" = "exec playerctl play-pause";
          "${mod}+Down" = "exec pamixer -d 5";
          "${mod}+Up" = "exec pamixer -i 5";
          # Notifications
          "${mod}+BracketLeft" = "exec makoctl dismiss";
          "${mod}+BracketRight" = "exec makoctl restore";
          # Screenshot
          "${mod}+Print" = "exec grim";
        };
      menu = "bemenu-run -p '>' | xargs swaymsg exec --";
      modifier = "Mod4";
      terminal = "kitty";
      window = {
        border = 1;
        titlebar = false;
      };
      workspaceAutoBackAndForth = false;
    };
  };

  programs.mako = with colors; {
    anchor = "top-right";
    backgroundColor = black;
    borderColor = blackBright;
    borderRadius = 10;
    borderSize = 1;
    defaultTimeout = 5000;
    font = "IBM Plex Mono";
    ignoreTimeout = true;
    layer = "overlay";
    margin = "10";
    markup = true;
    maxVisible = 10;
    padding = "5";
    progressColor = "over ${green}";
    textColor = white;
  };

# Desktop specific packages
  home.packages = with pkgs; lib.mkIf config.graphical.enable [
    wl-clipboard
    xdg-utils
    bemenu
    grim
    slurp
    imv
  ];

  qt.style = {
    package = pkgs.adwaita-qt;
    name = "adwaita-dark";
  };

# Set mouse cursor for gtk and x11
  home.pointerCursor = lib.mkIf config.graphical.enable {
    name = "breeze_cursors";
    package = pkgs.breeze-gtk;
    size = 24;
    gtk.enable = true;
    x11.enable = true;
  };

# Tell things to use wayland
  home.sessionVariables = lib.mkIf config.graphical.enable {
    MOZ_ENABLE_WAYLAND = 1;
    XDG_CURRENT_DESKTOP = "sway";
    NIXOS_OZONE_WL="1";
  };
};

}
