{ config, lib, pkgs, colors, ... }: {

options.sway.barStatus = with lib; mkOption {
  type = with types; either str package;
  default = "date +'%Y-%m-%d %H:%M:%S'";
  description = mdDoc "Command to run for the status section of bar. Update every second.";
};

config = {
  wayland.windowManager.sway = {
    package = null;
    config = {
      defaultWorkspace = "workspace number 1";
      bars = [ {
        command = "swaybar";
        colors = with (lib.mapAttrs (_: x: "#" + x) colors); {
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

      colors = with (lib.mapAttrs (_: x: "#" + x) colors); {
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
        "type:touchpad".natural_scroll = "enabled";
        "type:pointer".accel_profile = "flat";
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
          "${mod}+Return" = "exec alacritty";
          "${mod}+d" = "exec ${lib.getExe pkgs.webcord}";
          "${mod}+b" = "exec firefox";
          "${mod}+s" = "exec steam -console";
          "${mod}+u" = "exec cider";
          # Music
          "${mod}+Left" = "exec playerctl previous";
          "${mod}+Right" = "exec playerctl next";
          "${mod}+Delete" = "exec playerctl play-pause";
          "${mod}+Alt+Up" = "exec playerctl volume 0.05+";
          "${mod}+Alt+Down" = "exec playerctl volume 0.05-";
          "${mod}+Down" = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%- --limit 1";
          "${mod}+Up" = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+ --limit 1";
          "XF86AudioLowerVolume" = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%- --limit 1";
          "XF86AudioRaiseVolume" = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+ --limit 1";
          # Notifications
          "${mod}+BracketLeft" = "exec makoctl dismiss";
          "${mod}+BracketRight" = "exec makoctl restore";
          "${mod}+Shift+BracketLeft+BracketRight" = let
            toggleDND = pkgs.writeShellScript "toggleDND" ''
              if makoctl mode | grep -q dnd; then
                makoctl mode -r dnd
                ${lib.getExe pkgs.libnotify} "DND disabled"
              else
                ${lib.getExe pkgs.libnotify} "DND enabled"
                makoctl mode -a dnd
              fi
            '';
          in "exec ${toggleDND}";
        } // (
        # Screenshot
          let screenshotDir = "$XDG_PICTURES_DIR/screenshots/$(date +%F_%T).png";
        in {
          "${mod}+Print" = "exec grimshot copy output";
          "${mod}+Shift+Print" = "exec grimshot savecopy output ${screenshotDir}";
          "${mod}+p" = "exec grimshot copy anything";
          "${mod}+Shift+p" = "exec grimshot savecopy anything ${screenshotDir}";
        }
      );
      menu = "bemenu-run -p '>' | xargs swaymsg exec --";
      modifier = "Mod4";
      terminal = "alacritty";
      window = {
        border = 1;
        titlebar = false;
      };
      workspaceAutoBackAndForth = false;
    };
  };

  services.mako.settings = with (lib.mapAttrs (_: x: "#" + x) colors); {
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
    extraConfig = ''
      [mode=dnd]
      invisible=true
    '';
  };

  # Desktop specific packages
  home.packages = with pkgs; lib.mkIf config.graphical.enable [
    wl-clipboard
    xdg-utils
    bemenu
    sway-contrib.grimshot
    imv
  ];
};

}
