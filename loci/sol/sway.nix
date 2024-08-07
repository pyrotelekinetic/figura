{ ... }: let
  primary = "DP-3";
  secondary = "HDMI-A-1";
in {

home-manager.users.cison = {
  wayland.windowManager.sway.config = {
    output = {
      ${primary} = {
        position = "0 0";
        subpixel = "rgb";
      };
      ${secondary} = {
        position = "1920 0";
        subpixel = "rgb";
      };
    };
    workspaceOutputAssign = [
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
      input."type:tablet_tool".map_to_output = primary;
      # Stop PS5 controller from being a touchpad
      input."1356:3302:Sony_Interactive_Entertainment_DualSense_Wireless_Controller_Touchpad".events = "disabled"; # USB
      input."1356:3302:DualSense_Wireless_Controller_Touchpad".events = "disabled"; # Bluetooth
  };
  services.mako.output = primary;
};

}
