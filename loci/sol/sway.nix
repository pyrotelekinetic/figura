{ lib, ... }: let
  primary = "ViewSonic Corporation VA2265 SERIES U99153701230";
  secondary = "Ancor Communications Inc ASUS VS228 H5LMTF142883";
in {

home-manager.users.cison = {
  wayland.windowManager.sway.config = {
    output = {
      ${primary} = {
        position = "0 0";
        subpixel = "rgb";
      };
      ${secondary} = {
        position = "-1920 0";
        subpixel = "rgb";
      };
    };
    workspaceOutputAssign = let
      assign = x: lib.map (n: { output = x; workspace = toString n; });
    in (assign primary [1 3 5 7 9]) ++ (assign secondary [2 4 6 8 0]);
    input."type:tablet_tool".map_to_output = ''"${primary}"'';
    # Stop PS5 controller from being a touchpad
    input."1356:3302:Sony_Interactive_Entertainment_DualSense_Wireless_Controller_Touchpad".events = "disabled"; # USB
    input."1356:3302:DualSense_Wireless_Controller_Touchpad".events = "disabled"; # Bluetooth
  };
  services.mako.output = primary;
};

}
