{ lib, ... }: let
  primary = "ViewSonic Corporation VA2265 SERIES U99153701230";
  secondary = "Ancor Communications Inc ASUS VS228 H5LMTF142883";
  crt = "HJW MACROSILICON 0x0002E9BD";
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
      ${crt} = {
        modeline = "63.02  640 680 752 864  480 481 484 521  -hsync +vsync";
        position = "0 2000";
        subpixel = "none";
      };
    };
    keybindings = let
      mod = "Mod4";
    in {
      "${mod}+Grave" = "workspace crt";
      "${mod}+Shift+Grave" = "move container to workspace crt";
    };
    workspaceOutputAssign = let
      assign = x: lib.map (n: { output = x; workspace = toString n; });
    in (assign primary [1 3 5 7 9])
    ++ (assign secondary [2 4 6 8 0])
    ++ (assign crt ["crt"]);
    input."type:tablet_tool".map_to_output = ''"${primary}"'';
  };
  services.mako.output = primary;
};

}
