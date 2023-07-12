{ ... }: let
  primary = "DP-3";
  secondary = "HDMI-A-1";
in {

wayland.windowManager.sway.config = {
  output = {
    ${primary}.pos = "0 0";
    ${secondary}.pos = "1920 338";
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
};

}
