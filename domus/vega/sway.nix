{ ... }: {

wayland.windowManager.sway.config.keybindings = {
  XF86MonBrightnessUp = "exec xbacklight +5";
  XF86MonBrightnessDown = "exec xbacklight -5";
  "Shift+XF86MonBrightnessUp" = "exec xbacklight +20";
  "Shift+XF86MonBrightnessDown" = "exec xbacklight -20";
  "Control+XF86MonBrightnessUp" = "exec xbacklight 50";
  "Control+XF86MonBrightnessDown" = "exec xbacklight 50";
  "Control+Shift+XF86MonBrightnessUp" = "exec xbacklight 100";
  "Control+Shift+XF86MonBrightnessDown" = "exec xbacklight 0";
};

}
