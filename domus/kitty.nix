{ pkgs, ... }: {

programs.kitty = {
  enable = true;
  font = {
    package = pkgs.ibm-plex;
    name = "IBM Plex Mono";
    size = 14;
  };

  settings = {
    enable_audio_bell = false;
    disable_ligatures = "cursor";
    cursor_shape = "block";
    shell_integration = "no-cursor";
    sync_to_monitor = false;
    close_on_child_death = true;
    allow_remote_control = false;

    window_margin_width = 0;
    single_window_margin_width = 0;
    window_padding_width = 0;
    hide_window_decorations = true;
    tab_bar_style = "hidden";
    tab_bar_margin_height = "0.0 0.0";

    shell = ".";
    editor = ".";
  };

  settings.clear_all_shortcuts = true;
  keybindings = {
    "ctrl+shift+c" = "copy_to_clipboard";
    "ctrl+shift+v" = "paste_from_clipboard";
  };

  # Colors
  settings = {
    background = "#1c1c1c";
    foreground = "#afafaf";

    color0 = "#1c1c1c";
    color8 = "#515151";

    color1 = "#af004f";
    color9 = "#b03f72";

    color2 = "#1d5e44";
    color10 = "#527c6b";

    color3 = "#af871c";
    color11 = "#ae9b68";

    color4 = "#1c5f87";
    color12 = "#517c96";

    color5 = "#5f1c5f";
    color13 = "#7c517c";

    color6 = "#307c77";
    color14 = "#628784";

    color7 = "#afafaf";
    color15 = "#dcdcdc";

    selection_foreground = "none";
    selection_background = "none";
  };
};

}
