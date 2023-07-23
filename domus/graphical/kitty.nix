{ pkgs, colors, ... }: {

programs.kitty = {
  font = {
    package = pkgs.nerdfonts;
    name = "IBM Plex Mono";
    size = 14;
  };

  settings = with colors; {
    bold_font = "IBM Plex Mono Bold";
    italic_font = "IBM Plex Mono Italic";
    bold_italic_font = "IBM Plex Mono Bold Italic";

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

    clear_all_shortcuts = true;

  # Colors
    background = black;
    foreground = white;

    color0 = black;
    color8 = blackBright;

    color1 = red;
    color9 = redBright;

    color2 = green;
    color10 = greenBright;

    color3 = yellow;
    color11 = yellowBright;

    color4 = blue;
    color12 = blueBright;

    color5 = magenta;
    color13 = magentaBright;

    color6 = cyan;
    color14 = cyanBright;

    color7 = white;
    color15 = whiteBright;

    selection_foreground = "none";
    selection_background = "none";
  };

  keybindings = {
    "ctrl+shift+c" = "copy_to_clipboard";
    "ctrl+shift+v" = "paste_from_clipboard";
  };
};

}
