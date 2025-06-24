{ lib, colors, ... }: {

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

}
