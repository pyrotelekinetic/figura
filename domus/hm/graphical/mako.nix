{ lib, colors, ... }: {

services.mako.settings = with (lib.mapAttrs (_: x: "#" + x) colors); {
  anchor = "top-right";
  background-color = black;
  border-color = blackBright;
  border-radius = 10;
  border-size = 1;
  default-timeout = 5000;
  font = "IBM Plex Mono";
  ignore-timeout = true;
  layer = "overlay";
  margin = "10";
  markup = true;
  max-visible = 10;
  padding = "5";
  progress-color = "over ${green}";
  text-color = white;
  "mode=dnd" = {
    invisible = true;
  };
};

}
