{ config, lib, pkgs, ... }: {

home.packages = with pkgs; lib.mkIf config.graphical.games [
  prismlauncher
  osu-lazer
  heroic
  renpy
];

}
