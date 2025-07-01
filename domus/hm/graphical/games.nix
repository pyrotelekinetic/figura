{ config, lib, pkgs, ... }: lib.mkIf config.graphical.games {

home.packages = with pkgs; [
  prismlauncher
  osu-lazer
  clonehero
  #heroic - depends on EOL electron version
  dolphin-emu-primehack
  r2modman
  ryubing
];

}
