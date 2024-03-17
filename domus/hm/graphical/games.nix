{ config, lib, pkgs, ... }: lib.mkIf config.graphical.games {

home.packages = with pkgs; [
  prismlauncher
  osu-lazer
  clonehero
  #heroic - depends on EOL electron version
  renpy
  dolphin-emu-primehack
  r2modman
];

home.file.northstarproton = let
  northstarproton = pkgs.callPackage ./northstarproton.nix { };
in {
  source = northstarproton;
  target = ".local/share/Steam/compatibilitytools.d/${northstarproton.name}";
};

}
