{ config, lib, pkgs, ... }: lib.mkIf config.graphical.games {

home.packages = with pkgs; [
  prismlauncher
  osu-lazer
  clonehero
  heroic
  renpy
  yuzu-mainline
  dolphin-emu-primehack
];

home.file.northstarproton = let
  northstarproton = pkgs.callPackage ./northstarproton.nix { };
in {
  source = northstarproton;
  target = ".local/share/Steam/compatibilitytools.d/${northstarproton.name}";
};

}
