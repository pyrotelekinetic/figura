{ config, pkgs, lib, ... }: let
  cfg = config.mouse;
in {

options.mouse = let
  inherit (lib) mkOption;
  inherit (lib.types) attrsOf submodule str int;
in {
  edpi = mkOption {
    type = int;
  };
  mice = mkOption {
    type = attrsOf (submodule {
      options = {
        udevID = mkOption { type = str; };
        swayID = mkOption { type = str; };
        dpi = mkOption { type = int; };
        refresh = mkOption { type = int; };
      };
    });
    default = { };
  };
};

config = let
in lib.mkIf config.head.graphical {
  services.udev.packages = let
    makeHwdb = _: { udevID, dpi, refresh, ... }: ''
      ${udevID}
        MOUSE_DPI=${toString dpi}@${toString refresh}
    '';
    hwdb = lib.strings.concatStringsSep "\n"
      (lib.mapAttrsToList makeHwdb cfg.mice);
  in [ (
    pkgs.writeTextDir "/etc/udev/hwdb.d/99-mice.hwdb" hwdb
  ) ];

  home-manager.users.cison.wayland.windowManager.sway.config = let
    getScale = dpi: let
      edpi' = cfg.edpi + 0.0;
      cap = 1000;
      dpi' = if (dpi > cap)
        then cap
        else dpi;
    in (edpi' / dpi') - 1;
    makeAccel = _: { swayID, dpi, ... }: {
      name = swayID;
      value = { pointer_accel = toString (getScale dpi); };
    };
  in {
    input = lib.attrsets.mapAttrs' makeAccel cfg.mice;
  };
};

}
