{ config, pkgs, lib, ... }: {

options = {
  northstarProton = lib.mkEnableOption "Northstar Proton" // {
    default = false;
  };
};

config = {
  programs.steam.extraCompatPackages = lib.optional config.northstarProton
    (pkgs.callPackage ./package.nix {});

  assertions = [
    { assertion = config.northstarProton -> config.programs.steam.enable;
      message = "you can't enable Northstar Proton without steam";
    }
  ];
};

}
