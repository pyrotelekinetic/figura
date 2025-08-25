{ config, lib, inputs, pkgs, ... }: let
  wrappers = inputs.wrapper-manager.lib {
    inherit pkgs;
    specialArgs = {
      inherit inputs;
      nixosConfig = config;
    };
    modules = [
      ./vim
    ] ++ lib.optionals config.head.graphical [
      ./alacritty
      ./foot
    ];
  };
in {

users.users.cison.packages = [
  wrappers.config.build.toplevel
  pkgs.flow-control
];

}
