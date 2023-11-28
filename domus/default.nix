{ config, lib, inputs, pkgs, ... }: {

users.users.cison.packages = [
  (inputs.wrapper-manager.lib.build {
    inherit pkgs;
    specialArgs = { inherit inputs; };
    modules = [
      ./vim
    ] ++ lib.optionals config.head.graphical [
      ./alacritty
    ];
  })
];

}
