{ inputs, pkgs, ... }: {

users.users.cison.packages = [
  (inputs.wrapper-manager.lib.build {
    inherit pkgs;
    specialArgs = { inherit inputs; };
    modules = [
      ./vim
      ./alacritty
    ];
  })
];

}
