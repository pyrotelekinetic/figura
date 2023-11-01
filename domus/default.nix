{ inputs, pkgs, ... }: {

users.users.cison.packages = [
  (inputs.wrapper-manager.lib.build {
    inherit pkgs;
    modules = [
      ./vim
    ];
  })
];

}
