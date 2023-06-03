{

description = "Home Manager configuration of Carter";

inputs = {
  nixpkgs = {
    type = "github";
    owner = "NixOS";
    repo = "nixpkgs";
    ref = "nixos-22.11";
  };
  home-manager = {
    type = "github";
    owner = "nix-community";
    repo = "home-manager";
    ref = "release-22.11";
    inputs.nixpkgs.follows = "nixpkgs";
  };
};

outputs = { nixpkgs, home-manager, ... }: {
  homeConfigurations.cison = home-manager.lib.homeManagerConfiguration {
    pkgs = import nixpkgs {
      config.allowUnfree = true;
      system = "x86_64-linux";
    };
    modules = [
      ./home.nix
      ./sway.nix
      ./vim.nix
      ./games.nix
    ];
  };
};

}
