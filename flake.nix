{

description = "Home Manager and NixOS configurations of pyrotelekinetic";

inputs = {
  home-manager = {
    url = "github:nix-community/home-manager/master";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  pinputs.url = "github:pyrotelekinetic/pinputs/main";
  pyroscheme.url = "github:pyrotelekinetic/pyroscheme/main";
  sops-nix = {
    url = "github:Mic92/sops-nix/master";
    inputs = {
      nixpkgs.follows = "nixpkgs";
      nixpkgs-stable.follows = "nixpkgs";
    };
  };
};

outputs = inputs@{
  self,
  home-manager,
  nixos-hardware,
  nixpkgs,
  pinputs,
  pyroscheme,
  sops-nix
}: let
  mkSystem = import ./mkSystem.nix inputs;
in {
  nixosConfigurations = (
    mkSystem {
      host = "sol";
      system = "x86_64-linux";
    }
  ) // (
    mkSystem {
      host = "vega";
      system = "x86_64-linux";
    }
  ) // (
    mkSystem {
      host = "luna";
      system  = "aarch64-linux";
    }
  );

  images.luna = let
    luna-img = self.nixosConfigurations.luna.extendModules {
      modules = [
        "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
        {
          nixpkgs.buildPlatform.system = "x86_64-linux";
          users.users.cison.initialHashedPassword = "";
          documentation.enable = false;
        }
      ];
    };
  in
    luna-img.config.system.build.sdImage;

};

}
