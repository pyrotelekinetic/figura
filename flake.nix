{

description = "Home Manager and NixOS configurations of pyrotelekinetic";

inputs = {
  home-manager = {
    url = "github:nix-community/home-manager?ref=master";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  lanzaboote = {
    url = "github:nix-community/lanzaboote?ref=v0.3.0";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  nixos-hardware.url = "github:NixOS/nixos-hardware?ref=master";
  nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
  pinputs = {
    url = "github:pyrotelekinetic/pinputs?ref=main";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  pyroscheme.url = "github:pyrotelekinetic/pyroscheme?ref=main";
  sops-nix = {
    url = "github:Mic92/sops-nix?ref=master";
    inputs = {
      nixpkgs.follows = "nixpkgs";
      nixpkgs-stable.follows = "nixpkgs";
    };
  };
  wrapper-manager = {
    url = "github:viperML/wrapper-manager?ref=master";
    inputs.nixpkgs.follows = "nixpkgs";
  };
};

outputs = inputs: let
  mkSystem = import ./mkSystem.nix inputs;
in {
  nixosConfigurations = (
    mkSystem "sol" //
    mkSystem "vega" //
    mkSystem "luna" //
    mkSystem "altair"
  );

  images.luna = let
    luna-img = inputs.self.nixosConfigurations.luna.extendModules {
      modules = [
        "${inputs.nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
        {
          nixpkgs.buildPlatform.system = "x86_64-linux";
          documentation.enable = false;
        }
      ];
    };
  in
    luna-img.config.system.build.sdImage;

};

}
