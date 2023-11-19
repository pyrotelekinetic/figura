{ pkgs, config, lib, inputs, ... }: let
  configHM = config.home-manager.users.cison;
in {

imports = [
  ./head.nix
  ./pyrosite.nix
  ./hosts.nix
  ./remoteBuild.nix
];

home-manager.users.cison.graphical.enable = config.head.graphical;

sops = {
  defaultSopsFile = ./secrets.yaml;
  gnupg ={
    home = configHM.programs.gpg.homedir;
    sshKeyPaths = [];
  };
  secrets.testSecret = {};
};

hardware.opengl = {
  enable = config.head.graphical;
  driSupport = true;
};

nix = {
  package = pkgs.nixVersions.nix_2_17;
  settings = {
    experimental-features = [ "nix-command" "flakes" ];
    trusted-users = [ "root" "cison" ];
    auto-optimise-store = true;
    flake-registry = "";
  };
  gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
  registry.figura = {
    from = {
      type = "indirect";
      id = "figura";
    };
    to = {
      type = "github";
      owner = "pyrotelekinetic";
      repo = "figura";
      ref = "main";
    };
    exact = false;
  };
};

documentation.man.generateCaches = true;

networking.firewall.enable = true;

time.timeZone = "America/Phoenix";

# Internationalisation properties.
i18n = {
  defaultLocale = "en_US.UTF-8";
  extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };
};

users.users.cison = {
  isNormalUser = true;
  description = "Carter";
  extraGroups = [
    "networkmanager"
    "wheel"
    "disk"
    "keys"
  ];
  openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC4X2NdKZeEjmuezkLudU8HGnPnxCMC7g7LkWJdr+Rn9K/VuJOHSHXWeWXtxiAQiPjRiE71FflXLmTTUuPZFM6GCKSxd3uZ/Sx6Cw4qQdWoIfuNdJR+Gl89uEIpO2URBHmpxHRRB58qTLyBz86giU2jIq18jCvu9OdubvqBUavHmUrz63g13TNRAwbOBOqt8Mx3RWfvw0+KXOD8BykWLSKsOfOaqErAcAzEuhGHZ1boR+HcwgHis9Q7/YWSfojne3wqftSNHlSjYKDziI9cC3CPSsjidMZkUuhL6vP7gzPln9H0sktyVmrQXxlzMD4zc9IQLiODemWjOPVuYn1alOsFvfBIiQIB3maU5PV33uwba1i1rYgX8c+nSscokdD+F4S4ILlI7vFSYZVLgun/DPtshNiewT7y0uJgNmiJUzU75EO0uKpfnw8Sr/MoisiIglLDzUYjTOv7sHI0YXGRYrwpRkokuxTDZVoHWodiZp7Jej8VpEMzu5zu/AG5c00OWCTSD33bgpCahhSqy/vPBuqqmkBwML7hN/mIw+U/EUuT5uE392ktfw2uJuLQWwVPAkytaNCWgsmKzED0fhjoJVY1T9nbzP50iAJf+uvnFvJ+jJD+nx97ZghsmwivfRbFLKC2iH4eBZjjcWhDZ8aSqyFnInuaiY4YIkLomRdJbnhaOQ== (none)"
  ];
  initialHashedPassword = "";
};

xdg.portal = {
  enable = config.head.graphical;
  wlr.enable = config.head.graphical;
  extraPortals = with pkgs; [
    xdg-desktop-portal-gtk
  ];
};

security = {
  doas = {
    enable = true;
    extraRules = [ {
      runAs = "root";
      groups = [ "wheel" ];
      noPass = false;
      keepEnv = true;
      # Add all variables that should have been read from PAM_env
      # See https://github.com/NixOS/nixpkgs/issues/158988
      setEnv = with lib; let
        suffixedVariables =
          flip mapAttrs config.environment.profileRelativeSessionVariables (envVar: suffixes:
            flip concatMap config.environment.profiles (profile:
              map (suffix: profile + suffix ) suffixes
            )
          );
        suffixedVariables' = (zipAttrsWith (const concatLists) [
          # Make sure security wrappers are prioritized without polluting
          # shell environments with an extra entry.
          { PATH = [ config.security.wrapperDir ]; }

          (mapAttrs (const toList) config.environment.sessionVariables)
          suffixedVariables
        ]);
        fixEnvVars = replaceStrings
          [ "$HOME" "$USER" "\${XDG_STATE_HOME}" ]
          [ "/root" "root" "$XDG_STATE_HOME" ];
        doasVariable = k: v: k + "=" + concatStringsSep ":" (map fixEnvVars (toList v));
      in mapAttrsToList doasVariable suffixedVariables';
    } ];
  };
  sudo.enable = false;
  polkit.enable = true;
  rtkit.enable = true;
};

# Sorry Richard
nixpkgs.config.allowUnfree = true;

programs = {
  sway = {
    enable = config.head.graphical;
    extraPackages = [ ];
    wrapperFeatures.gtk = true;
  };
  dconf.enable = config.head.graphical;
  kdeconnect.enable = config.head.graphical;
  steam = {
    enable = config.head.graphical;
    remotePlay.openFirewall = false;
    dedicatedServer.openFirewall = false;
  };
  gamemode = {
    enable = config.head.graphical;
    enableRenice = true;
    settings = {
      general.inhibit_screensaver = 0;
      custom = {
        start = "${pkgs.libnotify}/bin/notify-send 'GameMode Start'";
        end = "${pkgs.libnotify}/bin/notify-send 'GameMode End'";
      };
    };
  };
};

environment.systemPackages = with pkgs; [
  coreutils-full
  git
  vim
  sops
  doas-sudo-shim

  nix-tree

  smartmontools
  usbutils
  pciutils
  lshw
  lm_sensors
  fwts

  ntfs3g

  zip unzip p7zip xz gzip bzip2
];

fonts.packages = [ pkgs.unifont ];

console = {
  packages = [ pkgs.terminus_font ];
  font = "Lat2-Terminus16";
  colors = let
    colors' = lib.mapAttrs (lib.const (lib.removePrefix "#")) inputs.pyroscheme.lib.colors;
  in with colors'; [
    black
    red
    green
    yellow
    blue
    magenta
    cyan
    white
    blackBright
    redBright
    greenBright
    yellowBright
    blueBright
    magentaBright
    cyanBright
    whiteBright
  ];
};

services = {
  smartd = {
    enable = true;
    notifications.x11.enable = config.head.graphical;
  };

  pipewire = {
    enable = config.head.graphical;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse.enable = true;
    wireplumber.enable = true;
  };

  openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
    openFirewall = true;
    ports = [ 22 26656 ];
  };

  # Enable (pgp) smartcard support
  pcscd.enable = true;
};

}
