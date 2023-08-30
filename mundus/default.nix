{ pkgs, config, ... }: let
  configHM = config.home-manager.users.cison;
in {

imports = [
  ./pyrosite.nix
];

sops = {
  defaultSopsFile = ../secrets.yaml;
  gnupg ={
    home = configHM.programs.gpg.homedir;
    sshKeyPaths = [];
  };
  secrets.testSecret = {};
};

hardware.opengl = {
  enable = configHM.graphical.enable;
  driSupport = true;
};

nix = {
  package = pkgs.nixVersions.nix_2_13;
  settings = {
    experimental-features = [ "nix-command" "flakes" ];
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
};

xdg.portal = {
  enable = configHM.graphical.enable;
  wlr.enable = true;
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
    } ];
  };
  sudo.enable = false;
  polkit.enable = true;
  rtkit.enable = true;
};

# Sorry Richard
nixpkgs.config.allowUnfree = true;

programs = {
  dconf.enable = configHM.graphical.enable;
  kdeconnect.enable = configHM.graphical.enable;
  steam = {
    enable = configHM.graphical.games;
    remotePlay.openFirewall = false;
    dedicatedServer.openFirewall = false;
  };
  gamemode = {
    enable = configHM.graphical.games;
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

  zip unzip xz gzip bzip2
];

fonts.fonts = [ pkgs.freefont_ttf ];

console = {
  packages = [ pkgs.terminus_font ];
  font = "Lat2-Terminus16";
};

services = {
  smartd = {
    enable = true;
    notifications.x11.enable = configHM.graphical.enable;
  };

  pipewire = {
    enable = configHM.graphical.enable;
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
