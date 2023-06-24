{ pkgs, ... }: {

hardware = {
  opengl = {
    enable = true;
    driSupport = true;
  };
  bluetooth.enable = true;
};

nix = {
  package = pkgs.nixVersions.nix_2_13;
  settings = {
    experimental-features = [ "nix-command" "flakes" ];
    auto-optimise-store = true;
  };
  gc = {
    automatic = true;
    dates = "monthly";
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

networking = {
  networkmanager.enable = true;
  firewall.enable = true;
};

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
  ];
  openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC4X2NdKZeEjmuezkLudU8HGnPnxCMC7g7LkWJdr+Rn9K/VuJOHSHXWeWXtxiAQiPjRiE71FflXLmTTUuPZFM6GCKSxd3uZ/Sx6Cw4qQdWoIfuNdJR+Gl89uEIpO2URBHmpxHRRB58qTLyBz86giU2jIq18jCvu9OdubvqBUavHmUrz63g13TNRAwbOBOqt8Mx3RWfvw0+KXOD8BykWLSKsOfOaqErAcAzEuhGHZ1boR+HcwgHis9Q7/YWSfojne3wqftSNHlSjYKDziI9cC3CPSsjidMZkUuhL6vP7gzPln9H0sktyVmrQXxlzMD4zc9IQLiODemWjOPVuYn1alOsFvfBIiQIB3maU5PV33uwba1i1rYgX8c+nSscokdD+F4S4ILlI7vFSYZVLgun/DPtshNiewT7y0uJgNmiJUzU75EO0uKpfnw8Sr/MoisiIglLDzUYjTOv7sHI0YXGRYrwpRkokuxTDZVoHWodiZp7Jej8VpEMzu5zu/AG5c00OWCTSD33bgpCahhSqy/vPBuqqmkBwML7hN/mIw+U/EUuT5uE392ktfw2uJuLQWwVPAkytaNCWgsmKzED0fhjoJVY1T9nbzP50iAJf+uvnFvJ+jJD+nx97ZghsmwivfRbFLKC2iH4eBZjjcWhDZ8aSqyFnInuaiY4YIkLomRdJbnhaOQ== (none)"
  ];
};

services.pipewire = {
  enable = true;

  alsa = {
    enable = true;
    support32Bit = true;
  };

  pulse.enable = true;

  media-session.enable = false;
  wireplumber.enable = true;
};

xdg = {
  portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
    ];
  };
};

security = {
  polkit.enable = true;
  rtkit.enable = true;
};

# Sorry Richard
nixpkgs.config.allowUnfree = true;

programs = {
  dconf.enable = true;
  kdeconnect.enable = true;
  steam = {
    enable = true;
    remotePlay.openFirewall = false;
    dedicatedServer.openFirewall = false;
  };
};

environment.systemPackages = with pkgs; [
  coreutils-full
  git
  vim

  nix-tree

  usbutils
  pciutils
  lshw
  lm_sensors
  fwts

  ntfs3g

  zip unzip xz gzip bzip2
];

fonts.fonts = [
  pkgs.freefont_ttf
  pkgs.ibm-plex
];

console = {
  packages = [ pkgs.terminus_font ];
  font = "Lat2-Terminus16";
};

services = {
  openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
    kbdInteractiveAuthentication = false;
    openFirewall = true;
    ports = [ 22 26656 ];
  };

  # Enable (pgp) smartcard support
  pcscd.enable = true;
};

system.stateVersion = "22.11";

}
