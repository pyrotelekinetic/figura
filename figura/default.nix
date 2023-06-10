{ inputs, config, pkgs, ... }: {

imports = [
  ./hardware-configuration.nix
  ./pyrosite.nix
];

pyrosite.enable = true;

hardware = {
  opengl = {
    enable = true;
    driSupport = true;
  };
  bluetooth.enable = true;
};

# Bootloader
boot = {
  loader = {
    systemd-boot = {
      enable = true;
      configurationLimit = 5;
      editor = false;
    };
    efi = {
      canTouchEfiVariables = true;
    };
  };
  plymouth.enable = true;
};

nix = {
  settings = {
    experimental-features = [ "nix-command" "flakes" ];
    auto-optimise-store = true;
  };
  gc = {
    automatic = true;
    dates = "monthly";
    options = "--delete-older-than 30d";
  };
};

# Enable networking
networking = {
  networkmanager.enable = true;
  hostName = "sol";
  hosts = {
    "192.168.1.2" = [ "luna" ];
  };
  firewall = {
    enable = true;
    allowedTCPPorts = [ 6606 ];
    allowedUDPPorts = [ 6606 ];
  };
  interfaces = {
    enp3s0.wakeOnLan.enable = true;
  };
};

# Set time zone
time.timeZone = "America/Phoenix";

# Select internationalisation properties.
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

# Configure keymap in X11
#services.xserver = {
# layout = "us";
# xkbVariant = "";
#};

# Define a user account.
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

services.blueman.enable = true;

# Pipewire
# Remove sound.enable or turn it off if you had it set previously, it seems to cause conflicts with pipewire
sound.enable = false;

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
    # Depracated
    #gtkUsePortal = true;
  };
};

security = {
  polkit.enable = true;
  rtkit.enable = true;
};

# Allow unfree packages
nixpkgs.config.allowUnfree = true;

#  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
#    "steam"
#    "steam-original"
#    "steam-runtime"
#  ];

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

  zip unzip xz gzip bzip2 p7zip
];

# Use librsvg's gdk-pixbuf loader cache file as it enables gdk-pixbuf to load SVG files (important for icons)
environment.sessionVariables = {
  GDK_PIXBUF_MODULE_FILE = "$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)";
};

fonts.fonts = [
  pkgs.freefont_ttf
  pkgs.ibm-plex
];

console = {
  packages = [ pkgs.terminus_font ];
  font = "Lat2-Terminus16";
};

services = {
  # Enable the OpenSSH daemon.
  openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
    kbdInteractiveAuthentication = false;
    openFirewall = true;
    ports = [ 22 26656 ];
  };

  pcscd.enable = true;

  udev = {
    enable = true;
    packages = [ pkgs.stlink ];
  };

  #printing.enable = true;

  #avahi = {
  #  enable = true;
  #  nssmdns = true;
  #};
};

# This value determines the NixOS release from which the default
# settings for stateful data, like file locations and database versions
# on your system were taken. It‘s perfectly fine and recommended to leave
# this value at the release version of the first install of this system.
# Before changing this value read the documentation for this option
# (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
system.stateVersion = "22.11"; # Did you read the comment?

}
