{pkgs, ...}: {

home = {
  username = "cison";
  homeDirectory = "/home/cison";

  stateVersion = "22.11";

  # Verify Home Manager and Nixpkgs are on same release ver
  enableNixpkgsReleaseCheck = true;

  # Set mouse cursor for gtk and x11
  pointerCursor = {
    name = "breeze_cursors";
    package = pkgs.breeze-gtk;
    size = 24;
    gtk.enable = true;
    x11.enable = true;
  };

  sessionVariables = {
    # Tell things to use wayland
    MOZ_ENABLE_WAYLAND = 1;
    XDG_CURRENT_DESKTOP = "sway";
    NIXOS_OZONE_WL="1";

    # Set less options
    LESS = "-FR";

    # Set default programs
    PAGER = "less";
    EDITOR = "vim";
  };

  # Include local binary path
  sessionPath = [
    "$HOME/.local/bin"
  ];
};

# Home Manager managed programs
programs = {
  # Manage HM with HM
  home-manager.enable = true;

  # Make bash work with HM
  bash = {
    enable = true;
    # Set prompt
    initExtra = ''
      source ~/dotfiles/git-prompt.sh
      GIT_PS1_SHOWDIRTYSTATE=true
      PS1="\[\e[0;37m\][\[\e[0;95m\]\u\[\e[0;34m\]@\[\e[0;95m\]\h \[\e[1;32m\]\w\[\e[0;37m\]\[\e[33m\]\$(__git_ps1 ' (%s)')\[\e[0;37m\]]\$\[\e[0m\] "
    '';
    # I should do this in a better way
    bashrcExtra = ''
      source ~/dotfiles/.bashrc
    '';
  };

  gpg = {
    enable = true;
    mutableKeys = true;
    mutableTrust = true;
    settings = {
      keyid-format = "none";
      with-subkey-fingerprint = true;
    };
  };

  password-store = {
    enable = true;
    settings = {
      PASSWORD_STORE_DIR = "$HOME/.password-store";
      PASSWORD_STORE_GENERATED_LENGTH = "64";
    };
  };
};

# Home Manager managed services
services = {
  gpg-agent = {
    enable = true;
    enableBashIntegration = true;
    enableScDaemon = true;
    enableSshSupport = true;
    sshKeys = [ "3C305675F93CC000802C4DE9F6DDF464ACDD3DE3" ];
    grabKeyboardAndMouse = true;
    pinentryFlavor = "qt";
  };
};

gtk = {
  enable = true;
};

qt = {
  enable = true;
  style = {
    package = pkgs.adwaita-qt;
    name = "adwaita-dark";
  };
};

nixpkgs.config.allowUnfree = true;

home.packages = with pkgs; [
  # Utilities
  coreutils-full
  psmisc
  file
  vim
  git
  kitty
  libqalculate

  # Blazingly fast
  ripgrep
  fd
  du-dust

  # Wayland desktop
  sway
  wayland
  wl-clipboard
  xdg-utils
  bemenu
  dunst
  grim
  slurp
  imv

  # Drip
  neofetch
  bottom
  ibm-plex

  # Social
  signal-desktop-beta

  # Tools
  firefox-wayland
  gimp
  kicad-small
  pavucontrol
  pulsemixer
  playerctl

  # Media
  plex-media-player
  cider

  # Games
  prismlauncher
  osu-lazer
  heroic
];

}
