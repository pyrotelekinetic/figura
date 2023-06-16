{ pkgs, ... }: rec {

imports = [
  ./graphical
  ./vim.nix
];

home = {
  username = "cison";
  homeDirectory = "/home/cison";

  stateVersion = "22.11";

  # Verify Home Manager and Nixpkgs are on same release ver
  enableNixpkgsReleaseCheck = true;

  sessionVariables = {
    # Set less options
    LESS = "-FR";
    PAGER = "less";
  };

  # Include local binary path
  sessionPath = [
    "$HOME/.local/bin"
  ];

  shellAliases = {
    # ls shorthand and auto-coloring
    ls = "ls --color=auto";
    la = "ls -a";
    ll = "ls -l";
    "l." = "ls -d .*";

    # cd shorthand
    ".." = "cd ..";

    # git shorthand
    gitstat = "git status";
  };
};

# Home Manager managed programs
programs = {
  # Manage HM with HM
  home-manager.enable = true;

  # Make bash work with HM
  bash = let
    git-prompt = pkgs.writeScript "git-prompt.sh"
      ( builtins.readFile ./git-prompt.sh );
  in {
    enable = true;
    initExtra = ''
      # Set prompt
      source ${git-prompt}
      GIT_PS1_SHOWDIRTYSTATE=true
      PS1="\[\e[0;37m\][\[\e[0;95m\]\u\[\e[0;34m\]@\[\e[0;95m\]\h \[\e[1;32m\]\w\[\e[0;37m\]\[\e[33m\]\$(__git_ps1 ' (%s)')\[\e[0;37m\]]\$\[\e[0m\] "

      # Set ssh alias for kitty
      [ "$TERM" = "xterm-kitty" ] && alias ssh='kitty +kitten ssh'
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

  git = {
    enable = true;
    userName = "Carter Ison";
    userEmail = "carter@isons.org";
    signing = {
      key = "3477FBCE6552E74A734485AD3312A18E7AE89BDE";
      signByDefault = true;
    };
    aliases = {
      l = "log --graph";
    };
    ignores = [
      "*.swp"
    ];
    extraConfig = {
      init.defaultBranch = "main";
      log.abbrevCommit = true;
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

  # Connect to phone with kdeconnect app
  kdeconnect = {
    enable = true;
    indicator = true;
  };
};

# XDG user directories
xdg = {
  enable = true;
  userDirs = {
    enable = true;
    desktop = "${home.homeDirectory}/desktop";
    documents = "${home.homeDirectory}/documents";
    download = "${home.homeDirectory}/downloads";
    pictures = "${home.homeDirectory}/pictures";
  };
};

nixpkgs.config.allowUnfree = true;

home.packages = with pkgs; [
  # Utilities
  coreutils-full
  psmisc
  file
  kitty
  libqalculate

  # Blazingly fast
  ripgrep
  fd
  du-dust

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
  pamixer

  # Media
  plex-media-player
  cider
  mpris-scrobbler
  mpd
  mpdris2
  mpc-cli
  ncmpcpp
];

}
