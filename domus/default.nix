{ lib, pkgs, config, ... }: {

imports = [
  ./graphical
  ./vim.nix
];

nix.settings.bash-prompt-prefix = ''\[\e[0;34m\] \[\e[0m\]'';

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

  # Custom compose sequences
  file.".XCompose" = lib.mkIf config.graphical.enable { text = ''
    include "%L"
    # For some reason including the unicode point seems to break some defualt sequences
    <Multi_key> <backslash> <backslash>  : "λ" # U03BB # GREEK SMALL LETTER LAMDA
  '';
  };
};

# Home Manager managed programs
programs = {
  # Manage HM with HM
  home-manager.enable = true;

  # Make bash work with HM
  bash = {
    enable = true;
    initExtra = ''
      # Set prompt
      source ${pkgs.git}/share/bash-completion/completions/git-prompt.sh
      GIT_PS1_SHOWDIRTYSTATE=true
      PS1="\[\e[0;37m\][\[\e[0;95m\]\u\[\e[0;34m\]@\[\e[0;95m\]\h \[\e[1;32m\]\w\[\e[0;37m\]\[\e[33m\]\$(__git_ps1 ' (%s)')\[\e[0;37m\]]\$\[\e[0m\] "

      # Set ssh alias for kitty
      [ "$TERM" = "xterm-kitty" ] && alias ssh='kitty +kitten ssh'

      # Tab complete after doas
      complete -F _root_command doas
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
      PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
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
    delta.enable = true;
    aliases = {
      l = "log --graph";
      hash = "log -n 1 --format=%H";
    };
    ignores = [
      "*.swp"
    ];
    extraConfig = {
      init.defaultBranch = "main";
      log.abbrevCommit = true;
      diff.colorMoved = "default";
    };
  };

  ssh = {
    enable = true;
    controlMaster = "auto";
    controlPath = "${config.home.homeDirectory}/.ssh/control/%r@%n:%p";
    controlPersist = "5m";
    matchBlocks = with lib.hm.dag; {
      luna = {
        hostname = "184.179.188.130";
        port = 2885;
      };
      sol = entryAfter [ luna ] {
        hostname = "192.168.1.8";
        proxyJump = "luna";
      };
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
    desktop = "${config.home.homeDirectory}/desktop";
    documents = "${config.home.homeDirectory}/documents";
    download = "${config.home.homeDirectory}/downloads";
    pictures = "${config.home.homeDirectory}/pictures";
  };
};

home.packages = with pkgs; [
  # Utilities
  coreutils-full
  psmisc
  file
  libqalculate

  # Blazingly fast
  ripgrep
  fd
  du-dust

  # Drip
  neofetch
  bottom
];

}
