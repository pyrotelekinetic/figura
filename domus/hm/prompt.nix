{ lib, pkgs, ... }: let
  fancy = ''\[\e[1;32m\]❯ \[\e[0m\]'';
  ascii = ''\[\e[1;32m\]> \[\e[0m\]'';

  # I hate this but it works. Yes, it has to be one line.
  pointer = ''$([ "$TERM" != "linux" ] && echo "${fancy}")$([ "$TERM" = "linux" ] && echo "${ascii}")'';
in {

nix.settings.bash-prompt-suffix = ''\[\e[2D\]\[\e[0;34m\] ${pointer}'';

programs.bash.initExtra = ''
  source ${pkgs.git}/share/bash-completion/completions/git-prompt.sh
  GIT_PS1_SHOWDIRTYSTATE=true
  GIT_PS1_SHOWUNTRACKEDFILES=true
  GIT_PS1_STATESEPARATOR=""
  PS1="\[\e[3;34m\]\w\[\e[0;33m\]\$(__git_ps1 '(%s)')\[\e[0m\]\n${pointer}"
'';

}
