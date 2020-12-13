# .bashrc

# source global definitions
if [ -f /etc/bashrc ]; then
  source /etc/bashrc
fi

# User specific environment
export PATH="$HOME/.local/bin:$HOME/bin:$PATH"

# User specific aliases and functions

# set default editor
EDITOR=vimx

# set custom wine prefix
export WINEPREFIX="$HOME/data/wine"

#PS1="[\u@\h \W]\$"
source ~/dotfiles/git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=true
PS1="\[\e[0;37m\][\[\e[0;95m\]\u\[\e[0;34m\]@\[\e[0;95m\]\h \[\e[1;32m\]\w\[\e[0;37m\]\[\e[33m\]\$(__git_ps1 ' (%s)')\[\e[0;37m\]]\$ "

# ls shorthand and auto-coloring
alias ls="ls --color=auto"
alias la="ls -a"
alias ll="ls -l"

# cd shorthand
alias ..="cd .."

# grep shorthand and auto-coloring
alias grep='grep --color=auto'
alias zgrep='zgrep --color=auto'
alias xzgrep='xzgrep --color=auto'
alias egrep='egrep --color=auto'
alias zegrep='zegrep --color=auto'
alias xzegrep='xzegrep --color=auto'
alias fgrep='fgrep --color=auto'
alias zfgrep='zfgrep --color=auto'
alias xzfgrep='xzfgrep --color=auto'

# always show prompt in ed
alias ed='ed -p \*'

# man shorthand
alias mank='man -k'

# always use vim-X11
alias vim='vimx'

# date format
alias datef='date +%a\ %b\ %d\ %T\ %Y'

# ncmcpp
alias ncm='ncmpcpp'

# git
alias gitlog='git log --oneline'
