# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
PATH="$HOME/.local/bin:$HOME/bin:$PATH"
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

# ls shorthand and auto-coloring
alias ls="ls --color=auto"
alias la="ls -a"
alias ll="ls -l"

# cd shorthand
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

# Grep shorthand and auto-coloring
alias grep='grep --color=auto'
alias zgrep='zgrep --color=auto'
alias xzgrep='xzgrep --color=auto'
alias egrep='egrep --color=auto'
alias zegrep='zegrep --color=auto'
alias xzegrep='xzegrep --color=auto'
alias fgrep='fgrep --color=auto'
alias zfgrep='zfgrep --color=auto'
alias xzfgrep='xzfgrep --color=auto'

# always use vim-X11
alias vim='vimx'

# custom prompt
PS1="\[\033[2;37m\][\D{%D}] \[\033[1;36m\]\u\[\033[00;35m\]@\h\[\033[00m\]:\[\033[32m\]\w\[\033[00m\]\$"

# set cursor to white block after every command
PROMPT_COMMAND='echo -e "\033[?16;0;224c"'
