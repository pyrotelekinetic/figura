# .bashrc

# source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
export PATH="$HOME/.local/bin:$HOME/bin:$PATH"

export WINEPREFIX="$HOME/data/wine"

# User specific aliases and functions

#PS1="[\u@\h \W]\$"
PS1="\[\033[31m\][\[\033[1;35m\]\u\[\033[0;34m\]@\[\033[1;34m\]\h \[\033[32m\]\w\[\033[0;37m\]\[\033[31m\]]\[\033[38m\]\$ \[\033[37m\]"

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
