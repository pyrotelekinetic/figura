# .bashrc

# User specific environment
export PATH="$HOME/.local/bin:$HOME/bin:$PATH"

# User specific aliases and functions

# set less options
export PAGER="less -FR"
alias less="less -FR"

# set default editor
export EDITOR=vim

#PS1="[\u@\h \W]\$"
source ~/dotfiles/git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=true
PS1="\[\e[0;37m\][\[\e[0;95m\]\u\[\e[0;34m\]@\[\e[0;95m\]\h \[\e[1;32m\]\w\[\e[0;37m\]\[\e[33m\]\$(__git_ps1 ' (%s)')\[\e[0;37m\]]\$\[\e[0m\] "

# ls shorthand and auto-coloring
alias ls="ls --color=auto"
alias la="ls -a"
alias ll="ls -l"
alias l.="ls -d .*"

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

# date format
alias datef='date +%a\ %b\ %d\ %T\ %Y'

# ncmcpp
alias ncm='ncmpcpp'

# git shorthand
alias gitlog='git log --oneline'
alias gitstat='git status'

# xclip
alias xcl='xclip -selection CLIPBOARD'
