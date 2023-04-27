# .bashrc

# User specific aliases and functions

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
