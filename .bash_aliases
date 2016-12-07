
PS1="[\[\033[32m\]\w]\[\033[0m\]\n\[\033[1;36m\]\u\[\033[1;33m\]-> \[\033[0m\\]"      #always pwd

export CLICOLOR=1
export LSCOLORS="gxBxhxDxfxhxhxhxhxcxcx"



#   -----------------------------
#   MAKE TERMINAL BETTER
#   -----------------------------

alias sd="ls -a"
alias la="ls -a"
alias ll="ls -l"
alias today='date +"%A, %B %d, %Y"'

alias cd..='cd ../'                         # Go back 1 directory level (for fast typers)
alias ,='cd ~/'
alias .='cd ../'

alias ~="cd ~"                              # Go Home
alias home='~'                              # Go Home
alias c='clear'                             # Clear terminal display

#   --------------------------------------
#   NETWORKING STUFF
#   --------------------------------------

#Ping to google
alias gping="ping -s 16 www.google.com"

#find ip address
alias myip='curl ip.appspot.com'

if [ "$(uname)" == "Darwin" ]; then
   source ~/dotskadinyo/.bash_darwin
fi
