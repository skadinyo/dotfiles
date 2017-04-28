
PS1="[\[\033[32m\]\w]\[\033[0m\]\n\[\033[1;36m\]\u\[\033[1;33m\]-> \[\033[0m\\]"      #always pwd

export CLICOLOR=1
export LSCOLORS="gxBxhxDxfxhxhxhxhxcxcx"

if [ "$(uname)" == "Darwin" ]; then
   source ~/dotskadinyo/.bash_darwin
fi
