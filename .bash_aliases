
PS1="[\[\033[32m\]\w]\[\033[0m\]\n\[\033[1;36m\]\u\[\033[1;33m\]-> \[\033[0m\\]"      #always pwd

export CLICOLOR=1
export LSCOLORS="gxBxhxDxfxhxhxhxhxcxcx"


#   -----------------------------
#   MAIN UTILITY
#   -----------------------------

alias text='open -a TextEdit'
alias textedit='open -a TextEdit'

alias pwcp='pwd|pbcopy'

alias reload='source ~/.bash_profile'

function tab() {
  osascript 2>/dev/null <<EOF
    tell application "System Events"
      tell process "Terminal" to keystroke "t" using command down
    end
    tell application "Terminal"
      activate
      do script with command "cd \"$PWD\"; $*" in window 1
    end tell
EOF
}

#   -----------------------------
#   DASHBOARD
#   -----------------------------

alias dashoff='defaults write com.apple.dashboard mcx-disabled -boolean YES && killall Dock'
alias dashon='defaults write com.apple.dashboard mcx-disabled -boolean NO && killall Dock'

#   -----------------------------
#   HIDDEN FILES
#   -----------------------------

alias showFiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
alias hideFiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'

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

alias f='open -a Finder ./'                 # Opens current directory in MacOS Finder
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

#   --------------------------------------
#   MOVING TERMINAL
#   --------------------------------------

alias maximize='printf "\e[3;0;0t" && printf "\e[8;0;0t"'
alias normalize="printf '\e[3;500;200t' && printf '\e[8;24;80t'"
alias left='printf "\e[3;0;0t" && resize -s 0 102'
alias right='printf "\e[3;638;0t" && resize -s 0 103'

#   --------------------------------------
#   SHOW SOMETHING
#   --------------------------------------

#   --------------------------------------
#   MAKE PDF
#   --------------------------------------

function MakePDF {
wkhtmltopdf --javascript-delay 20000 --page-size A4 --viewport-size 800x600 $1 $2
}

# youtube-dl download mp3 from link
function yoump3 {
youtube-dl --extract-audio --audio-format mp3 $1
}

#   --------------------------------------
#   TEMPORARY
#   --------------------------------------

alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
