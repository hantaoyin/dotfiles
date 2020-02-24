# Greeting message - No greeting.
set fish_greeting

# Do not put additional single quotes when a pathname has special characters.
set -xU QUOTING_STYLE 'literal'
set -xU EDITOR 'emacs'

set -xU LS_COLORS 'no=00:fi=00:di=38;5;33:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=38;5;40:*.tar=00;31:*.tgz=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.zip=00;31:*.rar=00;31:*.z=00;31:*.7z=00;31:*.Z=00;31:*.gz=00;31:*.bz2=00;31:*.xz=00;31:*.deb=00;31:*.rpm=00;31:*.jar=00;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.mkv=01;35:*.mp4=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.flac=01;35:*.mp3=01;35:*.mpc=01;35:*.ogg=01;35:*.wav=01;35:'

#set -xU XMODIFIERS '@im=ibus'

# Doesn't work yet.
# set -xU LESS_TERMCAP_mb '\E[01;31m'
# set -xU LESS_TERMCAP_md '\E[01;31m'
# set -xU LESS_TERMCAP_me '\E[0m'
# set -xU LESS_TERMCAP_se '\E[0m'
# set -xU LESS_TERMCAP_so '\E[01;44;33m'
# set -xU LESS_TERMCAP_ue '\E[0m'
# set -xU LESS_TERMCAP_us '\E[01;32m'

alias ls='ls --color=auto'
alias em='emacs -nw'
alias emc='emacsclient -n'
alias g++='g++ -O3 -Wall -std=c++17 -Wfatal-errors'
alias clang++='clang++ -O3 -Wall -std=c++17 -Wfatal-errors'
alias python=python3

# No core files.
# ulimit -c 0

function fish_prompt -d "Write out the prompt"
  printf '\e[38;5;198m%s:\e[38;5;33m%s\e[1;35m$ ' (hostname) (echo $PWD | sed -e "s|^$HOME|~|")
end
