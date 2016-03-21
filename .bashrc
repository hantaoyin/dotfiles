# Check for an interactive session
[ -z "$PS1" ] && return

# PS1='[\u@\h \W]\$ '

# A bold white prompt
#PS1='\[\033[1;37m\][\u@\h \W]\$\[\033[0m\] '

bash_prompt_cmd() {
    if [ $? -eq 0 ]; then
        # export PS1='\[\e[1;35m\]\u\[\e[1;36m\]@\[\e[1;34m\]\h\[\e[1;36m\]:\[\e[1;30m\]\w \[\e[1;32m\]^_^\[\e[1;35m\]$\[\e[0m\] '
        export PS1='\[\e[38;5;198m\]\h\[\e[1;31m\]:\[\e[38;5;33m\]\w \[\e[1;32m\]^_^\[\e[1;35m\]$\[\e[0m\] '
    else
        # export PS1='\[\e[1;35m\]\u\[\e[1;36m\]@\[\e[1;34m\]\h\[\e[1;36m\]:\[\e[1;30m\]\w \[\e[1;31m\]o_o\[\e[1;35m\]$\[\e[0m\] '
        export PS1='\[\e[38;5;198m\]\h\[\e[1;31m\]:\[\e[38;5;33m\]\w \[\e[1;31m\]o_o\[\e[1;35m\]$\[\e[0m\] '
    fi
}

PROMPT_COMMAND=bash_prompt_cmd

# The following explanation is from
# http://unix.stackexchange.com/questions/204707/ls-colors-for-256-color-terminal
#
# The argument to a LS_COLORS directive is a string that is written to the
# terminal as part of an escape sequence. When displaying a file name, ls writes
# \e[, then the string associated with the file type, then m, then the file
# name, then \e[0m (where \e represents an escape character). This is the escape
# sequence that tells xterm and compatible terminals (which is most of them
# nowadays) to change colors and other text attributes (CSI Pm m in the
# documentation. ls doesn't care what the sequence of characters means or how
# many semicolons it contains.
#
# Old terminals only supported 8 foreground colors, designated by the numbers 30
# through 37. Terminals that support more colors use the escape sequence
# \e[38;5;Psm where Ps is a color number, or \e[38;2;Pr;Pg;Pbm where Pr, Pg, Pb
# are RGB values. These can be combined with other attributes, e.g. \e38;5;61;1m
# or \e38;2;95;95;175;1m for bold slate blue text.
export LS_COLORS='no=00:fi=00:di=38;5;33:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=38;5;40:*.tar=00;31:*.tgz=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.zip=00;31:*.rar=00;31:*.z=00;31:*.7z=00;31:*.Z=00;31:*.gz=00;31:*.bz2=00;31:*.xz=00;31:*.deb=00;31:*.rpm=00;31:*.jar=00;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.mkv=01;35:*.mp4=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.flac=01;35:*.mp3=01;35:*.mpc=01;35:*.ogg=01;35:*.wav=01;35:'

# added to have a colorful man page output(from Arch Wiki)
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

######################################################################
# bash specific setup
######################################################################
# don't put duplicate lines in the history. See bash(1) for more
# options
shopt -s histappend
export HISTSIZE=100000
export HISTFILESIZE=100000
export HISTCONTROL='erasedups'
#export HISTIGNORE='&:cd *:ls *'

alias ls='ls --color=auto'
alias la='ls -a'
# Do not put additional single quotes when a pathname has special characters.
export QUOTING_STYLE=literal ls

# Short commands are always for common commands
alias em='emacs -nw'
alias emc='emacsclient -n'
export EDITOR=emacs

# Always display English manual.
alias man='LANG=C man'

alias g++='g++ -O3 -Wall -std=c++1y'
alias clang++='clang++ -O3 -Wall -std=c++1y'
alias csi='rlwrap csi'
