########################
## Source Other Files ##
########################
if [ -f /etc/zshrc ]; then
  source /etc/zshrc
fi

#################
## Keybindings ##
#################
# Emacs keybindings
bindkey -e

# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# Fix some keys that might not work
# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -A key

key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

# setup key accordingly
[[ -n "${key[Home]}"    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey  "${key[Right]}"   forward-char

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
function zle-line-init () {
    echoti smkx
}
function zle-line-finish () {
    echoti rmkx
}
#zle -N zle-line-init
#zle -N zle-line-finish

############
## Prompt ##
############
PROMPT="%B%F{green}[%F{yellow}%D{%a %b %d}%F{green}, %F{yellow}%*%F{green}]
(%F{cyan}%n%F{yellow}@%F{cyan}%m%F{green})
%F{yellow}[%F{green}%1~%F{yellow}] %F{blue}%h %F{green}%#%f%b "
PS4='xtrace: $LINENO: '

#####################################
## General Environmental Variables ##
#####################################
if type emacsclient > /dev/null 2>&1; then
   export EDITOR=emacsclient-t
   else export EDITOR=vim
fi

if type less > /dev/null 2>&1; then
   export PAGER=less
   else export PAGER=more
fi

# Allows Mac Homebrew to call the Github API with a higher rate limit
export HOMEBREW_GITHUB_API_TOKEN="2296b65523e17d044e836839148344118a7f1095"

export LESSOPEN=""
export BLOCKSIZE=K

set HISTORY=5000
set HISTIGNORE="&:ls:ls *:mutt:top:w"
unset MAILCHECK

umask 22

#############
## Options ##
#############
setopt autocd
setopt auto_resume

# Auto-completion on
autoload -U compinit
compinit

#############
## Aliases ##
#############
alias e='$EDITOR'
alias emacs='emacs -nw'
alias back='cd $OLDPWD'
alias ccs='ssh -Y dglidden@login.ccs.neu.edu'
alias ccv='ssh dglidden@ssh.ccv.brown.edu'
alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'
alias mkpatch="diff -urNa $1 $2"
alias less='less -i -x4 -R -j5'
alias le='echo $?'
alias la='ls -A'
alias ll='ls -l'
alias r='/usr/bin/R --no-save'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias matrixy='tr -c "[:digit:]" " " < /dev/urandom | dd cbs=$COLUMNS conv=unblock | GREP_COLOR="1;32" grep --color "[^ ]"'
alias groupperm='find . -type f -exec chmod 664 {} \; ; find . -type d -exec chmod 775 {} \;'
alias sfind='find . -type f -print | xargs grep -li'

###############
## Functions ##
###############
function up() {
  oldpwd=$PWD
  if [ -z $1 ]; then
    cd ..
  elif [ $1 -gt 0 ]; then
    let count=0
    while [ $count -lt $1 ]; do
      cd ..
      let count=count+1
    done
  else
    echo "Argument must be a positive integer."
  fi
  OLDPWD=$oldpwd
}

function joinpdf() {
  gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=merged.pdf "$@"
}

# I want ls to show me dotfiles when I'm not in my ~
function ls() {
  if [[ $PWD == $HOME ]]; then
    if [[ $OSTYPE == darwin* ]]; then
      /bin/ls -G "$@"
    elif [[ $OSTYPE == freebsd* ]]; then
      /bin/ls -CG "$@"
    elif [[ $OSTYPE == linux* ]]; then
      /bin/ls -v --color=always "$@"
    else
      /bin/ls
    fi
  else
    if [[ $OSTYPE == darwin* ]]; then
      /bin/ls -GA "$@"
    elif [[ $OSTYPE == freebsd* ]]; then
      /bin/ls -CGa "$@"
    elif [[ $OSTYPE == linux* ]]; then
      /bin/ls -Av --color=always "$@"
    else
      /bin/ls
    fi
  fi
}

function webperm() {
  ifs=$IFS
  IFS=$'\n'
  for file in `find . -type f -print` ; do
    if [ -x "$file" ]; then
      chmod 755 "$file"
    else
      chmod 644 "$file"
    fi
  done
  for dir in `find . -type d -print` ; do
    chmod 755 "$dir"
  done
  IFS=$ifs
}

function safeperm() {
  ifs=$IFS
  IFS=$'\n'
  for file in `find . -type f -print` ; do
    if [ -x "$file" ]; then
      chmod 700 "$file"
    else
      chmod 600 "$file"
    fi
  done
  for dir in `find . -type d -print` ; do
    chmod 700 "$dir"
  done
  IFS=$ifs
}

function reallykill() {
  PID=$1
  RETVAL=0

  for signal in "TERM" "INT" "HUP" "KILL"; do
    kill -$signal $PID
    RETVAL=$?
    [ $RETVAL -eq 0 ] && break
    echo "warning: kill failed: pid=$PID, signal=$signal" >&2
    sleep 1
  done

  return $RETVAL
}
