### .bashrc initialization file for dave
# see also .bashrc_home and .bashrc_work
#
### OS independent stuff first
#
# Set prompt
PS4='xtrace: $LINENO: '

### General environmental variables
if type vim > /dev/null 2>&1; then
   export EDITOR=vim
   else export EDITOR=vi
fi

if type less > /dev/null 2>&1; then
   export PAGER=less
   else export PAGER=more
fi

export LESSOPEN=""
export BLOCKSIZE=K

export LIB_HOME=~/lib
export BIN_HOME=~/bin
export PATH=${BIN_HOME}:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:/usr/X11R6/bin:/opt/local/bin:/usr/games:.

# CLASSPATH
export JAR=${LIB_HOME}/jar
export CLASSPATH=$JAR/jrap.jar:$JAR/TerminalIO.jar:$JAR/derby.jar:$JAR/derbyclient.jar:$JAR/derbynet.jar:$JAR/derbyrun.jar:$JAR/derbytools.jar:.

# Google Web Toolkit
export GWT_VERSION=2.4.0

set HISTORY=5000
set HISTIGNORE="&:ls:ls *:mutt:top:w"
# A righteous umask
umask 22
#export GREP_COLOR='1;32'
unset MAILCHECK

shopt -s cdspell
shopt -s extglob
shopt -s checkwinsize
shopt -s no_empty_cmd_completion
complete -d cd

### Aliases
if [ -f $HOME/bin/svn ]; then
        alias   svn="~/bin/svn"
fi
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
alias exp='PS1="\$ "'
alias myp="PS1='\[\e[1m\][\t] [\u\[\e[m\]@\[\e[1m\]\h] \[\e[32m\][\w]\[\e[m\]\n \[\e[1;33m\]\#\$ \[\e[m\]'"

alias matrixy='tr -c "[:digit:]" " " < /dev/urandom | dd cbs=$COLUMNS conv=unblock | GREP_COLOR="1;32" grep --color "[^ ]"'
alias groupperm='find . -type f -exec chmod 664 {} \; ; find . -type d -exec chmod 775 {} \;'

##### FUNCTIONS #####
# SVN tricks
M () { svn diff "$*" | gvimdiff -; }
G () { svn diff "$*" | gvimdiff -; }
U () { svn diff "$*" | gvimdiff -; }
R () { svn diff; }
A () { true; }
D () { true; }
I () { true; }
#? () { true; }

# find a string in any file within current directory tree
function sfind () {
  find . -type f -print | xargs grep -li "$1" 2>/dev/null
}

# tricks with cd
function up () {
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
if [ "$PWD" == "$HOME" ]; then
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
        if [[ "$OSTYPE" == darwin* ]]; then
            /bin/ls -GA "$@"
        elif [[ "$OSTYPE" == freebsd* ]]; then
            /bin/ls -CGa "$@"
        elif [[ "$OSTYPE" == linux* ]]; then
            /bin/ls -Av --color=always "$@"
        else
            /bin/ls
        fi
fi
}

# Better perms fixing
function webperm () {
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

function safeperm () {
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

# Kill something
function reallykill () {
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

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Cygwin X
# ########
# Set the display variable
export DISPLAY=:0.0

# Using CVS with SSH
# ##################
export CVS_RSH=ssh
export CVSROOT=:ext:dglidden@login.ccs.neu.edu:/home/dglidden/cvsrepos


##########################################################
##  colored prompt 
##########################################################

if ${use_color} ; then
    if [[ ${EUID} == 0 ]] ; then
        PS1='\[\e[1;32m\][\[\e[1;33m\]\d\[\e[1;32m\], \[\e[1;33m\]\t\[\e[1;32m\]] \n\[\e[1;34m\][\[\e[1;31m\]\h\[\e[1;34m\]] \n\[\e[1;33m\][\[\e[1;32m\]\w\[\e[1;33m\]] \[\e[1;31m\]->> \[\e[1;32m\]\$ \[\e[0m\]'
    else
        PS1='\[\e[1;32m\][\[\e[1;33m\]\d\[\e[1;32m\], \[\e[1;33m\]\t\[\e[1;32m\]] \n\[\e[1;32m\](\[\e[1;37m\]\u\[\e[1;33m\]@\[\e[1;37m\]\h\[\e[1;32m\]) \n\[\e[1;33m\][\[\e[1;32m\]\W\[\e[1;33m\]] \[\e[1;34m\]\# \[\e[1;32m\]\$ \[\e[0m\]'
    fi
else
    if [[ ${EUID} == 0 ]] ; then
        # show root@ when we don't have colors
        PS1='\u@\h \W \$ '
    else
        PS1='\u@\h \w \$ '
    fi
fi
#########################################################
