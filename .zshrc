# set pretzo theme
autoload -Uz promtpinit
promptinit
prompt paho

# path
if [[ $UID -ge 1000 && -d $HOME/bin && -z $(echo $PATH | grep -o $HOME/bin) ]]
then
    export PATH=$HOME/bin:${PATH}
fi

# disable ksshaskpass
export SSH_ASKPASS=""

# ssh keychain
if [[ -n $DISPLAY ]]; then
    keychain ~/.ssh/id_rsa &> /dev/null
    . ~/.keychain/`hostname`-sh
    #. ~/.keychain/`hostname`-sh-gpg
fi


export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/lib


autoload -Uz compinit
compinit
HISTFILE=~/.zshhistory
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd extendedglob
unsetopt beep
bindkey -e


alias ls=/usr/bin/exa
alias la="ls -la"
alias ll="ls -l"
alias rm="/bin/rm"

# added by travis gem
[ -f /home/paho/.travis/travis.sh ] && source /home/paho/.travis/travis.sh

# For thefuck
eval $(thefuck --alias)

# Update prompt every second
TMOUT=1
TRAPALRM() {
    zle reset-prompt
}
