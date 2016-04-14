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

alias rm="mv -t /tmp "

# For thefuck
eval $(thefuck --alias)

# Update prompt every second
TMOUT=1
TRAPALRM() {
    zle reset-prompt
}

# Setup theme

precmd () {
    vcs_info
}

prompt_setup () {
    # Load functions
    autoload -Uz add-zsh-hook
    autoload -Uz vcs_info

    setopt prompt_subst

    # Set vcs_info parameters.
    zstyle ':vcs_info:*' enable bzr git hg svn
    zstyle ':vcs_info:*' check-for-changes true
    zstyle ':vcs_info:*' formats '%F{magenta}%b%f%c%u'
    zstyle ':vcs_info:*' actionformats '%F{magenta}%b%f%c%u %F{yellow}(%a)%f'
    zstyle ':vcs_info:*' stagedstr '%F{green} ⚙%f'
    zstyle ':vcs_info:*' unstagedstr '%F{red} ⚙%f'

    # Set vcs_info hooks.
    zstyle ':vcs_info:*+start-up:*' hooks set_novcs_prompt_symbol
    zstyle ':vcs_info:git*+set-message:*' hooks set_vcs_prompt_symbol git_precmd
    zstyle ':vcs_info:*+set-message:*' hooks set_vcs_prompt_symbol
    # ------------------------------------------------------------------------------------

    local -h user host dir vcs time prompt
    user='%F{magenta}%n%f'
    host='%F{red}%m%f'
    dir='%F{green}%~%f'
    time='%F{cyan}%*%f'
    prompt='%F{cyan}❯❯ %f'


    PROMPT="$user@$host:$dir \${vcs_info_msg_0_}
$prompt"
    RPROMPT="$time"
    SPROMPT='zsh: correct %F{red}%R%f to %F{green}%r%f [nyae]? '


    return 0
}

prompt_setup '$@'
