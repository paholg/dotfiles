export CARGO_HOME='/home/paho/.cargo'
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export RUST_NEW_ERROR_FORMAT=true

export PUNCH_DIR='/home/paho/.local/share/punch/'
alias punch='python2 /home/paho/git/punch/punch'

export MANPAGER='/usr/bin/env most'
export pacman_program='pacaur'

# disable ksshaskpass
# export SSH_ASKPASS=""

# ssh keychain
if [[ -n $DISPLAY ]]; then
    keychain ~/.ssh/id_rsa &> /dev/null
    . ~/.keychain/`hostname`-sh
    #. ~/.keychain/`hostname`-sh-gpg
fi

autoload zmv

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/lib

# don't remember commands starting with space
setopt histignorespace

# path
path_dirs=($HOME/.gem/ruby/2.3.0/bin $HOME/.cargo/bin $HOME/.multirust/toolchains/stable/cargo/bin $HOME/bin)
for dir in $path_dirs
do
    if [[ $UID -ge 1000 && -d $dir && -z $(echo $PATH | grep -o $dir) ]]
    then
        export PATH=$dir:${PATH}
    fi
done

autoload -Uz compinit
compinit
HISTFILE=~/.zshhistory
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd extendedglob
unsetopt beep
bindkey -e

disable r


alias ls=/usr/bin/exa
alias la="ls -la"
alias ll="ls -l"
alias grep="echo 'use ripgrep!!\n'; /usr/bin/grep"

# For thefuck
eval $(thefuck --alias)

# # Update prompt every second
# TMOUT=1
# TRAPALRM() {
#     zle reset-prompt
# }


# Fix ctrl + arrow keys
bindkey ";5A" up-line
bindkey ";5B" down-line
bindkey ";5C" forward-word
bindkey ";5D" backward-word


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
