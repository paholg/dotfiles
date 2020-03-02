export CARGO_HOME=$HOME/.cargo
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export RUST_NEW_ERROR_FORMAT=true

export MANPAGER='/usr/bin/env most'

autoload zmv

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/lib

export GPG_TTY=$(tty)

source $HOME/.profile

eval `keychain --quiet --eval id_rsa`

# -----------------------------------------------------------------------------------------------

autoload -Uz compinit
compinit

HISTFILE=~/.zshhistory
HISTSIZE=10000
SAVEHIST=100000
setopt appendhistory
setopt sharehistory
setopt incappendhistory
setopt histignorespace

unsetopt beep
bindkey -e

disable r

alias ls=exa
alias la="ls -la"
alias ll="ls -l"

# Fix ctrl + arrow keys
bindkey ";5A" up-line
bindkey ";5B" down-line
bindkey ";5C" forward-word
bindkey ";5D" backward-word

# ------------------------------------------------------------------------------

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

source /home/paho/.config/broot/launcher/bash/br
