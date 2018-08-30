export CARGO_HOME=$HOME/.cargo
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export RUST_NEW_ERROR_FORMAT=true

export PUNCH_DIR=$HOME/.local/share/punch/
alias punch='python2 /home/paho/git/punch/punch'

export MANPAGER='/usr/bin/env most'

export GOPATH=$HOME/.go

# . $HOME/bin/set_path

autoload zmv

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/lib
export ARDUINO_PATH=/usr/local/arduino

# -----------------------------------------------------------------------------------------------
# Outreach Stuff

eval "$(rbenv init -)"

# eval "$(docker-machine env outreach)"
export OUTREACH_PROJECT_ROOT='/home/paho/src'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
# . "/usr/local/opt/nvm/nvm.sh"
# [[ -s "/Users/paho/.gvm/scripts/gvm" ]] && source "/Users/paho/.gvm/scripts/gvm"
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

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

# https://github.com/rupa/z
. $HOME/git/z/z.sh

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
