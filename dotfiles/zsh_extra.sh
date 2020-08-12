# ------------------------------------------------------------------------------
# Fix ctrl + left/right arrows
autoload -U select-word-style
select-word-style bash
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
# ------------------------------------------------------------------------------
# Program specific settings

if `command -v emacsclient >/dev/null 2>&1`; then
    export EDITOR="emacsclient -c"
elif `command -v emacs >/dev/null 2>&1`; then
    export EDITOR="emacs -nw -c"
elif `command -v vim >/dev/null 2>&1`; then
    export EDITOR="vim"
fi

if `command -v keychain >/dev/null 2>&1`; then
    eval `keychain --quiet --eval id_rsa`
fi

if `command -v exa >/dev/null 2>&1`; then
    alias ls=exa
    alias la="ls -la"
    alias ll="ls -l"
fi

if `command -v rustc >/dev/null 2>&1`; then
    export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
    export RUST_NEW_ERROR_FORMAT=true
    export CARGO_HOME=$HOME/.cargo
fi

if `/usr/bin/env most >/dev/null 2>&1`; then
    export MANPAGER='/usr/bin/env most'
fi

# ------------------------------------------------------------------------------
# Load host-specific settings
source $HOME/dotfiles/`hostname`/zshrc

# ------------------------------------------------------------------------------
# Set up prompt

# Helper functions taken from https://gist.github.com/romkatv/2a107ef9314f0d5f76563725b42f7cab
function prompt-length() {
    emulate -L zsh
    local COLUMNS=${2:-$COLUMNS}
    local -i x y=$#1 m
    if (( y )); then
        while (( ${${(%):-$1%$y(l.1.0)}[-1]} )); do
            x=y
            (( y *= 2 ));
        done
        local xy
        while (( y > x + 1 )); do
            m=$(( x + (y - x) / 2 ))
            typeset ${${(%):-$1%$m(l.x.y)}[-1]}=$m
        done
    fi
    echo $x
}

function fill-line() {
    emulate -L zsh
    local left_len=$(prompt-length $1)
    local right_len=$(prompt-length $2 9999)
    local pad_len=$((COLUMNS - left_len - right_len - ${ZLE_RPROMPT_INDENT:-1}))
    if (( pad_len < 1 )); then
        # Not enough space for the right part. Drop it.
        echo -E - ${1}
    else
        local pad=${(pl.$pad_len.. .)}  # pad_len spaces
        echo -E - ${1}${pad}${2}
    fi
}

precmd () {
    vcs_info
}

set_prompt () {
    emulate -L zsh
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

    # Make completions nicer
    zstyle ':completion:*' menu select

    # ------------------------------------------------------------------------------------

    local -h user host dir vcs time prompt
    user="%F{${ZSH_USER_COLOR:-magenta}}%n%f"
    host="%F{${ZSH_HOST_COLOR:-white}}%m%f"
    dir='%F{cyan}%~%f'
    time='%F{cyan}%*%f'
    top_left="$user@$host:$dir ${vcs_info_msg_0_}"
    bottom_left='%F{cyan}$ %f'
    top_right="$time"

    PROMPT="$(fill-line "$top_left" "$top_right")"$'\n'$bottom_left
    SPROMPT='zsh: correct %F{red}%R%f to %F{green}%r%f [nyae]? '

    return 0
}

autoload -Uz vcs_info
autoload -Uz add-zsh-hook

add-zsh-hook precmd set_prompt
