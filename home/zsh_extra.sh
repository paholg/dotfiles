# ------------------------------------------------------------------------------
# Fix ctrl + left/right arrows
autoload -U select-word-style
select-word-style bash
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

# ------------------------------------------------------------------------------
# Load host-specific settings
if [ -f $HOME/dotfiles/`hostname`/zshrc ]; then
    source $HOME/dotfiles/`hostname`/zshrc
fi

# # ------------------------------------------------------------------------------
# Set title
# See ArchWiki: https://wiki.archlinux.org/title/zsh#xterm_title
title_host() {
    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        host="%m: "
    else
        host=""
    fi
    echo $host
}

xterm_title_precmd() {
	  print -Pn -- "\e]2;$(title_host)%~\a"
}

xterm_title_preexec() {
	print -Pn -- "\e]2;$(title_host)" && print -n -- "${(q)1}" && print -Pn -- ' (%~)\a'
}

precmd_functions+=(xterm_title_precmd)
preexec_functions+=(xterm_title_preexec)
