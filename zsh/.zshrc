source $HOME/.zsh_aliases
source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/themes/catppuccin_mocha-zsh-syntax-highlighting.zsh

eval "$(zoxide init --cmd j zsh)"

[ -f "/Users/birdgg/.ghcup/env" ] && . "/Users/birdgg/.ghcup/env" # ghcup-env

if command -v tty >/dev/null 2>&1; then
    export GPG_TTY="$(tty 2>/dev/null)"
fi

if command -v gpgconf >/dev/null 2>&1; then
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket 2>/dev/null)"
    gpgconf --launch gpg-agent
fi

