eval "$(starship init zsh)"

source $HOME/.zsh_aliases
source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/themes/catppuccin_mocha-zsh-syntax-highlighting.zsh

eval "$(zoxide init --cmd cd zsh)"

[ -f "/Users/birdgg/.ghcup/env" ] && . "/Users/birdgg/.ghcup/env" # ghcup-env

alias lgit='lazygit'

# bun completions
[ -s "/Users/birdgg/.bun/_bun" ] && source "/Users/birdgg/.bun/_bun"

export GPG_TTY=$(tty)
