eval "$(starship init zsh)"

# plugins=(git sudo history-substring-search ssh-agent extract gpg-agent)
# zstyle :omz:plugins:ssh-agent identities github
# source $ZSH/oh-my-zsh.sh
source $HOME/.zsh_aliases

export EDITOR='nvim'

eval "$(zoxide init --cmd cd zsh)"
