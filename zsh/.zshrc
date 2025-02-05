export ZSH=$HOME/.oh-my-zsh
export ZSH_CUSTOM=$HOME/zsh_custom
plugins=(git autojump sudo web-search history-substring-search ssh-agent extract gpg-agent)
zstyle :omz:plugins:ssh-agent identities github
source $ZSH/oh-my-zsh.sh
source $HOME/.zsh_aliases

export EDITOR='vim'

# pnpm
export PNPM_HOME="/Users/birdgg/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"
# pnpm end

# bun completions
[ -s "/Users/birdgg/.bun/_bun" ] && source "/Users/birdgg/.bun/_bun"