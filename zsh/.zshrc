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

alias claude="/Users/birdgg/.claude/local/claude"

# Codex
alias cu="npm i -g @openai/codex"

# Claude Code environment variables
export ANTHROPIC_BASE_URL=https://api.moonshot.cn/anthropic/
export ANTHROPIC_AUTH_TOKEN=sk-yZvk5xq9CjvEprRcHiLlQKxdfwfAZKZec49eEPIp7zo7Zrhq
export ANTHROPIC_MODEL=kimi-k2-0905-preview
export ANTHROPIC_SMALL_FAST_MODEL=kimi-k2-turbo-preview
