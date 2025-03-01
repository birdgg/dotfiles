set fish_greeting ""

# aliases
alias ls "ls -p -G"
alias la "ls -A"
alias ll "ls -l"
alias lla "ll -A"
alias g git
alias b bun
alias shad "bunx --bun shadcn@canary"

command -qv nvim && alias vim nvim

set -gx EDITOR nvim

set -gx PATH ~/bin $PATH
set -gx PATH ~/.local/bin $PATH

set -gx PATH /opt/homebrew/bin $PATH
set -gx PATH /opt/homebrew/sbin $PATH
set -gx PATH ~/bin $PATH

# NodeJS
set -gx PATH node_modules/.bin $PATH

# BunJS
set -gx BUN_INSTALL ~/.bun
set -gx PATH $BUN_INSTALL/bin $PATH

# N
set -gx N_PREFIX ~/node_manage
set -gx PATH N_PREFIX/bin $PATH

gpgconf --launch gpg-agent

# Ensure that GPG Agent is used as the SSH agent
set -e SSH_AUTH_SOCK
set -U -x SSH_AUTH_SOCK ~/.gnupg/S.gpg-agent.ssh

zoxide init fish --cmd cd | source