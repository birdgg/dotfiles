export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin
# Homebrew
export PATH="/opt/homebrew/bin:$PATH"

# Pnpm
export PNPM_HOME="$HOME/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"

# Bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# N
export N_PREFIX="$HOME/n"
export PATH=$N_PREFIX/bin:$PATH

# Foundry
export PATH="$PATH:/Users/birdgg/.foundry/bin"

# Emacs
export PATH="/opt/homebrew/opt/emacs-plus@31/bin:$HOME/.emacs.d/bin:$PATH"

# Haskell
export PATH="$HOME/.local/bin:$PATH"

#Rust
. $HOME/.cargo/env

# Golang
export PATH="$PATH:$HOME/go/bin"
