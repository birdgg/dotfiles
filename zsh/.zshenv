export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin
# Homebrew
export PATH="/opt/homebrew/bin:$PATH"

# Bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# N
export N_PREFIX="$HOME/n"
export PATH=$N_PREFIX/bin:$PATH

# Haskell
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"

#Rust
. $HOME/.cargo/env

# Golang
export PATH="$PATH:$HOME/go/bin"
