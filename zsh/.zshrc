export ZSH=$HOME/.oh-my-zsh
export ZSH_CUSTOM=$HOME/zsh_custom
plugins=(git autojump sudo web-search history-substring-search ssh-agent extract gpg-agent)
zstyle :omz:plugins:ssh-agent identities github
source $ZSH/oh-my-zsh.sh

export EDITOR='vim'

export PKG_CONFIG_PATH=/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
export ANDROID_SDK=/Users/birdgg/Library/Android/sdk
export ANDROID_NDK=/Users/birdgg/Library/Android/sdk/ndk-bundle
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

# pnpm
export PNPM_HOME="/Users/birdgg/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"
# pnpm end

# bun completions
[ -s "/Users/birdgg/.bun/_bun" ] && source "/Users/birdgg/.bun/_bun"


# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
