export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="lambda"
plugins=(git vi autojump osx cabal sudo brew web-search history-substring-search)
source $ZSH/oh-my-zsh.sh
alias zshconfig="vim ~/.zshrc"
alias chrome="open -a 'Google Chrome' --args --disable-web-security
"
alias p="proxychains4 -q"
alias ec="emacsclient -nw"
alias ect="emacsclient -c &"
alias config='/usr/local/bin/git --git-dir=$HOME/.myconf/ --work-tree=$HOME'
export EDITOR='emacsclient'
# export SPACEMACSDIR=~/my-spacemacs

export PKG_CONFIG_PATH=/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
export ANDROID_SDK=/Users/birdgg/Library/Android/sdk
export ANDROID_NDK=/Users/birdgg/Library/Android/sdk/ndk-bundle