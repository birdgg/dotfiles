export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="lambda-mod"
plugins=(git autojump osx sudo web-search history-substring-search ssh-agent extract)
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
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`


# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
