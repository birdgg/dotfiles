export ZSH=$HOME/.oh-my-zsh
export ZSH_CUSTOM=$HOME/zsh_custom
ZSH_THEME="cdimascio-lambda"
plugins=(git autojump osx sudo web-search history-substring-search ssh-agent extract)
zstyle :omz:plugins:ssh-agent identities id_rsa id_rsa_company
source $ZSH/oh-my-zsh.sh

alias zshconfig="vim ~/.zshrc"
alias chrome="open -a 'Google Chrome' --args --disable-web-security --user-data-dir=/Users/birdgg/chromedev"
alias ec="emacsclient -nw"
alias ect="emacsclient -c &"
alias config='/usr/local/bin/git --git-dir=$HOME/.myconf/ --work-tree=$HOME'
alias dr="cd ~/.emacs.d && bin/doom refresh"
alias dc="cd ~/.emacs.d && bin/doom compile :core"
export EDITOR='vim'

export PKG_CONFIG_PATH=/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
export ANDROID_SDK=/Users/birdgg/Library/Android/sdk
export ANDROID_NDK=/Users/birdgg/Library/Android/sdk/ndk-bundle
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export N_PREFIX=/Users/birdgg/node_manage
export PATH=$N_PREFIX/bin:$PATH

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
