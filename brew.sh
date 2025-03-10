#!/usr/bin/env bash

# Install command-line tools using Homebrew

# Ask for the administrator password upfront
sudo -v

# Keep-alive: update existing `sudo` time stamp until the script has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Update and upgrade already-installed formulae
brew update
brew upgrade

# Install binaries
brew install ack
brew install bat
brew install diff-so-fancy
brew install git
brew install git-lfs
brew install stow
brew install tree
brew install zsh
brew install zsh-autosuggestions
brew install n

# Install casks
brew cask install 1password
brew cask install dropbox
brew cask install visual-studio-code

# Install oh-my-zsh
sh -c "$(wget -O- https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# Remove outdated versions from the cellar
brew cleanup