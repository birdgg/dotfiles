#!/usr/bin/env bash

# Install Homebrew formulae
source brew.sh

# Set up symlinks using stow
source symlinks.sh

# Ensure GnuPG is configured for signing commits
source gpg.sh
