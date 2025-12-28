#!/usr/bin/env bash

# Install development environment tools: Homebrew, Rust, Bun.js
# This script is idempotent - it will skip already installed tools

set -e

echo "=== Setting up development environment ==="

# Install Homebrew
if command -v brew &> /dev/null; then
    echo "✓ Homebrew is already installed"
else
    echo "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    echo "✓ Homebrew installed"
fi

# Install Rust via rustup
if command -v rustc &> /dev/null; then
    echo "✓ Rust is already installed ($(rustc --version))"
else
    echo "Installing Rust..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    echo "✓ Rust installed"
fi

# Install Bun.js
if command -v bun &> /dev/null; then
    echo "✓ Bun is already installed ($(bun --version))"
else
    echo "Installing Bun..."
    curl -fsSL https://bun.sh/install | bash
    echo "✓ Bun installed"
fi

# Install Ghostty terminal
if [ -d "/Applications/Ghostty.app" ]; then
    echo "✓ Ghostty is already installed"
else
    echo "Installing Ghostty..."
    brew install --cask ghostty
    echo "✓ Ghostty installed"
fi

# Install Claude Code
if command -v claude &> /dev/null; then
    echo "✓ Claude Code is already installed"
else
    echo "Installing Claude Code..."
    bun install -g @anthropic-ai/claude-code
    echo "✓ Claude Code installed"
fi

echo ""
echo "=== Setup complete ==="
echo "Note: You may need to restart your shell or run 'source ~/.zshrc' to use newly installed tools."
