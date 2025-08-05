# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a Doom Emacs configuration directory containing four core configuration files:

- `init.el` - Controls which Doom modules are enabled and module configuration
- `config.el` - Personal configuration and customizations
- `packages.el` - Package declarations and external package installations
- `custom.el` - Emacs custom variables (auto-generated)

## Common Commands

### Doom Management
- `doom sync` - Synchronize packages after modifying init.el or packages.el (most important command)
- `doom upgrade` - Update Doom Emacs and packages
- `doom doctor` - Diagnose configuration issues
- `doom build` - Rebuild configuration
- `doom purge` - Clean unused packages
- `doom reload` - Reload configuration without restarting Emacs

The doom executable is located at `$HOME/.emacs.d/bin/doom`.

## Configuration Architecture

### Module System
Doom uses a modular architecture configured in `init.el` using the `doom!` macro. The configuration is organized into categories:
- `:input` - Input methods and language support
- `:completion` - Completion frameworks (using Corfu + Vertico)
- `:ui` - User interface enhancements
- `:editor` - Editing features (Evil mode enabled)
- `:emacs` - Core Emacs functionality
- `:term` - Terminal emulation (vterm enabled)
- `:checkers` - Syntax checking
- `:tools` - Development tools (LSP, Magit, Tree-sitter enabled)
- `:lang` - Language-specific support (Haskell, JavaScript, Org-mode configured)
- `:config` - Final configuration

### Key Features Enabled
- Evil mode with vim keybindings
- LSP support for development
- Catppuccin theme with JetBrains Mono font
- Org-mode with journal, roam, and pomodoro features
- Haskell development with LSP and tree-sitter
- JavaScript development with LSP and tree-sitter
- Claude Code IDE integration via `claude-code-ide` package

### Package Management
New packages are declared in `packages.el` using the `package!` macro. After adding packages, run `doom sync` to install them.

### Configuration Pattern
Use the `after!` macro in `config.el` to configure packages after they load. This prevents conflicts with Doom's defaults.

### Development Setup
Org files are stored in "~/Google Drive/My Drive/Org" with weekly journal entries. The configuration includes frame transparency settings and supports both GUI and terminal usage.
