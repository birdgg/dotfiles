# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Full installation
source install.sh

# Symlink single config
stow <dir>              # e.g., stow zsh
stow <dir> --simulate   # dry run

# Doom Emacs (run after modifying init.el/packages.el)
doom sync
doom doctor
doom build

# Script validation
bash -n script.sh
```

## Architecture

This is a macOS dotfiles repository using **GNU Stow** for symlink management.

**Config directories** (each stows to `$HOME`):
- `zsh/` - Shell config (.zshrc, .zshenv, .zsh_aliases)
- `ghostty/` - Terminal config (.config/ghostty/)
- `doom-emacs/` - Editor config (.doom.d/)

**Bootstrap scripts**:
- `install.sh` - Main entry point, calls brew.sh and symlinks.sh
- `symlinks.sh` - Auto-stows all top-level directories
- `brew.sh` - Homebrew package provisioning
- `node.sh` - Node.js LTS via n

All scripts are idempotent. Add new tool configs as top-level directories and they'll be picked up automatically.

## Key Files

| Task | File |
|------|------|
| Add Zsh alias | `zsh/.zsh_aliases` |
| Modify env vars | `zsh/.zshenv` |
| Add Emacs package | `doom-emacs/.doom.d/packages.el` |
| Configure Emacs | `doom-emacs/.doom.d/config.el` |
| Enable Emacs module | `doom-emacs/.doom.d/init.el` |

## Style

- **Shell**: Bash, 4-space indent, dashed-lowercase filenames
- **Emacs Lisp**: 2-space indent, use `after!` blocks
- **Commits**: `feat:`, `fix:`, `chore:` + brief description

## Theme

All configs use **Catppuccin Mocha** color scheme.
