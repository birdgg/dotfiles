# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

macOS dotfiles repo with a Haskell CLI tool (`dot`) for idempotent machine setup. The CLI installs developer tools and symlinks config files via GNU Stow.

## Build & Run

```bash
cabal build              # Build the dot executable
cabal run dot -- install # Run interactive TUI installer
cabal run dot -- list    # Show tool install status
```

GHC version: 9.14.1, language standard: GHC2024. The cabal file enables `-Wall`.

CI builds a macOS ARM64 binary via GitHub Actions and commits it back to the repo.

## Architecture

Three modules in `app/`:

- **Main.hs** — CLI entry point using `optparse-applicative`. Parses `install [TOOL]` and `list` subcommands. Handles GNU Stow integration (ensures stow is installed, then symlinks config dirs to `$HOME`).
- **Tool.hs** — `ToolName` enum (Bounded/Enum) defines all managed tools. `toolFor` maps each name to a `Tool` record containing: display name, check command (`IO Bool`), install action (`IO ExitCode`), and optional stow package name. `allToolNames` derives the full list from `[minBound .. maxBound]`.
- **TUI.hs** — Terminal UI for interactive tool selection. Raw ANSI escape codes, no TUI library. Supports vim keys (j/k), arrow keys, space to toggle, 'a' for all, enter to confirm.

## Adding a New Tool

1. Add constructor to `ToolName` in `Tool.hs` (position in enum = display order)
2. Add `toolNameStr` case for the CLI name
3. Add `toolFor` case with check/install logic and optional stow package
4. If the tool has config files: create a top-level directory (e.g., `ghostty/`) mirroring the home directory structure for stow

## Stow Convention

Config directories at repo root mirror `$HOME`. Example: `ghostty/.config/ghostty/config` stows to `~/.config/ghostty/config`. Stow runs with `--adopt` flag.

## Dependencies

Only base libraries: `base`, `optparse-applicative`, `process`, `directory`. No external TUI framework.
