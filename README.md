# dotfiles

macOS dotfiles with a Haskell CLI (`dot`) for idempotent setup.

## Install

Requires [GHCup](https://www.haskell.org/ghcup/) (GHC + Cabal).

```bash
git clone https://github.com/birdgg/dotfiles.git ~/dotfiles
cd ~/dotfiles
cabal run dot -- install
```

## Usage

```bash
cabal run dot -- install          # Interactive TUI - select tools to install
cabal run dot -- install ghostty  # Install a specific tool
cabal run dot -- list             # Show all tools and their install status
```

## Tools

| Tool | Type | Stow |
|------|------|------|
| Homebrew | CLI | - |
| Zsh | Shell | zsh |
| GPG | CLI | gnupg |
| Tmux | CLI | tmux |
| Ghostty | App | ghostty |
| Bun | CLI | - |
| Rust | CLI | - |
| Haskell | CLI | - |
| Claude | CLI | - |
| Codex | CLI | - |
| 1Password | App | - |
| VS Code | App | - |
| Zed | App | - |
| OrbStack | App | - |
| Chrome | App | - |
| Telegram | App | - |
| Discord | App | - |
| Infuse | App | - |
| ClashX Meta | App | - |

## Structure

```
dotfiles/
├── app/              # Haskell CLI source
│   ├── Main.hs       # CLI entry point (optparse-applicative)
│   ├── Tool.hs       # Tool definitions and install logic
│   └── TUI.hs        # Interactive tool selector
├── ghostty/          # Ghostty terminal config (stow)
├── gnupg/            # GPG config + public key (stow)
├── tmux/             # Tmux config (stow)
├── zsh/              # Zsh config (stow)
└── dotfiles.cabal    # Build config
```

## License

MIT
