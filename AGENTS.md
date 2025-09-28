# Repository Guidelines

## Project Structure & Module Organization
- Root scripts orchestrate bootstrap: `install.sh` calls `brew.sh` and `symlinks.sh`. Keep them idempotent so reruns are safe.
- Config folders mirror dotfile targets; `zsh/` and `starship/.config/` sync into `$HOME` via GNU Stow.
- Doom Emacs lives in `doom-emacs/.doom.d/` with `init.el`, `config.el`, and `packages.el`; use `modules/` for bespoke extensions.
- Add new tool configs as top-level directories so `symlinks.sh` automatically picks them up.

## Setup, Build & Sync Commands
- `source install.sh` — install Homebrew packages, then restow every config directory.
- `stow <dir>` — relink a single config (`stow zsh`, `stow starship`) without touching others.
- `bash brew.sh` — re-run package provisioning when Homebrew catalog changes.
- `bash node.sh` — install and activate the latest LTS Node via `n`.
- Doom Emacs: `$HOME/.emacs.d/bin/doom sync` after altering `init.el` or `packages.el`; follow with `doom doctor` when debugging load errors.

## Coding Style & Naming Conventions
- Shell scripts target `bash`, use four-space indents inside loops/functions, and dashed lowercase filenames. Validate with `bash -n script.sh` or `shellcheck` when available.
- Emacs Lisp adheres to Doom defaults: two-space indent, modules ordered by category in `init.el`, and configuration in `config.el` guarded by `after!` blocks.
- Zsh aliases belong in `zsh/.zsh_aliases`; custom themes live under `zsh/zsh_custom/themes/`.

## Testing & Verification
- After shell edits, execute the script in a subshell (`bash brew.sh`) to confirm it completes without errors.
- For symlink changes, dry-run with `stow <dir> --simulate` before executing real updates.
- Run `doom sync` then `doom doctor` to ensure Emacs packages resolve; add `doom build` if you touch native modules.

## Commit & Pull Request Guidelines
- Match the repository history by using conventional commits (`feat:`, `fix:`, `chore:`) with a brief scope (`feat: doom emacs config`).
- Keep commits self-contained and ensure `install.sh` still bootstraps successfully before pushing.
- PRs should enumerate touched areas (`zsh`, `doom-emacs`, etc.) and document manual verification steps; attach screenshots for prompt/theme tweaks when relevant.
