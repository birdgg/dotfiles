;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:


(setq doom-theme 'catppuccin)
(setq doom-font (font-spec :family "JetBrains Mono" :size 20))
;; (set-frame-parameter nil 'alpha-background 50) ; For current frame
;; (add-to-list 'default-frame-alist '(alpha . (50 . 80))) ; For all new frames henceforth
(set-frame-parameter nil 'alpha-background 0.7)
;; (add-to-list 'default-frame-alist '(alpha-background . 80)) ; For all new frames henceforth


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; ~/.doom.d/config.el
;;
(use-package! claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

;; (use-package! haskell-ts-mode
;;   :ensure t
;;   :custom
;;   (haskell-ts-font-lock-level 4)
;;   (haskell-ts-use-indent t)
;;   (haskell-ts-ghci "ghci")
;;   :config
;;   (add-to-list 'treesit-language-source-alist
;;                '(haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1")))
;;   (unless (treesit-grammar-available-p 'haskell)
;;     (treesit-install-language-grammar 'haskell))

;;   ;; Optional: Associate with .hs files
;;   (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-ts-mode)))

;; (use-package! lsp-haskell
;;   :defer t
;;   :init
;;   (add-hook 'haskell-mode-local-vars-hook #'lsp! 'append)
;;   (add-hook 'haskell-literate-mode-local-vars-hook #'lsp! 'append)
;;   :config
;;   ;; Does some strange indentation if it pastes in the snippet
;; (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed))
;; Haskell configs
;; (after! lsp-haskell
;; (setq lsp-haskell-formatting-provider "ormolu"))

;; Org mode
(setq org-directory "~/Google Drive/My Drive/Org")
(setq org-journal-dir "~/Google Drive/My Drive/Org/journal")
(setq org-journal-file-type 'weekly)

(after! org
  (pushnew! org-link-abbrev-alist
            '("axiom"      . "https://axiom.trade/meme/%s")))

;;;###autoload
(defun my/set-frame-opacity (opacity)
  "Interactively change the current frame's opacity.

OPACITY is an integer between 0 to 100, inclusive."
  (interactive '(interactive))
  (let* ((parameter
          'alpha-background
          )
         (opacity
          (if (eq opacity 'interactive)
              (read-number "Opacity (0-100): "
                           (or (frame-parameter nil parameter)
                               100))
            opacity)))
    (set-frame-parameter nil parameter opacity)))
