;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq doom-theme 'catppuccin)
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 20)
      doom-symbol-font (font-spec :family "JetBrainsMono Nerd Font" :size 20))
(set-frame-parameter nil 'alpha-background 0.5)
(add-hook 'window-setup-hook #'toggle-frame-maximized)
(add-to-list 'default-frame-alist '(undecorated-round . t))

(setq display-line-numbers-type t)

(use-package! claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-window-width 70))

(use-package! verb
  :after org
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(after! lsp-mode
  (setq lsp-lens-place-position 'above-line))

;; Haskell
(after! lsp-haskell
  ;;https://github.com/emacs-lsp/lsp-mode/issues/4473
  (setq lsp-rename-use-prepare nil))


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


(set-font-ligatures! 'prog-mode :append ":<|>")
