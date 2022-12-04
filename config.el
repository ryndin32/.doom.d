;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+keybinds.el")

(setq user-full-name "Your Name"
      user-mail-address "you@example.com")

(after! projectile
  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-project-search-path '("~/Documents/Projects" "~/Documents/Projects/horatius/services"))
  (pushnew! projectile-project-root-files ".envrc" "pyproject.toml")
  (pushnew! projectile-globally-ignored-directories
            ".vscode"
            ".idea"
            ".envrc"
            "*__pycache__"
            "*.mypy_cache"
            "*.pytest_cache"
            "*.venv"
            "*node_modules"))



;;; UI

(setq doom-font (font-spec :family "Monaco" :size 14))
(setq doom-variable-pitch-font (font-spec :family "Helvetica" :size 14))
(setq doom-theme 'doom-one-light)
(setq display-line-numbers-type t)
(add-hook! 'doom-init-ui-hook #'v-center-frame)



;;; Keybinds

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Input-Method.html
(setq default-input-method "russian-computer")

(setq mac-command-modifier      'control
      mac-option-modifier       'meta
      mac-right-option-modifier 'left)
(setq x-meta-keysym             'ctrl
      x-super-keysym            'meta)


;;
;;; Modules

;; evil
(setq evil-want-change-word-to-end nil
      evil-ex-substitute-global t
      evil-split-window-below t
      evil-vsplit-window-right t
      +evil-want-o/O-to-continue-comments nil)

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(setq evil-snipe-repeat-keys nil)

(after! evil-escape
  (setq evil-escape-key-sequence "fd"
        evil-escape-excluded-states nil
        evil-escape-excluded-major-modes nil
        evil-escape-inhibit-functions nil))

;; company
(set-company-backend! 'text-mode
  (delq 'company-ispell
        (car (alist-get 'text-mode +company-backend-alist))))

;; lsp
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(setq lsp-enable-suggest-server-download nil
      lsp-enable-symbol-highlighting nil
      lsp-lens-enable nil
      lsp-modeline-code-actions-enable nil
      lsp-modeline-diagnostics-enable nil
      lsp-signature-auto-activate nil
      lsp-signature-render-documentation nil
      lsp-ui-doc-show-with-cursor nil
      lsp-ui-sideline-enable nil)
(add-hook! 'lsp-after-initialize-hook
  (run-hooks (intern (format "%s-lsp-hook" major-mode))))

;; web
(setq emmet-self-closing-tag-style " /")
(add-hook 'web-mode-hook (lambda ()
                           (setq web-mode-markup-indent-offset 2)
                           (setq web-mode-css-indent-offset 2)
                           (setq web-mode-code-indent-offset 2)
                           (setq web-mode-style-padding 0)
                           (setq web-mode-script-padding 0)))
(add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2)))
(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))
(setq typescript-indent-level 2)

;; org
(setq org-directory "~/Documents/Org/")
(after! org-capture
  (dolist (elt '(("m" "Music" item
                  (file+headline "music.org" "Inbox")
                  nil :prepend t)))
    (push elt org-capture-templates)))

;; flycheck
(after! flycheck
  (delq! 'idle-change flycheck-check-syntax-automatically))

;; format
(setq +format-with-lsp nil)

;; winum
(setq winum-scope 'frame-local)
(after! which-key
  (setq which-key-idle-delay 0.4)
  (map! :leader :prefix "w" "0" nil)
  (push '(("\\(.*\\)1" . "winum-select-window-1") .
          ("\\11..9" . "select window 1..9"))
        which-key-replacement-alist)
  (push '((nil . "winum-select-window-[2-9]") . t)
        which-key-replacement-alist)
  (push '(("override-state" . nil) . t)
        which-key-replacement-alist))

;; treemacs
(require 'ace-window)
(setq treemacs-wrap-around nil
      +treemacs-git-mode 'extended)
(after! treemacs
  (defun treemacs-custom-filter (file _)
    (or (string= file ".vscode")
        (string= file ".idea")
        (string= file ".envrc")
        (string= file "__pycache__")
        (string= file ".mypy_cache")
        (string= file ".pytest_cache")
        (string= file ".venv")
        (string= file "node_modules")))
  (push #'treemacs-custom-filter treemacs-ignored-file-predicates))

;; term
(setq vterm-shell (cond (IS-LINUX "/usr/bin/fish")
                        (IS-MAC   "/opt/homebrew/bin/fish")))

;; pdf
(after! pdf-tools
  (add-hook! pdf-view-mode #'pdf-view-midnight-minor-mode))

;; rss
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)
(after! elfeed
  (setq elfeed-search-filter "@1-week-ago +youtube "
        elfeed-initial-tags nil
        elfeed-goodies/powerline-default-separator nil))

;; bazel/starlark
(use-package! bazel
  :mode ("/Tiltfile" . bazel-starlark-mode))

;; other
(setq dired-kill-when-opening-new-dired-buffer t)
(setq +workspaces-on-switch-project-behavior t)
