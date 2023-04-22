;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Your Name"
      user-mail-address "you@example.com")

(after! projectile
  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-project-search-path '("~/Documents/Projects" "~/Exercism" "~/Documents/Projects/horatius/services")))


;;
;;; UI

(setq doom-theme 'doom-one-light
      doom-font (font-spec :family "JetBrains Mono NL" :size 14)
      doom-unicode-font (font-spec :family "JetBrains Mono NL")
      doom-variable-pitch-font (font-spec :family "Helvetica" :size 14))

(setq scroll-margin 5)
(add-hook! 'doom-init-ui-hook #'v-center-frame)


;;
;;; Keybinds

(load! "+keybinds.el")

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Input-Method.html
(setq default-input-method "russian-computer")

(setq mac-command-modifier      'control
      mac-option-modifier       'meta
      mac-right-option-modifier 'left)
(setq x-meta-keysym             'ctrl
      x-super-keysym            'meta)


;;
;;; Modules

;;; :checkers syntax
(after! flycheck
  (delq! 'idle-change flycheck-check-syntax-automatically))


;;; :completion company
(set-company-backend! 'text-mode
  (delq 'company-ispell
        (car (alist-get 'text-mode +company-backend-alist))))


;;; :editor evil
(setq evil-want-change-word-to-end nil
      evil-kill-on-visual-paste nil
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


;;; :editor format
(setq +format-with-lsp nil)
(set-popup-rule! "^\\*format-all-errors" :select nil :quit 'other)


;;; :emacs dired
(setq dired-kill-when-opening-new-dired-buffer t)


;;; :lang javascript
(setq typescript-indent-level 2)
(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))
(after! projectile
  (pushnew! projectile-globally-ignored-directories "*node_modules"))


;;; :lang org
(setq org-directory "~/Documents/Org/")


;;; :lang rust
(after! rustic
  (set-face-attribute 'rust-string-interpolation nil :slant 'normal)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"))
(set-popup-rule! "^\\*cargo-run" :select nil :quit 'other)
(after! projectile
  (pushnew! projectile-globally-ignored-directories "target"))


;;; :lang web
(setq emmet-self-closing-tag-style " /")
(add-hook 'web-mode-hook (lambda ()
                           (setq web-mode-markup-indent-offset 2)
                           (setq web-mode-css-indent-offset 2)
                           (setq web-mode-code-indent-offset 2)
                           (setq web-mode-style-padding 0)
                           (setq web-mode-script-padding 0)))
(add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2)))


;;; :tools direnv
(after! projectile
  (pushnew! projectile-project-root-files ".envrc")
  (pushnew! projectile-globally-ignored-directories ".envrc"))


;;; :tools lsp
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(setq lsp-enable-suggest-server-download nil
      lsp-enable-symbol-highlighting nil
      lsp-lens-enable nil
      lsp-modeline-code-actions-enable nil
      lsp-modeline-diagnostics-enable nil
      lsp-signature-auto-activate nil
      lsp-signature-render-documentation nil
      lsp-ui-doc-show-with-cursor nil)
(add-hook! 'lsp-after-initialize-hook
  (run-hooks (intern (format "%s-lsp-hook" major-mode))))
;; remove after switching to emacs 29
;; https://github.com/typescript-language-server/typescript-language-server/issues/559#issuecomment-1259470791
(advice-add 'json-parse-string :around
            (lambda (orig string &rest rest)
              (apply orig (s-replace "\\u0000" "" string)
                     rest)))
(advice-add 'json-parse-buffer :around
            (lambda (oldfn &rest args)
              (save-excursion
                (while (search-forward "\\u0000" nil t)
                  (replace-match "" nil t)))
              (apply oldfn args)))


;;; :tools pdf
(after! pdf-tools
  (unless (string-match-p "light" (symbol-name doom-theme))
    (add-hook! pdf-view-mode #'pdf-view-midnight-minor-mode)))


;;; :tools term
(setq vterm-shell (or (executable-find "fish") "/bin/sh"))


;;; :ui treemacs
(require 'ace-window)
(setq treemacs-wrap-around nil
      +treemacs-git-mode 'extended)
(after! treemacs
  ;; (setq doom-themes-treemacs-theme "doom-colors")
  (defun treemacs-custom-filter (file _)
    (or (string= file ".envrc")
        (string= file ".idea")
        (string= file ".mypy_cache")
        (string= file ".pytest_cache")
        (string= file ".ruff_cache")
        (string= file ".venv")
        (string= file ".vscode")
        (string= file "__pycache__")
        (string= file "node_modules")
        (string= file "target")))
  (push #'treemacs-custom-filter treemacs-ignored-file-predicates))
;; https://github.com/emacs-lsp/lsp-treemacs/issues/89#issuecomment-779976219
(after! lsp-treemacs
  (load-library "doom-themes-ext-treemacs"))


;;; :ui window-select
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


;;; :ui workspaces
(setq +workspaces-on-switch-project-behavior t)


;;; other
(use-package! bazel :mode ("/Tiltfile" . bazel-starlark-mode))
