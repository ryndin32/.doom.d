;;; lang/python/config.el -*- lexical-binding: t; -*-

(defvar +python-ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info")
  "Command to initialize the ipython REPL for `+python/open-ipython-repl'.")

(defvar +python-jupyter-command '("jupyter" "console" "--simple-prompt")
  "Command to initialize the jupyter REPL for `+python/open-jupyter-repl'.")

(after! projectile
  (pushnew! projectile-project-root-files "pyproject.toml" "setup.py" "requirements.txt")
  (pushnew! projectile-globally-ignored-directories
            "*.mypy_cache"
            "*.pytest_cache"
            "*.ruff_cache"
            "*.venv"
            "*__pycache__"))


;;
;;; Packages

(use-package! python
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil)
  (when (modulep! +lsp) (add-hook 'python-mode-local-vars-hook #'lsp!))
  :config
  (set-repl-handler! 'python-mode #'+python/open-repl :persist t)
  (set-docsets! '(python-mode inferior-python-mode) "Python 3" "NumPy" "SciPy" "Pandas")

  (set-ligatures! 'python-mode
                  ;; Functional
                  :def "def"
                  :lambda "lambda"
                  ;; Types
                  :null "None"
                  :true "True" :false "False"
                  :int "int" :str "str"
                  :float "float"
                  :bool "bool"
                  :tuple "tuple"
                  ;; Flow
                  :not "not"
                  :in "in" :not-in "not in"
                  :and "and" :or "or"
                  :for "for"
                  :return "return" :yield "yield")

  ;; Stop the spam!
  (setq python-indent-guess-indent-offset-verbose nil)

  (add-hook! 'python-mode-hook
    (defun +python-use-correct-flycheck-executables-h ()
      "Use the correct Python executables for Flycheck."
      (let ((executable python-shell-interpreter))
        (save-excursion
          (goto-char (point-min))
          (save-match-data
            (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                      (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
              (setq executable (substring-no-properties (match-string 1))))))
        ;; Try to compile using the appropriate version of Python for
        ;; the file.
        (setq-local flycheck-python-pycompile-executable executable)
        ;; We might be running inside a virtualenv, in which case the
        ;; modules won't be available. But calling the executables
        ;; directly will work.
        (setq-local flycheck-python-pylint-executable "pylint")
        (setq-local flycheck-python-flake8-executable "flake8"))))

  (setq-hook! 'python-mode-hook tab-width python-indent-offset)

  ;; Personal
  (setq python-indent-def-block-scale 1)
  (add-hook! 'python-mode-lsp-hook
    (setq-default flycheck-checker nil)
    ;; (flycheck-select-checker 'python-flake8)
    ;; (flycheck-add-next-checker 'lsp 'python-flake8)
    (setq lsp-diagnostics-provider :none)
    (setq-default flycheck-disabled-checkers
                  (pushnew! flycheck-disabled-checkers
                            'lsp
                            'python-mypy
                            'python-pycompile)))

  (flycheck-define-checker python-ruff
    nil
    :command ("ruff"
              "--format=text"
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-ruff))


(use-package! pyimport
  :defer t
  :init
  (map! :after python
        :map python-mode-map
        :localleader
        (:prefix ("i" . "imports")
         :desc "Insert missing imports" "i" #'pyimport-insert-missing
         :desc "Remove unused imports"  "r" #'pyimport-remove-unused
         :desc "Optimize imports"       "o" #'+python/optimize-imports)))


(use-package! py-isort
  :defer t
  :init
  (map! :after python
        :map python-mode-map
        :localleader
        (:prefix ("i" . "imports")
         :desc "Sort imports"      "s" #'py-isort-buffer
         :desc "Sort region"       "S" #'py-isort-region)))


;;
;;; LSP

(use-package! lsp-pyright
  :after lsp-mode
  :init
  (setq lsp-pyright-multi-root nil
        lsp-pyright-typechecking-mode "off"))
