;;; tools/dap/config.el -*- lexical-binding: t; -*-

(defvar +debugger--dap-alist
  `(((:lang cc +lsp)         :after ccls        :require (dap-lldb dap-gdb-lldb))
    ((:lang elixir +lsp)     :after elixir-mode :require dap-elixir)
    ((:lang go +lsp)         :after go-mode     :require dap-go)
    ((:lang java +lsp)       :after java-mode   :require lsp-java)
    ((:lang php +lsp)        :after php-mode    :require dap-php)
    ((:lang python +lsp)     :after python      :require dap-python)
    ((:lang ruby +lsp)       :after ruby-mode   :require dap-ruby)
    ((:lang rust +lsp)       :after rustic-mode :require (dap-lldb dap-cpptools))
    ((:lang javascript +lsp)
     :after (js2-mode typescript-mode)
     :require (dap-node dap-chrome dap-firefox ,@(if IS-WINDOWS '(dap-edge)))))
  "TODO")


;;
;;; Packages

(use-package! dap-mode
  :when (featurep! :tools lsp)
  :hook (dap-mode . dap-tooltip-mode)
  :init
  (setq dap-breakpoints-file (concat doom-etc-dir "dap-breakpoints")
        dap-utils-extension-path (concat doom-etc-dir "dap-extension/"))
  (after! lsp-mode (require 'dap-mode))
  (map! :leader
        :prefix ("d" . "debug")
        "." #'dap-hydra
        "'" #'dap-ui-repl
        "c"  #'dap-continue
        "i"  #'dap-step-in
        "o"  #'dap-step-out
        "r"  #'dap-restart-frame
        "s"  #'dap-next
        "v"  #'dap-ui-inspect-thing-at-point
        (:prefix ("a" . "abandon")
         "a"  #'dap-disconnect
         "A"  #'dap-delete-all-sessions)
        (:prefix ("b" . "breakpoint")
         "b" #'dap-breakpoint-toggle
         "c" #'dap-breakpoint-condition
         "l" #'dap-breakpoint-log-message
         "h" #'dap-breakpoint-hit-condition
         "a" #'dap-breakpoint-add
         "d" #'dap-breakpoint-delete
         "D" #'dap-breakpoint-delete-all)
        (:prefix ("d" . "debug")
         "d" #'dap-debug
         "e" #'dap-debug-edit-template
         "l" #'dap-debug-last
         "r" #'dap-debug-recent)
        (:prefix ("e" . "eval")
         "e" #'dap-eval
         "r" #'dap-eval-region
         "t" #'dap-eval-thing-at-point
         "t" #'dap-ui-expressions-add)
        (:prefix ("I" . "inspect")
         "i" #'dap-ui-inspect
         "r" #'dap-ui-inspect-region
         "t" #'dap-ui-inspect-thing-at-point)
        (:prefix ("S" . "switch")
         "s" #'dap-switch-session
         "t" #'dap-switch-thread
         "f" #'dap-switch-frame)
        (:prefix ("w" . "window")
         "o" #'dap-go-to-output-buffer
         "l" #'dap-ui-locals
         "s" #'dap-ui-sessions
         "b" #'dap-ui-breakpoints))
  :config
  (pcase-dolist (`((,category . ,modules) :after ,after :require ,libs)
                 +debugger--dap-alist)
    (when (doom-module-p category (car modules) (cadr modules))
      (dolist (lib (doom-enlist after))
        (with-eval-after-load lib
          (mapc #'require (doom-enlist libs))))))

  (dap-mode 1)

  (when (featurep! :lang python)
    (setq dap-python-debugger 'debugpy)
    (advice-add #'dap-python--pyenv-executable-find
                :override
                (lambda (command) (executable-find command)))
    (dap-register-debug-template "Python :: Attach"
                                 (list :type "python"
                                       :request "attach"
                                       :port 5678
                                       :host "127.0.0.1"))
    (dap-register-debug-template "Python :: Django"
                                 (list :type "python"
                                       :request "launch"
                                       :console "integratedTerminal"
                                       :cwd "${workspaceFolder}"
                                       :program "${workspaceFolder}/manage.py"
                                       :args "runserver 8088 --noreload"
                                       :django t))
    (dap-register-debug-template "Python :: FastAPI"
                                 (list :type "python"
                                       :request "launch"
                                       :console "integratedTerminal"
                                       :cwd "${workspaceFolder}"
                                       :module "uvicorn"
                                       :program "app.main:app"
                                       :args "--port 8088"
                                       :jinja: t))))


(use-package! dap-ui
  :when (featurep! :tools lsp)
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode)
  :config
  (setq dap-auto-configure-features nil)
  (remove-hook 'dap-ui-mode-hook #'dap-ui-controls-mode))
