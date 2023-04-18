;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       company
       vertico

       :ui
       doom
       hl-todo
       modeline
       ophints
       (popup +defaults)
       treemacs
       vc-gutter
       vi-tilde-fringe
       (window-select +numbers)
       workspaces

       :editor
       (evil +everywhere)
       fold
       format
       multiple-cursors
       snippets

       :emacs
       electric
       ibuffer
       undo
       vc

       :term
       vterm

       :checkers
       (syntax +childframe)

       :tools
       direnv
       eval
       lookup
       (lsp +peek)
       magit
       pdf

       :os
       (:if IS-MAC macos)

       :lang
       data
       emacs-lisp
       json
       (javascript +lsp)
       markdown
       org
       (python +lsp)
       (rust +lsp)
       web
       yaml

       :config
       (default +bindings +smartparens))
