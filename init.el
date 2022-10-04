;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;;chinese
       ;;japanese
       ;;layout

       :completion
       company
       ;;helm
       ;;ido
       ;;ivy
       vertico

       :ui
       ;;deft
       doom
       ;;doom-dashboard
       ;;doom-quit
       ;;(emoji +unicode)
       ;;fill-column
       hl-todo
       ;;hydra
       ;;indent-guides
       ;;ligatures
       ;;minimap
       modeline
       ;;nav-flash
       ;;neotree
       ophints
       (popup +defaults)
       ;;tabs
       (treemacs +lsp)
       ;;unicode
       vc-gutter
       vi-tilde-fringe
       (window-select +numbers)
       workspaces
       ;;zen

       :editor
       (evil +everywhere)
       ;;file-templates
       fold
       format
       ;;god
       ;;lispy
       multiple-cursors
       ;;objed
       ;;parinfer
       ;;rotate-text
       snippets
       ;;word-wrap

       :emacs
       ;;dired
       electric
       ibuffer
       undo
       vc

       :term
       ;;eshell
       ;;shell
       ;;term
       vterm

       :checkers
       (syntax +childframe)
       ;;(spell +flyspell)
       ;;grammar

       :tools
       ;;ansible
       dap
       ;;(debugger +lsp)
       direnv
       docker
       ;;editorconfig
       ;;ein
       (eval +overlay)
       ;;gist
       lookup
       (lsp +peek)
       magit
       ;;make
       pass
       pdf
       ;;prodigy
       ;;rgb
       ;;taskrunner
       ;;terraform
       ;;tmux
       ;;upload

       :os
       (:if IS-MAC macos)
       ;;tty

       :lang
       ;;agda
       ;;beancount
       ;;cc
       ;;clojure
       ;;common-lisp
       ;;coq
       ;;crystal
       ;;csharp
       data
       ;;(dart +flutter)
       ;;elixir
       ;;elm
       emacs-lisp
       ;;erlang
       ;;ess
       ;;factor
       ;;faust
       ;;fsharp
       ;;fstar
       ;;gdscript
       (go +lsp)
       ;;(haskell +dante)
       ;;hy
       ;;idris
       json
       ;;java
       (javascript +lsp)
       ;;julia
       kotlin
       ;;latex
       ;;lean
       ;;ledger
       ;;lua
       markdown
       ;;nim
       ;;nix
       ;;ocaml
       org
       ;;php
       ;;plantuml
       ;;purescript
       (python +lsp +pyright)
       ;;qt
       ;;racket
       ;;raku
       ;;rest
       ;;rst
       ;;(ruby +rails)
       ;;rust
       ;;scala
       ;;(scheme +guile)
       ;;sh
       ;;sml
       ;;solidity
       ;;swift
       ;;terra
       (web +lsp)
       yaml
       ;;zig

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere
       ;;irc
       ;;(rss +org)
       ;;twitter

       :config
       ;;literate
       (default +bindings +smartparens))
