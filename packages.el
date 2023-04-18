;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! protobuf-mode)

(package! dockerfile-mode :pin "52c6c00da1d31c0b6c29c74335b3af63ed6bf06c")

(package! bazel
  :recipe (:host github :repo "bazelbuild/emacs-bazel-mode")
  :pin "e9285452389023f581404864333af8cc110ab50e")

(package! org-contrib
  :recipe (:host github :repo "emacsmirror/org-contrib")
  :pin "8c138dc5375dc13a0d1952e61c638ef4d57df368")
