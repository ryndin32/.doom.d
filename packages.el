;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! protobuf-mode)

(package! bazel
  :recipe (:host github :repo "bazelbuild/emacs-bazel-mode")
  :pin "e9285452389023f581404864333af8cc110ab50e")

(package! org-contrib
  :recipe (:host github :repo "emacsmirror/org-contrib")
  :pin "8c138dc5375dc13a0d1952e61c638ef4d57df368")
