;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; LSP
(package! lsp-pyright :pin "c745228f39fdb35372b29b909f25fa6c98dc7c88")

;; Testing frameworks
(package! python-pytest :pin "9bf8db38bf18feb0484931877210cecfaa96bfc6")

;; Import managements
(package! pyimport :pin "a6f63cf7ed93f0c0f7c207e6595813966f8852b9")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
