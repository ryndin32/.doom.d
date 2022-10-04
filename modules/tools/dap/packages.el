;; -*- no-byte-compile: t; -*-
;;; tools/dap/packages.el

(when (featurep! :tools lsp)
  (package! dap-mode :pin "5d5043f962de030cadf761613199e0251c602d1e")
  (package! posframe :pin "0d23bc5f7cfac00277d83ae7ba52c48685bcbc68"))
