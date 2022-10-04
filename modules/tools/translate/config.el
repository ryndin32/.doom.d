(use-package! google-translate
  :init
  (with-eval-after-load 'google-translate-tk
    (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))
  :config
  (set-popup-rule! "^\\*Google Translate\\*$" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)
  (setq google-translate-default-source-language "auto"
        google-translate-default-target-language "ru")
  (map! (:leader
         "v t" #'google-translate-at-point
         "v T" #'google-translate-query-translate)))
