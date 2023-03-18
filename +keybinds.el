(map!
 (:leader
  :prefix ("v" . "v")
  "p" #'consult-yank-from-kill-ring
  "m" #'toggle-frame-maximized
  "v" #'v-term
  "c" #'v-center-frame
  "t" #'v-toggle-theme
  "k" (cmd! (load! "+keybinds.el")))

 ;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Backspace-invokes-help.html
 "C-h" #'delete-backward-char
 "C-x h" #'help-command

 "C-`" nil
 :i "C-v" #'yank
 :i "C-z" #'undo

 (:when (modulep! :ui workspaces)
   :n "] TAB" #'+workspace/switch-right
   :n "[ TAB" #'+workspace/switch-left)

 (:when (modulep! :tools pdf)
   :map pdf-view-mode-map
   ;; might conflict with undo functionality
   :n "d" #'pdf-view-scroll-up-or-next-page
   :n "u" #'pdf-view-scroll-down-or-previous-page)

 (:textobj "b" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
  :textobj "B" #'evil-inner-paren #'evil-a-paren)

 :nvo "gh" #'evil-beginning-of-line
 :nvo "gl" #'evil-end-of-line
 :nvo "gH" #'evil-first-non-blank
 :nvo "gL" #'evil-last-non-blank

 (:v "s" #'evil-surround-region
  :v "S" #'evil-Surround-region
  :o "s" #'evil-surround-edit
  :o "S" #'evil-Surround-edit

  :map evil-surround-mode-map
  :v "s" #'evil-surround-region
  :v "S" #'evil-Surround-region
  :v "gS" nil)

 (:map (evil-ex-completion-map
        evil-ex-search-keymap)
       "C-v" #'yank)

 (:map vertico-map
       "C-h" #'vertico-directory-delete-char
       "C-l" #'vertico-directory-enter)

 (:map treemacs-mode-map
       "p" #'treemacs-peek
       [mouse-1] #'treemacs-single-click-expand-action
       (:when (modulep! :ui workspaces)
         ;; "gt" #'+workspace:switch-next
         ;; "gT" #'+workspace:switch-previous
         "]w" #'+workspace/switch-right
         "[w" #'+workspace/switch-left
         "] TAB" #'+workspace/switch-right
         "[ TAB" #'+workspace/switch-left))

 (:after elfeed
  :map elfeed-search-mode-map
  :n "p" #'v-elfeed-mpv
  :n "gr" #'elfeed-search-fetch
  :n "gR" #'elfeed-search-update--force)

 (:after nov
  :map nov-mode-map
  :nvo "gh" #'evil-beginning-of-visual-line
  :nvo "gl" #'evil-end-of-visual-line
  :nvo "gj" #'evil-next-line
  :nvo "gk" #'evil-previous-line
  :nvo "j" #'evil-next-visual-line
  :nvo "k" #'evil-previous-visual-line)

 (:after vterm
  :map vterm-mode-map
  :in "C-g" #'+popup/toggle
  :i  "C-h" #'vterm-send-C-h
  :i  "C-j" #'vterm-send-C-j
  :i  "C-k" #'vterm-send-C-k
  :i  "C-x" #'vterm-send-C-x
  :i  "C-v" #'yank
  :i  "C-c" (cmd! (if mark-active
                      (call-interactively #'kill-ring-save)
                    (call-interactively #'vterm-send-C-c))))

 ;; TODO check spacemacs' `auto-completion-complete-with-key-sequence'
 :i "C-;" (cmds! (not (minibufferp)) #'+company/complete)
 (:after company
         (:map company-active-map
               "C-;" #'company-show-doc-buffer
               "C-h" #'company-abort
               "C-j" #'company-select-next
               "C-k" #'company-select-previous
               "C-l" #'company-complete-selection
               "TAB" #'company-complete-selection
               [tab] #'company-complete-selection)
         ;; "RET" nil
         ;; [return] nil)
         (:map company-search-map
               "C-;" #'company-show-doc-buffer
               "C-h" #'company-search-abort))

 (:leader
  "1" #'winum-select-window-1
  "2" #'winum-select-window-2
  "3" #'winum-select-window-3
  "4" #'winum-select-window-4
  "5" #'winum-select-window-5
  "6" #'winum-select-window-6
  "7" #'winum-select-window-7
  "8" #'winum-select-window-8
  "9" #'winum-select-window-9
  "0" #'treemacs-select-window
  :desc "Restart Emacs" "q r" #'doom/restart
  :desc "Restart & restore Emacs" "q R" #'doom/restart-and-restore
  ;; :desc "Jump to bookmark" "z" #'bookmark-jump
  :desc "LSP breadcrumb on headerline" "t h" #'lsp-headerline-breadcrumb-mode
  :desc "M-x" "z" #'execute-extended-command))
