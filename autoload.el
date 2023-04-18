;;;###autoload
(defun v-center-frame (&optional width)
  nil
  (interactive "P")
  (set-frame-size nil (or width 180) 100)
  (set-frame-position nil (/ (- (display-pixel-width) (frame-native-width)) 2) 0))

;;;###autoload
(defun v-term ()
  nil
  (interactive)
  (evil-window-split)
  (+vterm/here nil)
  (tear-off-window nil))
