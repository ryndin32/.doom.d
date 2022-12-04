;;;###autoload
(defun v-center-frame (&optional width)
  nil
  (interactive "P")
  (set-frame-size nil (or width 180) 100)
  (set-frame-position nil (/ (- (display-pixel-width) (frame-native-width)) 2) 0))

;;;###autoload
(defun v-toggle-theme ()
  nil
  (interactive)
  (if (eq doom-theme 'doom-one)
      (progn (load-theme 'doom-one-light :no-confirm)
             (shell-command "gsettings set org.gnome.desktop.interface color-scheme 'default'")
             (shell-command "gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita'"))
      (progn (load-theme 'doom-one :no-confirm)
             (shell-command "gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'")
             (shell-command "gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark'"))))

;;;###autoload
(defun v-elfeed-mpv ()
  nil
  (interactive)
  (setq entry (elfeed-search-selected :single))
  (setq entry-link (elfeed-entry-link entry))
  (elfeed-untag-1 entry 'unread)
  (elfeed-search-update-entry entry)
  (call-process "mpv" nil 0 nil
                "--no-terminal"
                "--script-opts=ytdl_hook-ytdl_path=yt-dlp"
                (format
                 "--ytdl-format=bestvideo[height<=?%1$s]+bestaudio/best[height<=?%1$s]"
                 (display-pixel-height))
                "--"
                entry-link))

;;;###autoload
(defun v-term ()
  nil
  (interactive)
  (evil-window-split)
  (+vterm/here nil)
  (tear-off-window nil))
