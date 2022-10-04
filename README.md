# Install
``` sh
git clone https://github.com/ryndin32/.doom.d ~/.config/doom
git clone --depth 1 --single-branch https://github.com/doomemacs/doomemacs ~/.config/emacs
cd ~/.config/emacs
git fetch --depth=1 origin $(cat ~/.config/doom/revision.txt)
git checkout $(cat ~/.config/doom/revision.txt)
./bin/doom install
```
