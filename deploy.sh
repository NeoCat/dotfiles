#!/bin/bash
cd $HOME
ln -s .dotfiles/.{emacs,elisp,tmux.conf,zshrc} .
# requires netifaces and psutil installation with pip
mkdir -p ~/.config/powerline/themes/tmux
ln -s $HOME/.dotfiles/powerline-themes-tmux-default.json ~/.config/powerline/themes/tmux/default.json
cp -i .dotfiles/gitconfig_default .gitconfig
