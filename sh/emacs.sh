#!/bin/bash

sudo apt-get install build-essential autoconf automake libtool texinfo build-essential xorg-dev libgtk2.0-dev libjpeg-dev libncurses5-dev libdbus-1-dev libgif-dev libtiff-dev libm17n-dev libpng12-dev librsvg2-dev libotf-dev libxml2-dev
cd ~/src
git clone https://github.com/mirrors/emacs.git
cd emacs
./autogen.sh
./configure
make bootstrap
make
ln -s src/emacs ~/bin/emacs
