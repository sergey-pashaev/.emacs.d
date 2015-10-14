#!/bin/bash

# debian
# sudo apt-get install silversearcher-ag

# src
sudo apt-get install -y automake pkg-config libpcre3-dev zlib1g-dev liblzma-dev
cd ~/src
git clone https://github.com/ggreer/the_silver_searcher.git
cd the_silver_searcher
./build.sh
cd ~/bin
ln -s ~/src/the_silver_searcher/ag ag
