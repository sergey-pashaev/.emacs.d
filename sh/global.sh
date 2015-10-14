#!/bin/bash

cd ~/src
wget http://tamacom.com/global/global-6.5.1.tar.gz
tar -zxvf global-6.5.1.tar.gz
cd ./global-6.5.1
./configure
make

cd ~/bin
ln -s ~/src/global-6.5.1/global/global global
ln -s ~/src/global-6.5.1/globash/globash globash
ln -s ~/src/global-6.5.1/gtags/gtags gtags
ln -s ~/src/global-6.5.1/gozilla/gozilla gozilla

# treat .h files as source files by global
echo 'export GTAGSFORCECPP=""' >> ~/.bashrc
