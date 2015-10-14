#!/bin/bash

cd ~/Distributives
wget http://sourceforge.net/projects/ditaa/files/latest/download -O ditaa.zip
unzip -p ditaa.zip ditaa0_9.jar > ~/bin/ditaa.jar
chmod +x ~/bin/ditaa.jar
