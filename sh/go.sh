#!/bin/bash

cd ~/Distributives
wget https://storage.googleapis.com/golang/go1.7.3.linux-amd64.tar.gz
tar -C /usr/local -xzf go1.7.3.linux-amd64.tar.gz
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc

# go workspace
mkdir -p ~/workspace/go/{bin,src,pkg}
echo 'export GOPATH=~/workspace/go' >> ~/.bashrc
echo 'export PATH=$PATH:$GOPATH/bin' >> ~/.bashrc

source ~/.bashrc

go get -u github.com/nsf/gocode
go get -u github.com/rogpeppe/godef
go get -u golang.org/x/tools/cmd/goimports
go get -u golang.org/x/tools/cmd/oracle
go get -u github.com/golang/lint/golint
