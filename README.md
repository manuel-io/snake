# Snake
[![Build Status](https://travis-ci.org/blkdev/snake.svg?branch=master)](https://travis-ci.org/blkdev/snake)

## Download
    git clone https://github.com/blkdev/snake.git

## Update OS
    yum update
    yum install -y ghc-OpenGL-devel ghc-OpenGLRaw-devel
    yum install -y freeglut-devel

## Prepare
    cabal update
    cabal install cabal-install
    cabal sandbox init

## Build
    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests && cabal build && cabal test

## Run
    cabal run
