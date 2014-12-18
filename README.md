# Snake
[![Build Status](https://travis-ci.org/blkdev/snake.svg?branch=master)](https://travis-ci.org/blkdev/snake)

## Download
    git clone https://github.com/blkdev/snake.git

## Update OS
    yum update
    yum install -y ghc-OpenGL-devel ghc-OpenGLRaw-devel
    yum install -y freeglut-devel

## Build
    cabal update
    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests && cabal build && cabal test
    make

## Run
    ./bin/snake
    ./dist/build/snake/snake
