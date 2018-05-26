# Snake
![Build Status](https://travis-ci.org/manuel-io/snake.svg?branch=master)

---

## Installation and usage

### Download
    git clone https://github.com/manuel-io/snake.git

### Update OS
    yum update
    yum install -y ghc-OpenGL-devel ghc-OpenGLRaw-devel
    yum install -y freeglut-devel

### Prepare
    cabal update
    cabal install cabal-install
    cabal sandbox init

### Build
    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests
    cabal build && cabal test
    cabal install

### Run
    cabal run
