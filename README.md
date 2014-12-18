## Download
    git clone https://github.com/Blkdev/snake.git

## Update OS
    yum update
    yum install -y ghc-OpenGL-devel ghc-OpenGLRaw-devel
    yum install -y freeglut-devel

## Build
    cabal update
    cabal install --only-dependencies
    make

## Run
    ./bin/snake
