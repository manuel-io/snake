FROM ubuntu

RUN apt-get update

RUN apt-get install -y ghc \
                       libghc-opengl-dev \
                       libghc-openglraw-dev \
                       freeglut3-dev

RUN apt-get install -y make \
                       zlib1g-dev \
                       cabal-install

ADD LICENSE /snake/LICENSE
ADD README.md /snake/README.md
ADD Makefile /snake/Makefile
ADD Setup.hs /snake/Setup.hs
ADD Main.hs /snake/Main.hs
ADD snake.cabal /snake/snake.cabal
ADD spec/ /snake/spec/

WORKDIR /snake/
ENV PATH /root/.cabal/bin:$PATH

RUN cabal update
RUN cabal install cabal-install
RUN cabal sandbox init

RUN make build
