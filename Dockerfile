# Used to make static binary for linux
# Based on: https://gist.github.com/rlefevre/1523f47e75310e28eee243c9c5651ac9

FROM alpine:3.10

# Install required packages
RUN apk add --update ghc cabal git musl-dev zlib-dev ncurses-dev ncurses-static wget

# Copy the necessary code
WORKDIR /tmp
COPY bin ./bin
COPY src ./src
COPY elm-proofread.cabal .

# Build a statically linked binary
RUN cabal new-update
RUN cabal new-configure --disable-executable-dynamic --ghc-option=-optl=-static --ghc-option=-optl=-pthread
RUN cabal new-build
RUN strip -s ./dist-newstyle/build/x86_64-linux/ghc-8.4.3/elm-proofread-0.1.0/x/elm-proofread/build/elm-proofread/elm-proofread
