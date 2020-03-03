FROM haskell:8.6.5

WORKDIR /usr/src/tg-haskell
COPY . .

RUN cabal new-update
RUN cabal new-install .

CMD ["tg-haskell"]