FROM fpco/haskell-scratch:integer-gmp
COPY .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/smhi-scraper/smhi-scraper .
ENTRYPOINT ./smhi-scraper
