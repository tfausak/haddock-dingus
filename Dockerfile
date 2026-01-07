FROM benz0li/ghc-musl:9.14-int-native
WORKDIR /work
COPY cabal.project haddock-dingus.cabal Main.hs /work
RUN \
  set -o errexit -o xtrace; \
  cabal update; \
  cabal build --enable-executable-static; \
  cp $( cabal list-bin haddock-dingus ) .

FROM scratch
EXPOSE 3000
COPY --from=0 /work/haddock-dingus /
CMD ["/haddock-dingus"]
