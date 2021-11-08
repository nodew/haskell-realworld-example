FROM utdemir/ghc-musl:v22-ghc901

RUN apk update

RUN apk add libpq

RUN ghcup install stack

WORKDIR /app

COPY . .

RUN stack build --system-ghc --ghc-options ' -static -optl-static -optl-pthread -fPIC' --no-nix --stack-yaml ./stack-9.0.1.yaml


