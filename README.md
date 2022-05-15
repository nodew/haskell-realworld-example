# Haskell-realworld-example

A Haskell implementation of **[realworld.io](https://realworld.io)**

## Tech stack

- **RIO** is an alternative Prelude
- **Servant** for web api implementation
- **cryptonite** for Cryptography
- **PostgreSQL** for persistence
- **Rel8** for interacting with PostgreSQL databases

## Get start

```bash
git clone https://github.com/nodew/haskell-realworld-example.git
cd haskell-realworld-example

# Run with docker
docker-compose -f ./docker-compose.yml -f up

# Otherwise, manually build
stack build

# Setup postgres connection string
POSTGRES_CONNECT_STRING="host=localhost port=5432 user=postgres password=postgres dbname=conduit connect_timeout=10"
# Setup default key for JWT
JWK_STRING=xxxxxxxxxxxxxxxx

stack exec conduit-server-exe
```

## TODO

- [x] Add integration tests
- [x] Add DB migrator
- [ ] Build full static Haskell binaries with docker or nix
- [x] Support docker compose
