# builder
FROM fpco/stack-build-small:lts-20.21 as base

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update
RUN apt-get install libpq-dev -y

WORKDIR /app

COPY stack.yaml .
COPY package.yaml .

RUN stack build --only-dependencies

FROM base as builder

WORKDIR /app

COPY . .

RUN stack --local-bin-path output install

# runtime container
FROM ubuntu:22.04 as runtime

RUN apt-get update

RUN apt-get install libpq-dev libncurses5 -y

ENV POSTGRES_CONNECT_STRING=host=localhost port=5432 user=postgres password=postgres dbname=conduit connect_timeout=10
ENV POSTGRES_POOL_SIZE=1
ENV JWK_STRING=

WORKDIR /root/

COPY --from=builder /app/output ./

EXPOSE 8080

CMD [ "./conduit-server-exe" ]
