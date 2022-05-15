# builder
FROM fpco/stack-build-small:lts-19.4 as base

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update
RUN apt install libpq-dev -y

WORKDIR /app

COPY stack.yaml .
COPY package.yaml .

RUN stack build --only-dependencies

FROM base as builder

WORKDIR /app

COPY . .

RUN stack --local-bin-path output install

# runtime container
FROM ubuntu:21.10

RUN apt update

RUN apt install libpq-dev libncurses5 -y

ENV POSTGRES_CONNECT_STRING=host=localhost port=5432 user=postgres password=postgres dbname=conduit connect_timeout=10
ENV POSTGRES_POOL_SIZE=1
ENV JWK_STRING=

WORKDIR /root/

COPY --from=builder /app/output ./

EXPOSE 8080

CMD [ "./conduit-server-exe" ]
