FROM node:lts-slim as builder

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update

RUN apt install git -y

WORKDIR /app

RUN git clone --depth=1 https://github.com/khaledosman/react-redux-realworld-example-app.git .
RUN sed -i "s#https://conduit.productionready.io/api#/api#" ./src/agent.js

RUN yarn install

RUN yarn build

FROM nginx:1.21.6-alpine

ARG UPSTREAM="http://localhost:8080"

COPY --from=builder /app/build /srv
COPY nginx.conf /etc/nginx/conf.d/default.conf
RUN sed -i "s#<upstream>#$UPSTREAM#" /etc/nginx/conf.d/default.conf

EXPOSE 80
