FROM golang:1.9-alpine

WORKDIR /go/src/github.com/google/mtail
COPY . /go/src/github.com/google/mtail

RUN apk add --update --no-cache --virtual build-dependencies git make \
      && export GOPATH=/go \
      && make \
      && apk del build-dependencies

ENTRYPOINT ["mtail"]
