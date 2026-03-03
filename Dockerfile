FROM golang:alpine AS builder
RUN apk add --update git make
WORKDIR /go/src/github.com/google/mtail
COPY . /go/src/github.com/google/mtail
RUN  make depclean && make install_deps && PREFIX=/go make STATIC=y -B install
RUN apk add -U --no-cache ca-certificates

FROM scratch
COPY --from=builder /go/bin/mtail /usr/bin/mtail
COPY --from=alpine /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/
ENTRYPOINT ["/usr/bin/mtail"]
EXPOSE 3903


ARG version=0.0.0-local
ARG build_date=unknown
ARG commit_hash=unknown
ARG vcs_url=unknown
ARG vcs_branch=unknown

LABEL org.opencontainers.image.ref.name="google/mtail" \
      org.opencontainers.image.vendor="Google" \
      org.opencontainers.image.title="mtail" \
      org.opencontainers.image.description="extract internal monitoring data from application logs for collection in a timeseries database" \
      org.opencontainers.image.authors="Jamie Wilkinson (@jaqx0r)" \
      org.opencontainers.image.licenses="Apache-2.0" \
      org.opencontainers.image.version=$version \
      org.opencontainers.image.revision=$commit_hash \
      org.opencontainers.image.source=$vcs_url \
      org.opencontainers.image.documentation="https://google.github.io/mtail/" \
      org.opencontainers.image.created=$build_date \
      org.opencontainers.image.url="https://github.com/google/mtail"
