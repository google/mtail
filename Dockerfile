FROM golang:1.12.3-alpine3.9 AS builder
RUN apk add --update git make
WORKDIR /go/src/github.com/google/mtail
COPY . /go/src/github.com/google/mtail
RUN  make depclean && make install_deps && PREFIX=/go make -B install


FROM alpine:3.7

ARG version=0.0.0-local
ARG build_date=unknown
ARG commit_hash=unknown
ARG vcs_url=unknown
ARG vcs_branch=unknown

EXPOSE 3903
ENTRYPOINT ["/usr/bin/mtail"]

LABEL org.label-schema.vendor='Google' \
    org.label-schema.name='mtail' \
    org.label-schema.description='extract whitebox monitoring data from application logs for collection in a timeseries database' \
    org.label-schema.usage='https://github.com/google/mtail/blob/master/docs/Programming-Guide.md' \
    org.label-schema.url='https://github.com/google/mtail' \
    org.label-schema.vcs-url=$vcs_url \
    org.label-schema.vcs-branch=$vcs_branch \
    org.label-schema.vcs-ref=$commit_hash \
    org.label-schema.version=$version \
    org.label-schema.schema-version='1.0' \
    org.label-schema.build-date=$build_date

COPY --from=builder /go/bin/mtail /usr/bin/mtail
