FROM ubuntu

ARG application=test
ENV APP=$application
COPY $application-root /

ENTRYPOINT "./$APP"
