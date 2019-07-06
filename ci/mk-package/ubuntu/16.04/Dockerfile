FROM ubuntu:16.04

WORKDIR /build

RUN apt-get update && \
  apt-get install -y \
  git \
  ruby ruby-dev rubygems \
  build-essential \
  curl \
  rpm \
  libtinfo-dev

RUN gem install --no-ri --no-rdoc fpm

RUN curl -sSL https://get.haskellstack.org/ | sh

ADD .package-build /build

ADD ubuntu/mk-package.sh /mk-package.sh

ENTRYPOINT ["/mk-package.sh"]
