FROM ubuntu:16.04

RUN apt-get update && \
  apt-get install -y \
  git \
  ruby ruby-dev rubygems \
  build-essential \
  curl \
  rpm

RUN gem install --no-ri --no-rdoc fpm

RUN curl -sSL https://get.haskellstack.org/ | sh

COPY . script/

ADD mk-package.sh /mk-package.sh

ADD build /build

ENTRYPOINT ["/mk-package.sh"]
