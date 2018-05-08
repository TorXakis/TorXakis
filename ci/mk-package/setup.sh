#!/bin/bash

# Install the programs necessary to make a Linux package for TorXakis

# fpm (http://fpm.readthedocs.io/en/latest/)
sudo apt-get install ruby ruby-dev rubygems build-essential
sudo gem install --no-ri --no-rdoc fpm

