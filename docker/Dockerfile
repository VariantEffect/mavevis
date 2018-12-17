# Copyright (C) 2018  Jochen Weile, Roth Lab
#
# This file is part of MaveVis.
#
# MaveVis is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# MaveVis is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with MaveVis.  If not, see <https://www.gnu.org/licenses/>.

# This file defines a docker image encapsulating a CGI-enabled apache server
# with a RestFul service and web-app for MaveVis with all its dependencies.
#
# A makefile is provided to assist with building and running

FROM ubuntu:16.04
MAINTAINER Jochen Weile <jochenweile@gmail.com>
ARG UPDATE_RPKG=0

#Force apt-get to skip confirmation questions
ENV DEBIAN_FRONTEND noninteractive

#Install apache httpd and enable CGI
RUN apt update && \
    apt -y dist-upgrade && \
    apt --no-install-recommends install -y apache2 && \
    a2enmod cgi headers

# Apache ports for HTTP and HTTPS
EXPOSE 80
EXPOSE 443
# EXPOSE 8004

#directory for software setup
RUN mkdir /setup
WORKDIR /setup

#install dependencies for app
RUN apt update && \
    apt --no-install-recommends install -y r-base wget \
    g++ make libcurl4-openssl-dev libssl-dev \
    libxml2 libxml2-dev libjson-c-dev dssp clustalo 

#download, build and install FreeSASA, then cleanup temp files
RUN wget https://github.com/mittinatten/freesasa/releases/download/2.0.2/freesasa-2.0.2.tar.gz &&\
    tar xzf freesasa-2.0.2.tar.gz &&\
    cd freesasa-2.0.2 &&\
    ./configure --disable-xml &&\
    make &&\
    make install&&\
    cd .. &&\
    rm -r freesasa*

#install R packages
#The UPDATE_RPKG argument doens't actually do anything in the script
#it just allows forcing docker to override the cache for this instruction
#by running $docker build --build-arg UPDATE_RPKG=`date +%s` -t jweile/mavevis .
COPY installDependencies.R /setup/
RUN Rscript installDependencies.R ${UPDATE_RPKG}


# Create mavevis' directory structure and grant access to apache
RUN mkdir -p /var/www/mavevis/httpdocs &&\
    mkdir -p /var/www/mavevis/logs &&\
    chmod 777 /var/www/mavevis/logs &&\
    mkdir -p /var/www/mavevis/cache/ &&\
    chmod 777 /var/www/mavevis/cache/

#set environment variables so the app knows were to write/read data
ENV MAVEVIS_CACHE /var/www/mavevis/cache/
ENV MAVEVIS_LOGS /var/www/mavevis/logs/
#Note: the apache user cannot see these variables, so they must also be set in the
# apache config file (mavevis.conf)!!!
RUN printf "export MAVEVIS_CACHE=/var/www/mavevis/cache/\n">>/etc/apache2/envvars &&\
    printf "export MAVEVIS_LOGS=/var/www/mavevis/logs/\n">>/etc/apache2/envvars

#Move CGI scripts to staging location and grant access to apache
ADD cgi/ /var/www/html/mavevis/httpdocs/
COPY mavevis_launcher.R /var/www/html/mavevis/
COPY searchIndex.csv /var/www/mavevis/cache/
RUN chmod a+rx /var/www/html/mavevis/httpdocs/*.R &&\
    chmod a+rx /var/www/html/mavevis/*.R &&\
    mkdir /var/www/html/mavevis/httpdocs/results/ &&\
    chmod 777 /var/www/html/mavevis/httpdocs/results/ &&\
    chmod 666 /var/www/mavevis/cache/searchIndex.csv

#Introduce apache configuration files for app and enable it
COPY security.conf /etc/apache2/conf-available/
COPY mavevis.conf /etc/apache2/sites-available/
RUN a2dissite 000-default.conf && a2ensite mavevis

# Print logs to stdout
# RUN \
#   ln -sf /proc/self/fd/1 /var/log/apache2/access.log && \
#   ln -sf /proc/self/fd/1 /var/log/apache2/error.log && \
#   # ln -sf /proc/self/fd/1 /var/www/mavevis/logs/exec.log && \
#   ln -sf /proc/self/fd/1 /var/www/mavevis/logs/error.log && \
#   ln -sf /proc/self/fd/1 /var/www/mavevis/logs/access.log

#setup startup script and daemon
COPY startup.sh daemon.R sync.R /setup/

#startup script starts apache and daemon
CMD bash /setup/startup.sh
