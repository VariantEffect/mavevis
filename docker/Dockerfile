FROM ubuntu:16.04

#Force apt-get to skip confirmation questions
ENV DEBIAN_FRONTEND noninteractive

#Install apache httpd and enable CGI
RUN \
  apt update && \
  apt -y dist-upgrade && \
  apt install -y apache2 && \
  a2enmod cgi

# Apache ports for HTTP and HTTPS
EXPOSE 80
EXPOSE 443
# EXPOSE 8004

#directory for software setup
RUN mkdir /setup
WORKDIR /setup

#install dependencies for app
RUN apt update && \
  apt install -y r-base wget libcurl4-openssl-dev libssl-dev \
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
COPY installDependencies.R /setup/
RUN Rscript installDependencies.R


# Create mavevis' directory structure and grant access to apache
RUN mkdir -p /var/www/mavevis/httpdocs &&\
  mkdir -p /var/www/mavevis/logs &&\
  chmod 777 /var/www/mavevis/logs &&\
  mkdir -p /cache/ && chmod 777 /cache/

#Move CGI scripts to staging location and grant access to apache
ADD cgi/ /var/www/html/mavevis/httpdocs/
RUN chmod a+rx /var/www/html/mavevis/httpdocs/*.R

#Introduce apache configuration file for app and enable it
COPY mavevis.conf /etc/apache2/sites-available/
RUN a2dissite 000-default.conf && a2ensite mavevis

# Print logs to stdout
RUN \
  ln -sf /proc/self/fd/1 /var/log/apache2/access.log && \
  ln -sf /proc/self/fd/1 /var/log/apache2/error.log && \
  ln -sf /proc/self/fd/1 /var/www/mavevis/logs/exec.log && \
  ln -sf /proc/self/fd/1 /var/www/mavevis/logs/error.log && \
  ln -sf /proc/self/fd/1 /var/www/mavevis/logs/access.log

#start apache
CMD apachectl -DFOREGROUND
