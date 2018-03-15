#Use opencpu server as base image
FROM opencpu/base

#directory for software setup
WORKDIR /setup

#make installDependencies.R available
ADD . /setup

#install dependencies for FreeSASA, DSSP and ClustalOmega
RUN apt install -y wget libxml2-dev libjson-c-dev dssp clustalo

#install FreeSASA
RUN wget https://freesasa.github.io/freesasa-2.0.2.tar.gz &&\
	tar xzf freesasa-2.0.2.tar.gz &&\
	cd freesasa-2.0.2 &&\
	./configure &&\
	make &&\
	make install

#install R packages
RUN Rscript installDependencies.R

# setup caching location for mavevis
RUN mkdir -p /cache/
ENV MAVECACHE /cache/

#grant AppArmor permissions to cache directory
RUN printf "\n/cache rw,\n/cache/** rw,">>/etc/apparmor.d/opencpu.d/custom &&\
	service apparmor restart
