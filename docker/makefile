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

COMMIT := $(shell git rev-parse HEAD|cut -c1-8)
#These defaults can be overridden using: make -e USER=itsme PROJECTDIR=/path/to/dir/
USER ?= jweile
PROJECTDIR ?= /home/jweile/projects/

build:	
	docker build --build-arg UPDATE_RPKG=`date +%s` -t $(USER)/mavevis:$(COMMIT) .
	docker tag $(USER)/mavevis:$(COMMIT) $(USER)/mavevis:latest

clean:
	docker stop mavevis&&docker container rm mavevis

run:
	nohup docker run -t -p 80:80 --name mavevis $(USER)/mavevis:latest &

runwmount:
	nohup docker run -t -p 80:80 --name mavevis \
		--mount type=bind,source=$(PROJECTDIR),target=/projects,readonly \
		$(USER)/mavevis:latest &

push:
	docker push $(USER)/mavevis:$(COMMIT)
	docker push $(USER)/mavevis:latest
