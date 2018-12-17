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

build:	
	docker build --build-arg UPDATE_RPKG=`date +%s` -t jweile/mavevis:$(COMMIT) .
	docker tag jweile/mavevis:$(COMMIT) jweile/mavevis:latest

clean:
	docker stop mavevis&&docker container rm mavevis

run:
	nohup docker run -t -p 80:80 --name mavevis jweile/mavevis:$(COMMIT) &

runwmount:
	nohup docker run -t -p 80:80 --name mavevis \
		--mount type=bind,source=/home/jweile/projects/,target=/projects,readonly \
		jweile/mavevis:$(COMMIT) &

push:
	docker push jweile/mavevis:$(COMMIT)