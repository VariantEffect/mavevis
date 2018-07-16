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