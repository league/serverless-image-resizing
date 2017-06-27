.PHONY: all image package dist clean

TEST_BUCKET=league-shoga-test

all: package

image:
	docker build --tag amazonlinux:nodejs .

package: image
	docker run --rm --volume ${PWD}/lambda:/build amazonlinux:nodejs npm install --production

dist: package
	cd lambda && zip -FS -q -r ../dist/function.zip *

test: package
	aws s3 mb s3://$(TEST_BUCKET)
	aws s3 cp etc/pastel-hills-inverse.png s3://$(TEST_BUCKET)
	cd lambda && BUCKET=$(TEST_BUCKET) nix-shell -p nodejs --command \
	  "node_modules/aws-lambda-local/lambda-local.js -f index.js -e ../tests.json"
	aws s3 ls --recursive s3://$(TEST_BUCKET)

clean:
	rm -r lambda/node_modules
	docker rmi --force amazonlinux:nodejs
