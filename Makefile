.PHONY: all image package dist clean

TEST_BUCKET=league-shoga-test

# These is an Ed25519 key pair for testing; replace in production!
PUBLIC_KEY=82pYsrBDzrTga62mhdh_ZQsODH8-X1uHgnkDi5VbQc0=
SECRET_KEY=CgsFNfvQFQPOsrlVLv0EiVPZ_cUXIqVWGvU4O_2x_5vzaliysEPOtOBrraaF2H9lCw4Mfz5fW4eCeQOLlVtBzQ==
BODY=Hello world

all: package

image:
	docker build --tag amazonlinux:nodejs .

package: image
	docker run --rm --volume ${PWD}/lambda:/build amazonlinux:nodejs npm install --production

dist: package
	cd lambda && zip -FS -q -r ../dist/function.zip *

test: package
	aws s3 mb s3://$(TEST_BUCKET)
	aws s3 cp etc/pastel-hills-inverse.png "s3://$(TEST_BUCKET)/orig/a3f7.2c472acad42df37ab064dd71.png"
	aws s3 cp etc/datoptic.jpg             "s3://$(TEST_BUCKET)/orig/c21.fb5847bea97d2e5bc388a19c.jpg"
	cd lambda && BUCKET=$(TEST_BUCKET) PUBLIC_KEY=$(PUBLIC_KEY) nix-shell -p nodejs --command \
	  "node_modules/aws-lambda-local/lambda-local.js -f index.js -e ../tests.json"
	aws s3 ls --recursive s3://$(TEST_BUCKET)

sig:
	runhaskell -Wall sign.hs "$(SECRET_KEY)" "$(BODY)"

clean:
	rm -rf lambda/node_modules
	aws s3 rm --recursive s3://$(TEST_BUCKET)

reallyclean: clean
	docker rmi --force amazonlinux:nodejs
