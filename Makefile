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
	aws s3 cp etc/pastel-hills-inverse.png "s3://$(TEST_BUCKET)/fullsize/a3f7.LEcqytQt83qwZN1xyzvgJ7Op92AtPmauZI4bNwvuwm9leEmKjb0IWnARYRTJZaeGXQ2LyCYmdCbxlYuyx3UsCw.png"
	aws s3 cp etc/datoptic.jpg             "s3://$(TEST_BUCKET)/fullsize/c21.-1hHvql9LlvDiKGc9zZsJwGCUcN4xbu3PFkiguH1ExSfII5bTO3j_3PSX6cYrJdwbUDWWcyhCi85wTtlKP0NDQ.jpg"
	cd lambda && BUCKET=$(TEST_BUCKET) PUBLIC_KEY=$(PUBLIC_KEY) nix-shell -p nodejs --command \
	  "node_modules/aws-lambda-local/lambda-local.js -f index.js -e ../tests.json"
	aws s3 ls --recursive s3://$(TEST_BUCKET)

sig:
	runhaskell -Wall sign.hs "$(SECRET_KEY)" "$(BODY)"

clean:
	rm -r lambda/node_modules
	docker rmi --force amazonlinux:nodejs
