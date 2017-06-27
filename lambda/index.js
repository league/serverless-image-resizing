'use strict';

const AWS = require('aws-sdk');
const S3 = new AWS.S3({
  signatureVersion: 'v4',
});
const Sharp = require('sharp');

const BUCKET = process.env.BUCKET;
const URL = process.env.URL;

const theHandler = function(event, context, callback) {
  const key = event.queryStringParameters.key;
  const match = key.match(/(\d+)x(\d+)\/(.*)/);
  if(match == null) {
    callback({
      statusCode: '400',
      code: 'BadRequest',
      body: 'The path was malformed'
    });
    return;
  }

  const width = parseInt(match[1], 10);
  const height = parseInt(match[2], 10);
  const originalKey = match[3];

  S3.getObject({Bucket: BUCKET, Key: originalKey}).promise()
    .then(data => Sharp(data.Body)
      .resize(width, height)
      .toFormat('png')
      .toBuffer()
    )
    .then(buffer => S3.putObject({
        Body: buffer,
        Bucket: BUCKET,
        ContentType: 'image/png',
        Key: key,
      }).promise()
    )
    .then(() => callback(null, {
        statusCode: '301',
        headers: {'location': `${URL}/${key}`},
        body: '',
      })
    )
    .catch(err => callback(err))
}

const testHandler = function(key, mesg, check) {
  theHandler(
    {queryStringParameters: {key: key}},
    null,
    function(err, result) {
      if(check(err, result)) {
        console.log(" ✓", mesg);
      }
      else {
        console.log(" ✗", mesg);
        console.log("ERR:", err);
        console.log("RESULT:", result);
        //throw new Error(mesg);
      }
    });
}

exports.handler = function(event, context, callback) {
  if('tests' in event) {
    console.log("------ TESTS ------");

    testHandler(
      "badbad",
      "BadRequest for malformed path",
      function(err, result) {
        return err != null && 'code' in err && err.code == 'BadRequest';
      });

    testHandler(
      "1x1/nothing",
      "NoSuchKey response",
      function(err, result) {
        return err != null && 'code' in err && err.code == 'NoSuchKey';
      });

    testHandler(
      "320x200/pastel-hills-inverse.png", // was 3200x2000
      "Good resize response for png",
      function(err, result) {
        return err == null && result != null && 'statusCode' in result &&
          result.statusCode == '301' && 'headers' in result &&
          'location' in result.headers
      });
  }
  else {
    theHandler(event, context, callback);
  }
}
