'use strict';

const AWS = require('aws-sdk');
const S3 = new AWS.S3({
  signatureVersion: 'v4',
});
const Sharp = require('sharp');

const BUCKET = process.env.BUCKET;
const URL = process.env.URL;
const PUBLIC_KEY_B64 = process.env.PUBLIC_KEY;
const public_key = Buffer.from(PUBLIC_KEY_B64, 'base64');

const theHandler = function(event, context, callback) {
  const key = event.queryStringParameters.key;
  const match = key.match(/custom\/([0-9a-f]+)\.([-_0-9A-Za-z]+)\.(\d+)x(\d+)\.([-_0-9A-Za-z]+)\.([a-z]+)/);
  //                                ^ID          ^SIG1            ^W    ^H      ^SIG2             ^EXT
  if(match == null) {
    callback({
      statusCode: '400',
      code: 'BadRequest',
      body: 'The path was malformed'
    });
    return;
  }

  const photoId = match[1];
  const sig1 = match[2];
  const width = parseInt(match[3], 10);
  const height = parseInt(match[4], 10);
  const sig2 = match[5]; // SIG2 (the one we're verifying)
  const ext = match[6];
  const signedBase = sig1 + "." + width + "x" + height;
  const originalKey = "fullsize/" + photoId + "." + sig1 + "." + ext;

  var targetFormat;
  var contentType;

  if(ext == "jpg") {
    targetFormat = "jpeg";
    contentType = "image/jpeg";
  }
  else if(ext == "png") {
    targetFormat = "png";
    contentType = "image/png";
  }
  else {
    callback({
      statusCode: '400',
      code: 'BadRequest',
      body: 'Unsupported extension ' + ext
    });
    return;
  }

  S3.getObject({Bucket: BUCKET, Key: originalKey}).promise()
    .then(data => Sharp(data.Body)
      .resize(width, height)
      .toFormat(targetFormat)
      .toBuffer()
    )
    .then(buffer => S3.putObject({
        Body: buffer,
        Bucket: BUCKET,
        ContentType: contentType,
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

const goodRedirectResult = function(err, result) {
  return err == null && result != null && 'statusCode' in result &&
    result.statusCode == '301' && 'headers' in result &&
    'location' in result.headers
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
      "custom/ffffffffffffffff.ABCD1234abcd.1x1.EFGH5678efgh.png",
      "NoSuchKey response",
      function(err, result) {
        return err != null && 'code' in err && err.code == 'NoSuchKey';
      });

    testHandler(
      // Original is 3200x2000, so we'll try 10% = 320x200
      "custom/a3f7.LEcqytQt83qwZN1xyzvgJ7Op92AtPmauZI4bNwvuwm9leEmKjb0IWnARYRTJZaeGXQ2LyCYmdCbxlYuyx3UsCw.320x200.abc.png",
      "Good resize response for png",
      goodRedirectResult
    );

    testHandler(
      // Original was 3120x4160 so we'll try 25% = 780x1040
      "custom/c21.-1hHvql9LlvDiKGc9zZsJwGCUcN4xbu3PFkiguH1ExSfII5bTO3j_3PSX6cYrJdwbUDWWcyhCi85wTtlKP0NDQ.780x1040.abc.jpg",
      "Good resize response for jpg",
      goodRedirectResult
    );
  }
  else {
    theHandler(event, context, callback);
  }
}
