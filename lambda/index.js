'use strict';

const Ed25519 = require('ed25519');
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

  const match =
        key.match(/^resized\/([-_0-9A-Za-z]+)\/([\.0-9a-f]+)\.(\d+)x(\d+)\.([a-z]+)$/);
  //                          ^1SIG             ^2ID           ^3W   ^4H    ^5EXT

  if(match == null) {
    callback({
      statusCode: '400',
      code: 'BadRequest',
      body: 'The path was malformed'
    });
    return;
  }

  // Extract parts of the regex
  const photoId = match[2];
  const width = parseInt(match[3], 10);
  const height = parseInt(match[4], 10);
  const sig = match[1];
  const ext = match[5];
  const signedBase = photoId + "." + width + "x" + height + "." + ext;
  const originalKey = "orig/" + photoId + "." + ext;

  // Verify the cryptographic signature
  const r = Ed25519.Verify(Buffer.from(signedBase, 'utf-8'),
                           Buffer.from(sig, 'base64'),
                           public_key);
  if(!r) {
    callback({
      statusCode: '403',
      code: 'Forbidden',
      body: 'Signature is invalid'
    });
    return;
  }

  // Interpret the extension as an image type
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
      "resized/7mlmKcANExJGQxm82N1IV5RHDZRT9zh8QniqP7MJ9hKgDH1o_7H4w2L1IVD0_lMxoRssOsVl3qJ1GqoEWLT6DA/ffffffffffffffff.abcdef1234.1x1.png",
      "NoSuchKey response",
      function(err, result) {
        return err != null && 'code' in err && err.code == 'NoSuchKey';
      });

    testHandler(
      "resized/BTGohpOGOqehvwV7R6ULd2fqnFN79_9M3FmaX00X8btOS38Lz-ukt4PWhElHKDNWzxMaDmeMsQpykl-CLXqkCA/ab12.fa33.320x200.png",
      "Bad signature",
      function(err, result) {
        return err != null && 'code' in err && err.code == 'Forbidden';
      }
    );

    testHandler(
      // Original is 3200x2000, so we'll try 10% = 320x200
      "resized/UkZ5FK9-DN__RihkpDqHr2eizyBASlKPcgIiu9XA04-ImLwaXY_d-5xUl2BbvEV0uHysEb_9yM12FaGFXcbuCg/a3f7.2c472acad42df37ab064dd71.320x200.png",
      "Good resize response for png",
      goodRedirectResult
    );

    testHandler(
      // Original was 3120x4160 so we'll try 25% = 780x1040
      "resized/srtEql7iz8qyt4EOBtNexbkOMDGo4V5yrTndZy5vm9RSEoA4z8psiUPELxKD2wrJtqSDqMb8_Ojdtep7YaVvAw/c21.fb5847bea97d2e5bc388a19c.780x1040.jpg",
      "Good resize response for jpg",
      goodRedirectResult
    );
  }
  else {
    theHandler(event, context, callback);
  }
}
