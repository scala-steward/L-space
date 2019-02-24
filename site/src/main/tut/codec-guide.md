---
layout: docs
title: Codec guide
position: 4
---

# Codec Guide
* [Supported formats](#supported-formats)
  * [Json-ld](#json-ld)
* [Encoding/Decoding](#encoding/decoding)
 
## Supported formats
L-space is about semantic graph computing. It is expected data distributed and hence it should provide for the necessary tools to coop.

### Json-ld
The main format used for encoding/decoding information is json-ld. The current implementation takes an interpretation 
on the current json-ld specs but should read existing json-ld fine. 
Still working on supporting pure json-ld and creating a custom json-ld-ish version to allow for the full spectrum of L-space.

## Encoding/Decoding
L-space provides for generic type parsers. All logic is available within 'lspace-parse-core'. 
The main project supports a few implementation, one with Argonaut ('lspace-parse-argonaut') and one with Circe ('lspace-parse-circe').
Implementions with other json-libraries only takes about 20 lines of code. 

