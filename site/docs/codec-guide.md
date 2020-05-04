---
layout: docs
title: Codec guide
position: 4
---

# Codec Guide
* [The Context](#the-context)
* [Supported formats](#supported-formats)
  * [Json-ld](#json-ld)
  * [GraphQL](#graphql)
* [Encoding/Decoding](#encoding/decoding)

## The Context
Communication between entities requires coordination in order to understand eachother. To disseminate information there 
needs to be some sort of conductor to guarantee a smooth flow. This conductor is the 'context'. A context hold a mapping 
of aliases to predicate uri's, directives, expected datatypes and containers. 
This context is a gatekeeper's instruction on what an API is allowed to consume and execute or what it is allowed to produce. 

## Supported formats
L-space is about semantic graph computing. It expects data to be distributed and hence it should provide for the 
necessary tools to coop.

### Json-ld
The main format used for encoding/decoding information is json-ld. The current implementation takes an interpretation 
on the current json-ld specs but should read existing json-ld fine. 
Still working on supporting pure json-ld and creating a custom json-ld-ish version to allow for the full spectrum of L-space.

### GraphQL
GraphQL is supported for querying. Responses will be returned as json or json-ld. The GraphQL queries are reinforced by 
a context which is a replacement for the 'GraphQL Type System'. 

## Encoding/Decoding
L-space provides for generic type parsers. All logic is available within 'lspace-parse-core'. 
The main project supports a few implementation, one with Argonaut ('lspace-parse-argonaut') and one with Circe ('lspace-parse-circe').
Implementions with other json-libraries only takes about 20 lines of code. 

