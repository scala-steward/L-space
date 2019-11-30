package lspace.services.codecs

import shapeless.Witness

object Text {
  type Plain = Witness.`"text/plain"`.T
//  type Plain = "text/plain" //scala 2.13 literal types
  type Html = Witness.`"text/html"`.T
//  type Html = "text/html" //scala 2.13 literal types

}
