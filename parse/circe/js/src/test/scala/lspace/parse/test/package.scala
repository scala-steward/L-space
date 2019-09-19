package lspace.parse

package object test {
  val g = scala.scalajs.js.Dynamic.global
  g.fetch = g.require("node-fetch")
  g.require("abortcontroller-polyfill/dist/polyfill-patch-fetch")
  g.Headers = g.require("fetch-headers")
}
