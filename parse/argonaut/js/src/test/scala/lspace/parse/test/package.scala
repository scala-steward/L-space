package lspace.parse

package object test {
  def init(): Unit = {
    val g = scalajs.js.Dynamic.global.globalThis
    g.fetch = g.require("node-fetch")
    g.require("abortcontroller-polyfill/dist/polyfill-patch-fetch")
    g.Headers = g.require("fetch-headers")
  }
}
