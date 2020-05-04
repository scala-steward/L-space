package lspace.parse

package object test {
  def init(): Unit = {
    scalajs.js.Dynamic.global.globalThis.fetch = scalajs.js.Dynamic.global.require("node-fetch")
    scalajs.js.Dynamic.global.require("abortcontroller-polyfill/dist/polyfill-patch-fetch")
    scalajs.js.Dynamic.global.globalThis.Headers = scalajs.js.Dynamic.global.require("fetch-headers")
  }
}
