package lspace.client.io

import lspace.client.session.AppServices
import org.scalajs
import org.scalajs.dom
import org.scalajs.dom.raw._

object DataWebsocket {

  var socket: WebSocket = null

  def startWebsocket: Unit = {
    scalajs.dom.console.log("Trying to open a websocket connection ...")
    if (socket != null) socket.close()
    socket = new WebSocket(getWebsocketUri(dom.document))

    socket.onopen = { (event: Event) =>
      scalajs.dom.console.log("Chat connection was successful!")

      //      socket.send(upickle.default.write[ToServerProtocol.ToServerMessage](ToServerProtocol.getPersons(1), 2))

      event
    }
    socket.onerror = { (event: Event) =>
      scalajs.dom.console.log(event)
    //      socket.close()
    //      startWebsocket
    }
    socket.onmessage = { (event: MessageEvent) =>
      }
    socket.onclose = { (event: CloseEvent) =>
      scalajs.dom.console.log(event.code)
      scalajs.dom.console.log(event.reason)
      scalajs.dom.console.log("Connection to socket lost! Reconnecting in 10 seconds ...")
      dom.window.setTimeout(() => {
        socket = null
        startWebsocket
      }, 10000)
    }

  }

  private def getWebsocketUri(document: Document): String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"

    //    s"$wsProtocol://${dom.document.location.host}/data/socket"
    s"$wsProtocol://${AppServices.domain}/socket"
  }
}
