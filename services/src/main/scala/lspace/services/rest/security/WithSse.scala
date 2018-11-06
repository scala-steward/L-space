package lspace.services.rest.security

import argonaut.Json
import com.twitter.util.Promise
import io.finch.sse.ServerSentEvent

trait WithSse {
  var sse: Option[Promise[ServerSentEvent[Json]]] = None
}
