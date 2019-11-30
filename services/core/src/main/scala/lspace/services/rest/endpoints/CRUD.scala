package lspace.services.rest.endpoints

import shapeless.:+:
import sttp.tapir._
import sttp.tapir.server.akkahttp._

object CRUD {

  val baseEndpoint: Endpoint[Unit, ErrorInfo, Unit, Nothing] =
    endpoint.in("api" / "v1.0").errorOut(jsonBody[ErrorInfo])

  val a = endpoint.in(header[Int]("abc")).out(stringBody)
  val b = path[String]("b") / path[String]("c")
  endpoint.in(b).post

  endpoint.in(stringBody).out(stringBody).in(stringBody).out(stringBody).mapInTo
}

trait Endpoint[I, E, O, S] {

  def :+:[I2, E2, O2, S2]: Endpoint[I :+: I2, E :+: E2, O :+: O2, S :+: S2]
}
