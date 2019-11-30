package lspace.services.finch

import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response, Status}
import io.finch.Input
import lspace.services
import lspace.services.LService
import lspace.services.rest.endpoints.LabeledNodeApi
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

object LServiceSpec {}
trait LServiceSpec extends AsyncWordSpec with Matchers {

  implicit class WithApiService[Json](labeledNodeApi: LabeledNodeApi[Json])(
      implicit service: LService[Request, Response, Service]) {
    import services.util._
    def labeledApiTests = {
      val label = labeledNodeApi.newNodeBaseIri //ontology.label("en").getOrElse(labeledNodeApi.ontology.iri).toLowerCase()
      s"have an $label-api" in {
        val input = Input
          .get(s"/")
          .withHeaders("Accept" -> "application/ld+json")
        val res = service.service(input.request)

        res.map { response =>
          val headers = response.headerMap
          response.status shouldBe Status.Ok
          response.contentType shouldBe Some("application/ld+json")
        }
      }
    }
  }
}
