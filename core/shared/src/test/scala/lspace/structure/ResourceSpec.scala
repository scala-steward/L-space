package lspace.structure

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

trait ResourceSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll with GraphFixtures {

  def sampledResourceTests(sampleGraph: Graph) = {}
}
