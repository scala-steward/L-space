package lspace.structure

import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Matchers}

trait ResourceSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll with GraphFixtures {

  def sampledResourceTests(sampleGraph: Graph) = {}
}
