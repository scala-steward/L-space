package lspace
package librarian
package steps

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StepsSpec extends AnyWordSpec with Matchers:

  "Coin" in {
    "Coin(0.0)" should compile
    "Coin(0.1)" should compile
    "Coin(1.0)" should compile
    "Coin(1.1)" shouldNot compile
    "Coin(-0.1)" shouldNot compile
    "Coin(-1)" shouldNot compile
  }
