package lspace.types

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CompareSpec extends AnyWordSpec with Matchers {

  "Positive[t]" when {
    "t of type Int" in {
      "-1: Positive[Int]" shouldNot typeCheck
      "0: Positive[Int]" shouldNot typeCheck
      "1: Positive[Int]" should compile
      "2: Positive[Int]" should compile
    }
    "t of type Double" in {
      "-1.0: Positive[Double]" shouldNot typeCheck
      "0.0: Positive[Double]" shouldNot typeCheck
      "0.1: Positive[Double]" should compile
      "1.0: Positive[Double]" should compile
    }
    "t of type Long" in {
      "-1L: Positive[Long]" shouldNot typeCheck
      "0L: Positive[Long]" shouldNot typeCheck
      "1L: Positive[Long]" should compile
      "2L: Positive[Long]" should compile
    }
  }
  val xx: Gt[0] = Gt(1)
  "Gt[t]" when {
    "t of type Int" in {
      "-1: Gt[0]" shouldNot typeCheck
      "0: Gt[0]" shouldNot typeCheck
      "1: Gt[0]" should compile
      "2: Gt[0]" should compile
      "2: Gt[0L]" shouldNot typeCheck
    }
    "t of type Double" in {
      "-1.0: Gt[0.0]" shouldNot typeCheck
      "0.0: Gt[0.0]" shouldNot typeCheck
      "0.1: Gt[0.0]" should compile
      "1.0: Gt[0.0]" should compile
      "Gt(1.0): Gt[0.0]" should compile
    }
    "t of type Long" in {
      "-1L: Gt[0L]" shouldNot typeCheck
      "0L: Gt[0L]" shouldNot typeCheck
      "1L: Gt[0L]" should compile
      "2L: Gt[0L]" should compile
      "2: Gt[0L]" shouldNot typeCheck
    }
  }

  "Gte[t]" when {
    "t of type Int" in {
      "-1: Gte[0]" shouldNot typeCheck
      "0: Gte[0]" should compile
      "1: Gte[0]" should compile
      "2: Gte[0]" should compile
      "2: Gte[0L]" shouldNot typeCheck
    }
    "t of type Double" in {
      "-1.0: Gte[0.0]" shouldNot typeCheck
      "0.0: Gte[0.0]" should compile
      "0.1: Gte[0.0]" should compile
      "1.0: Gte[0.0]" should compile
    }
    "t of type Long" in {
      "-1L: Gte[0L]" shouldNot typeCheck
      "0L: Gte[0L]" should compile
      "1L: Gte[0L]" should compile
      "2L: Gte[0L]" should compile
      "2: Gte[0L]" shouldNot typeCheck
    }
  }

  "Lt[t]" when {
    "t of type Int" in {
      "1: Lt[0]" shouldNot typeCheck
      "0: Lt[0]" shouldNot typeCheck
      "-1: Lt[0]" should compile
      "-2: Lt[0]" should compile
      "-2: Lt[0L]" shouldNot typeCheck
    }
    "t of type Double" in {
      "1.0: Lt[0.0]" shouldNot typeCheck
      "0.0: Lt[0.0]" shouldNot typeCheck
      "-0.1: Lt[0.0]" should compile
      "-1.0: Lt[0.0]" should compile
    }
    "t of type Long" in {
      "1L: Lt[0L]" shouldNot typeCheck
      "0L: Lt[0L]" shouldNot typeCheck
      "-1L: Lt[0L]" should compile
      "-2L: Lt[0L]" should compile
      "-2: Lt[0L]" shouldNot typeCheck
    }
  }

  "Lte[t]" when {
    "t of type Int" in {
      "1: Lte[0]" shouldNot typeCheck
      "0: Lte[0]" should compile
      "-1: Lte[0]" should compile
      "-2: Lte[0]" should compile
      "-2: Lte[0L]" shouldNot typeCheck
    }
    "t of type Double" in {
      "1.0: Lte[0.0]" shouldNot typeCheck
      "0.0: Lte[0.0]" should compile
      "-0.1: Lte[0.0]" should compile
      "-1.0: Lte[0.0]" should compile
    }
    "t of type Long" in {
      "1L: Lte[0L]" shouldNot typeCheck
      "0L: Lte[0L]" should compile
      "-1L: Lte[0L]" should compile
      "-2L: Lte[0L]" should compile
      "-2: Lte[0L]" shouldNot typeCheck
    }
  }
}
