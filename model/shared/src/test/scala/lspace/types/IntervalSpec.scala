package lspace.types

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IntervalSpec extends AnyWordSpec with Matchers:

  "Between[min, max]" when {
    "bounds of type Int" in {
      "-1: Between[0,0]" shouldNot compile
      "0: Between[0,1]" should compile
      "1: Between[0,2]" should compile
      "2: Between[0,2]" should compile
      "3: Between[0,2]" shouldNot compile
      "2: Between[0,2L]" shouldNot compile
    }
    "bounds of type Double" in {
      "-1.0: Between[0.0,2.0]" shouldNot compile
      "0.0: Between[0.0,2.0]" should compile
      "0.1: Between[0.0,2.0]" should compile
      "1.0: Between[0.0,2.0]" should compile
      "3.0: Between[0.0,2.0]" shouldNot compile
      "Between(1.0): Between[0.0,2.0]" should compile
    }
    "bounds of type Long" in {
      "-1L: Between[0L,2L]" shouldNot typeCheck
      "0L: Between[0L,2L]" should compile
      "1L: Between[0L,2L]" should compile
      "2L: Between[0L,2L]" should compile
      "3L: Between[0L,2L]" shouldNot compile
      "2: Between[0L,2L]" shouldNot compile
    }
  }

  "Inside[min, max]" when {
    "bounds of type Int" in {
      "-1: Inside[0,0]" shouldNot compile
      "0: Inside[0,1]" shouldNot compile
      "1: Inside[0,2]" should compile
      "2: Inside[0,2]" shouldNot compile
      "3: Inside[0,2]" shouldNot compile
      "1: Inside[0,2L]" shouldNot compile
    }
    "bounds of type Double" in {
      "-1.0: Inside[0.0,2.0]" shouldNot compile
      "0.0: Inside[0.0,2.0]" shouldNot compile
      "0.1: Inside[0.0,2.0]" should compile
      "1.0: Inside[0.0,2.0]" should compile
      "3.0: Inside[0.0,2.0]" shouldNot compile
      "Inside(1.0): Inside[0.0,2.0]" should compile
    }
    "bounds of type Long" in {
      "-1L: Inside[0L,2L]" shouldNot typeCheck
      "0L: Inside[0L,2L]" shouldNot compile
      "1L: Inside[0L,2L]" should compile
      "2L: Inside[0L,2L]" shouldNot compile
      "3L: Inside[0L,2L]" shouldNot compile
      "2: Inside[0L,2L]" shouldNot compile
    }
  }
  (-1: Outside[0,1])
  "Outside[min, max]" when {
    "bounds of type Int" in {
      "-1: Outside[0,0]" should compile
      "0: Outside[0,1]" shouldNot compile
      "1: Outside[0,2]" shouldNot compile
      "2: Outside[0,2]" shouldNot compile
      "3: Outside[0,2]" should compile
      "3: Outside[0,2L]" shouldNot compile
    }
    "bounds of type Double" in {
      "-1.0: Outside[0.0,2.0]" should compile
      "0.0: Outside[0.0,2.0]" shouldNot compile
      "0.1: Outside[0.0,2.0]" shouldNot compile
      "2.0: Outside[0.0,2.0]" shouldNot compile
      "3.0: Outside[0.0,2.0]" should compile
      "Outside(3.0): Outside[0.0,2.0]" should compile
    }
    "bounds of type Long" in {
      "-1L: Outside[0L,2L]" should compile
      "0L: Outside[0L,2L]" shouldNot compile
      "1L: Outside[0L,2L]" shouldNot compile
      "2L: Outside[0L,2L]" shouldNot compile
      "3L: Outside[0L,2L]" should compile
      "3: Outside[0L,2L]" shouldNot compile
    }
  }
