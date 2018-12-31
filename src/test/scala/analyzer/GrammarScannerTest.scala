package analyzer

import org.scalatest.FlatSpec

class GrammarScannerTest extends FlatSpec {

  import GrammarScanner._
  private val algo = new Algorithm
  import algo._

  "parseProduction" should "be able to parse normal production" in {
    val result = ("test", List("a", "b"))
    assertResult(result) {
      parseProduction(" test -> a b")
    }
  }
  it should "be able to report error" in {
    assertThrows[RuntimeException] {
      parseProduction("test ->")
    }
    assertThrows[RuntimeException] {
      parseProduction("-> test")
    }
    assertThrows[RuntimeException] {
      parseProduction("sadf -> ->")
    }
    assertThrows[RuntimeException] {
      parseProduction("nothing")
    }

  }

  "parseProductions" should "work correctly" in {
    val result = construct(List(
      "test" -> List("a", "b"),
      "test2" -> List("a", "b"),
      "test3" -> List("a", "b"),
    ))
    val source =
      """
        | test -> a b
        | test2   -> a   b
        | test3->a b
      """.stripMargin
    assertResult(result) {
      parseProductions(source)
    }
  }


}
