package analyzer

import org.scalatest.FlatSpec

class LeftRecursiveDetectorTest extends FlatSpec {
  val algo = new Algorithm with LeftRecursiveDetector
  import algo._

  "isLeftRecur" should "be able ot detect left recursive" in {
    val prod = construct(List(
      "A" -> List("A", "c"),
      "A" -> List("b", "c"),
    ))
    assert(isLeftRecursive(prod))
  }

  it should "report false with non-recursive prods" in {
    val prods = construct(List(
      "A" -> List("B", "c"),
      "B" -> List("b", "c"),
    ))
    assert(!isLeftRecursive(prods))
  }

  "eliminateLeftRecursion" should "eliminate in original productions' order" in {
    val prod = construct(List(
      "E" -> List("E", "+", "T"),
      "E" -> List("T"),
      "T" -> List("T", "*", "F"),
      "T" -> List("F"),
      "F" -> List("(", "E", ")"),
      "F" -> List("n"),
    ))

    val excepted = construct(List(
      "E" -> List("T", "E\'"),
      "E\'" -> List("+", "T", "E\'"),
      "E\'" -> List(""),
      "T" -> List("F", "T\'"),
      "T\'" -> List("*", "F", "T\'"),
      "T\'" -> List(""),
      "F" -> List("(", "E", ")"),
      "F" -> List("n"),
    ))
    assert {
      val result = eliminateLeftRecursion(prod)
      !isLeftRecursive(result) &&
      result.toSet == excepted.toSet
    }
  }

  "orderedGrouping" should "work as excepted" in {
    val prod = construct(List(
      /*0*/"E" -> List("t", "e", "s"),
      /*1*/"F" -> List("t"),
      /*2*/"E" -> List("e", "s", "t"),
      /*3*/"A" -> List("e"),
      /*4*/"F" -> List("s", "t", "e"),
      /*5*/"A" -> List("s"),
    ))
    val result = List(
      Set(0,2),
      Set(1,4),
      Set(3,5),
    ).map(_.map(prod(_)))
    assertResult(result) {
      orderedGrouping(prod)
    }
  }
}
