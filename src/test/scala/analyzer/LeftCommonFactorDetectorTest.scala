package analyzer

import org.scalatest.FlatSpec

class LeftCommonFactorDetectorTest extends FlatSpec {
  val algo = new Algorithm with LeftCommonFactorDetector
  import algo._

  private val prodsWithLF = construct(List(
    "A" -> List("B", "c"),
    "B" -> List("b", "c"),
    "A" -> List("B", "d"),
  ))

  private val prodsWithoutLF = construct(List(
    "A" -> List("B", "c"),
    "B" -> List("b", "c"),
    "C" -> List("B", "d"),
    "C" -> List("d", "e"),
  ))

  "hasLeftCommonFactor" should "detect common left factor" in {
    assert(hasLeftCommonFactor(prodsWithLF))
  }

  it should "not be wrong" in {
    assert(!hasLeftCommonFactor(prodsWithoutLF))
  }

  "eliminateLeftCommonFactor" should "work correctly" in {
    assert(
      !hasLeftCommonFactor(eliminateLeftCommonFactor(prodsWithLF))
    )
  }

  it should "not report wrong" in {
    assert(
      !hasLeftCommonFactor(eliminateLeftCommonFactor(prodsWithoutLF))
    )
  }

  it should "work as excepted" in {
    val prods = construct(List(
      "A" -> List("b", "c", "d", "e"),
      "A" -> List("j", "k", "l", "m"),
      "A" -> List("j", "k", "m", "n"),
      "A" -> List("b", "c", "f", "g"),
    ))

    val result = construct(List(
      "A" -> List("b", "c", "A\'"),
      "A" -> List("j", "k", "A\'\'"),
      "A\'" -> List("d", "e"),
      "A\'" -> List("f", "g"),
      "A\'\'" -> List("m", "n"),
      "A\'\'" -> List("l", "m"),
    ))
    assertResult(result.toSet) {
      eliminateLeftCommonFactor(prods).toSet
    }
  }

  "longestCommonPrefixProduction" should "work in normal case1" in {
    val prods = construct(List(
      "S" -> List("a", "b", "c", "d"),
      "S" -> List("a", "b", "e", "f"),
      "S" -> List("e", "f"),
      "S" -> List("f", "g"),
    ))
    val result = (List(Term("a"), Term("b")), prods.take(2).toSet)
    assertResult(result) {
      longestCommonPrefixProductions(prods)
    }
  }
  it should "work in normal case2" in {
    val prods = construct(List(
      "S" -> List("a", "b", "c", "d"),
      "S" -> List("a", "b", "e", "f"),
      "S" -> List("e", "f", "g"),
      "S" -> List("e", "f", "g"),
    ))
    val result = (prods(2).body, prods.drop(2).toSet)
    assertResult(result) {
      longestCommonPrefixProductions(prods)
    }
  }
  it should "work without common prefix" in {
    val prods = construct(List(
      "S" -> List("a", "b", "c", "d"),
      "S" -> List("b", "b", "e", "f"),
      "S" -> List("c", "f", "g"),
      "S" -> List("d", "f", "g"),
    ))
    val result = (List.empty, Set.empty)
    assertResult(result) {
      longestCommonPrefixProductions(prods)
    }
  }
  it should "work with multiple common prefix" in {
    val prods = construct(List(
      "S" -> List("a", "b", "c", "d"),
      "S" -> List("a", "b", "e", "f"),
      "S" -> List("c"),
      "S" -> List("c", "b", "e", "f"),
      "S" -> List("d", "f", "g"),
      "S" -> List("d", "f", "i"),
    ))
    val result1 = (List(Term("d"), Term("f")), prods.drop(4).toSet)
    val result2 = (List(Term("a"), Term("b")), prods.take(2).toSet)
    assert {
      val result = longestCommonPrefixProductions(prods)
      result == result1 || result == result2
    }
  }
  it should "work with only one production" in {
    val prods = construct(List(
      "S" -> List("a", "b", "c", "d"),
    ))
    assertResult((List.empty, Set.empty)) {
      longestCommonPrefixProductions(prods)
    }
  }

  "splitRecursive" should "work as excepted" in {
    val prods = construct(List(
      "S" -> List("a", "b", "c", "d"),
      "S" -> List("a", "b", "e", "f"),
      "S" -> List("e", "f", "h"),
      "S" -> List("e", "f", "g"),
    ))
    val result1 = construct(List(
      "S" -> List("a", "b", "S\'"),
      "S\'" -> List("c", "d"),
      "S\'" -> List("e", "f"),
      "S" -> List("e", "f", "S\'\'"),
      "S\'\'" -> List("h"),
      "S\'\'" -> List("g"),
    )).toSet
    val result2 = construct(List(
      "S" -> List("a", "b", "S\'\'"),
      "S\'\'" -> List("c", "d"),
      "S\'\'" -> List("e", "f"),
      "S" -> List("e", "f", "S\'"),
      "S\'" -> List("h"),
      "S\'" -> List("g"),
    )).toSet
    assert {
      val result = splitRecursive(prods, 1).toSet
      result == result1 || result == result2
    }
  }
}
