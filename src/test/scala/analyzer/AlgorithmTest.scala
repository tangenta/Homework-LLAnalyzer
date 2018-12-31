package analyzer

import org.scalatest.FlatSpec

class AlgorithmTest extends FlatSpec {
  val algo = new Algorithm
  import algo._
  private val prods = construct(List(
    "A" -> List("B", "C"),
    "B" -> List("b", "c"),
    "C" -> List("D", "E"),
    "D" -> List("d"),
    "D" -> List(""),
    "E" -> List("e"),
  ))
  private def localFirst: Symbol => Set[Term] = first(prods, _)
  private def localFollow: NonTerm => Set[Term] = follow(prods, _, NonTerm("A"))

  "first" should "map A -> {b}" in {
    assertResult(Set("b").map(Term)) {
      localFirst(NonTerm("A"))
    }
  }

  it should "map C -> {d, e}" in {
    assertResult(Set("d", "e").map(Term)) {
      localFirst(NonTerm("C"))
    }
  }

  it should "map D -> {d, ''}" in {
    assertResult(Set("d", Empty.value.str).map(Term)) {
      localFirst(NonTerm("D"))
    }
  }

  "follow" should "map A -> {$}" in {
    assertResult(Set("$").map(Term)) {
      localFollow(NonTerm("A"))
    }
  }

  it should "map B -> {first C}" in {
    assertResult(localFirst(NonTerm("C"))) {
      localFollow(NonTerm("B"))
    }
  }

  it should "map E -> {$}" in {
    assertResult(Set("$").map(Term)) {
      localFollow(NonTerm("E"))
    }
  }

  "parse" should "check a list of terms' validation" in {
    def buildTermList(str: String): List[Term] =
      str.toList.map(c => Term(c.toString))
    val sample = buildTermList("bcde")
    val sample2 = buildTermList("bce")
    val sample3 = buildTermList("ecb")
    val table = new LLTable(prods, algo, NonTerm("A"))
    val check = parse(table, _: List[Term])
    assert {
      check(sample) && check(sample2) && !check(sample3)
    }
  }
}
