package analyzer

import org.scalatest.FlatSpec

class LLTableTest extends FlatSpec {
  private val algo = new Algorithm
  import algo._
  private val prods = construct(List(
    /*0*/ "E" -> List("T", "E\'"),
    /*1*/ "E\'" -> List("+", "T", "E\'"),
    /*2*/ "E\'" -> List(""),
    /*3*/ "T" -> List("F", "T\'"),
    /*4*/ "T\'" -> List("*", "F", "T\'"),
    /*5*/ "T\'" -> List(""),
    /*6*/ "F" -> List("id"),
    /*7*/ "F" -> List("(", "E", ")"),
  ))
  private val table = new LLTable(prods, algo, NonTerm("E"))

  "LLTable" should "has necessary productions" in {
    assertResult(prods(0)) {
      table.get(NonTerm("E"), Term("id")).get
    }
    assertResult(prods(0)) {
      table.get(NonTerm("E"), Term("(")).get
    }
    assertResult(prods(1)) {
      table.get(NonTerm("E\'"), Term("+")).get
    }
    assertResult(prods(2)) {
      table.get(NonTerm("E\'"), Term(")")).get
    }
    assertResult(prods(2)) {
      table.get(NonTerm("E\'"), Term("$")).get
    }
    assertResult(prods(2)) {
      table.get(NonTerm("E\'"), Term("$")).get
    }
    assertResult(prods(3)) {
      table.get(NonTerm("T"), Term("(")).get
    }
    assertResult(prods(3)) {
      table.get(NonTerm("T"), Term("id")).get
    }
    assertResult(prods(5)) {
      table.get(NonTerm("T\'"), Term("+")).get
    }
    assertResult(prods(5)) {
      table.get(NonTerm("T\'"), Term(")")).get
    }
    assertResult(prods(5)) {
      table.get(NonTerm("T\'"), Term("$")).get
    }
    assertResult(prods(4)) {
      table.get(NonTerm("T\'"), Term("*")).get
    }
    assertResult(prods(6)) {
      table.get(NonTerm("F"), Term("id")).get
    }
    assertResult(prods(7)) {
      table.get(NonTerm("F"), Term("(")).get
    }

  }

  it should "report isLL1Table as true for prod" in {
    assert(table.isLL1Table)
  }

  "nonTerms" should "return all nonterminals" in {
    assertResult(Set("E", "E\'", "T", "T\'", "F").map(NonTerm)) {
      table.nonTerms.toSet
    }
  }

  "terms" should "return all terminals" in {
    assertResult(Set("+", "*", "id", "(", ")").map(Term)) {
      table.terms.toSet
    }
  }
}
