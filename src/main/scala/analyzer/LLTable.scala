package analyzer

import scala.collection.mutable

class LLTable(productions: List[Production], algorithm: Algorithm,
              val startingSymbol: NonTerm) {
  private val table = mutable.Map.empty[NonTerm, mutable.Map[Term, Production]]
  private var isLL1Grammar = true
  import algorithm._
  productions.foreach { prod =>
    val firstSet = first(productions, prod.body)
    val terms =
      if (firstSet.contains(Empty.value))
        firstSet - Empty.value ++ follow(productions, prod.head, startingSymbol)
      else firstSet
    val tmap = table.getOrElseUpdate(prod.head, mutable.Map.empty)
    terms.foreach { term =>
      if (tmap.contains(term)) isLL1Grammar = false
      tmap.update(term, prod)
    }
  }
  // construction complete

  val isLL1Table: Boolean = isLL1Grammar
  lazy val nonTerms: List[NonTerm] = productions.map(_.head).distinct
  lazy val terms: List[Term] = productions.flatMap(_.body).collect {
    case t: Term if t != Empty.value => t
  }.distinct
  def get(nonTerm: NonTerm, term: Term): Option[Production] = {
    table.get(nonTerm).flatMap(_.get(term))
  }
}
