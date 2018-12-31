package analyzer

object Adapter {
  private val plainAlgo = new Algorithm
  private val lrdAlgo = new Algorithm with LeftRecursiveDetector
  private val lcfdAlgo = new Algorithm with LeftCommonFactorDetector

  def isLeftRecursive(prods: List[Production]): Boolean =
    lrdAlgo.isLeftRecursive(prods)

  def eliminateLeftRecursion(prods: List[Production]): List[Production] = {
    lrdAlgo.eliminateLeftRecursion(prods)
  }

  def hasLeftCommonFactor(prods: List[Production]): Boolean =
    lcfdAlgo.hasLeftCommonFactor(prods)

  def eliminateLeftFactor(prods: List[Production]): List[Production] =
    lcfdAlgo.eliminateLeftCommonFactor(prods)

  def stringify(prods: List[Production]): String = {
    plainAlgo.stringify(prods)
  }

  def stringify(table: LLTable): String =
    plainAlgo.stringify(table)

  def first(prods: List[Production], symbol: Symbol): Set[Term] =
    plainAlgo.first(prods, symbol)

  def follow(prods: List[Production], symbol: NonTerm, startingSymbol: NonTerm): Set[Term] =
    plainAlgo.follow(prods, symbol, startingSymbol)

  def parse(table: LLTable, terms: List[Term]): String = {
    var result: String = ""
    val parsingListener = new ParsingListener {
      override def onStepping(symbols: Seq[Symbol], terms: Seq[Term]): Unit = {
        result += symbols.map(_.str).mkString(" ") + " | " +
          terms.map(_.str).mkString(" ") + "\n"
      }
    }
    val success = plainAlgo.parse(table, terms, parsingListener)
    if (success) result else null
  }

  def buildTerms(str: String): List[Term] = {
    str.split("[\t ]").filter(_.nonEmpty).map(Term).toList
  }
}
