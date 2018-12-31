package analyzer

trait Symbol {
  def str: String
}
case class Term(str: String) extends Symbol
case class NonTerm(str: String) extends Symbol
object Empty { val value = Term("`empty`") }
object EOS { val value = Term("$") }

case class Production(head: NonTerm, body: List[Symbol])

class ParsingListener {
  def onStepping(symbols: Seq[Symbol], terms: Seq[Term]): Unit = {}
  def onDeriving(production: Production): Unit = {}
  def onMatching(term: Term): Unit = {}
}