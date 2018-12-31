package analyzer

object GrammarScanner {
  val algorithm: Algorithm with LeftCommonFactorDetector with LeftRecursiveDetector =
    new Algorithm with LeftCommonFactorDetector with LeftRecursiveDetector
  import algorithm._

  def parseProductions(gramStr: String): List[Production] = {
    val lines = gramStr.split('\n').filterNot(s => s.isEmpty || s.forall(_.isWhitespace))
    val listProds = lines.map(parseProduction).toList
    construct(listProds)
  }

  protected [analyzer] def parseProduction(prod: String): (String, List[String]) = {
    val splitStr = prod.split("->")
    if (splitStr.size != 2) throw new RuntimeException(s"syntax error at ${splitStr.toList}: $prod")

    def splitAndFilter(str: String) = str.split("[\t ]").filter(_.nonEmpty)
    val head = splitAndFilter(splitStr(0))
    val body = splitAndFilter(splitStr(1))

    if (head.length != 1) throw new RuntimeException(s"syntax error in head at $prod")
    if (body.isEmpty) throw new RuntimeException(s"syntax error in body at $prod")

    (head(0), body.map(s => if (s == "empty") "" else s).toList)
  }
}
