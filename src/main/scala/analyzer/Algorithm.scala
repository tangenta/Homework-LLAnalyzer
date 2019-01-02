package analyzer


class Algorithm {
  def construct(prods: List[(String, List[String])]): List[Production] = {
    val nt = prods.map(_._1).toSet
    prods.map(elem => Production(NonTerm(elem._1), elem._2.map(e =>
      if (nt.contains(e)) NonTerm(e)
      else if (e.isEmpty) Empty.value
      else Term(e)
    )))
  }

  def stringify(prods: List[Production]): String = {
    def extractedStr(symbol: Symbol): String = symbol match {
      case t: NonTerm => t.str
      case n: Term => n.str
    }
    prods.map(p =>
      p.head.str + " -> " + p.body.map(extractedStr).mkString(" ")
    ).mkString("\n")
  }

  def stringify(table: LLTable): String = {
    def formalize(strs: List[String]): List[String] = {
      val maxLen = strs.maxBy(_.length).length + 2
      val column = strs.map{ s =>
        val pp = (maxLen - s.length) / 2  // padding prefix
        val ps = maxLen - pp - s.length   // padding suffix
        "|" + " " * pp + s + " " * ps + "|"
      }
      val splitter = "+" + "-" * maxLen + "+"
      splitter :: column.head :: splitter :: column.tail ::: List(splitter)
    }
    def concat(columns: List[List[String]]): String = {
      columns.reduceLeft { (acc, column) =>
        acc.zip(column).map { case (a, c) => a + c.tail }
      }.mkString("\n")
    }
    val strList = formalize("" :: table.nonTerms.map(_.str)) ::  // first column -- nonterminal
      (EOS.value :: table.terms).map { term =>
        val columnTail = table.nonTerms.map { nonTerm =>
          val optionProd = table.get(nonTerm, term)
          optionProd.map(_.body.map(_.str).mkString(" ")).
            getOrElse("")
        }
        formalize(term.str :: columnTail)
      }
    concat(strList)
  }

  // prone to stack overflow when handling left recursive grammar
  def first(prods: List[Production], symbol: Symbol): Set[Term] = symbol match {
    case term: Term => Set(term)
    case nonTerm: NonTerm => prods.toSet.withFilter(_.head == nonTerm).flatMap {
      prod => first(prods, prod.body)
    }
  }


  def first(prods: List[Production], symbols: List[Symbol]): Set[Term] = {
    var skipRest = false
    val result = symbols.foldLeft(Set.empty[Term]) { (acc, elem) =>
      if (skipRest) acc else {
        val elemFirst = first(prods, elem)
        skipRest = !elemFirst.contains(Empty.value)
        acc ++ elemFirst - Empty.value
      }
    }
    if (skipRest) result else result + Empty.value
  }


  def follow(prods: List[Production], symbol: NonTerm, startingSymbol: NonTerm): Set[Term] = {
    follow1(prods, symbol, Set.empty, startingSymbol)
  }

  private def follow1(prods: List[Production], symbol: NonTerm,
                      route: Set[NonTerm], startingSymbol: NonTerm): Set[Term] = {
    def searchFollow(prod: Production): Set[Term] = {
      val firstArgs = allFollowSymbols(prod, symbol)
      val occurLastPos = firstArgs.contains(List.empty)
      val firstSet = (firstArgs - List.empty).flatMap(first(prods, _))
      val containsEmpty = firstSet.contains(Empty.value)
      if (occurLastPos || containsEmpty)
        firstSet ++ follow1(prods, prod.head, route + symbol, startingSymbol)
      else firstSet
    }

    if (route.contains(symbol)) Set.empty
    else {
      val result = prods.toSet.flatMap(searchFollow)
      if (symbol == startingSymbol) result + EOS.value else result
    }
  }


  /** Find all the symbols which follow a specific symbol in a production.
    *
    * @example {{{
    * prod: A -> B C B D
    * symbol: "B"
    * return: Set(List(C,B,D), List(D))
    * }}}
    * @param prod a production
    * @param symbol the specific symbol
    * @return a set of symbol list
    */
  private def allFollowSymbols(prod: Production, symbol: Symbol): Set[List[Symbol]] = {
    prod.body.foldLeft((prod.body, Set.empty[List[Symbol]])) {
      case ((rest, result), sym) =>
        if (sym == symbol) (rest.tail, result + rest.tail)
        else (rest.tail, result)
    }._2
  }

  def parse(table: LLTable, terms: List[Term],
               parsingListener: ParsingListener = new ParsingListener): Boolean = {
    def helper(symbols: List[Symbol], restTerms: List[Term]): Boolean = {
      parsingListener.onStepping(symbols, restTerms)
      (symbols, restTerms) match {
        case (s, Nil) => s.isEmpty
        case (List(Empty.value, _), r) => helper(symbols.tail, r)
        case (List(t: Term, _), r) if t == r.head =>
          parsingListener.onMatching(t)
          helper(symbols.tail, r.tail)
        case (List(nt: NonTerm, _), r) => table.get(nt, r.head).exists { p =>
          parsingListener.onDeriving(p)
          helper(p.body ::: symbols.tail, r)
        }
        case _ => false
      }
    }

    helper(List(table.startingSymbol, EOS.value), terms  ++ List(EOS.value))
  }

}
