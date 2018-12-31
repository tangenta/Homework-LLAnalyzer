package analyzer


trait LeftCommonFactorDetector extends Algorithm {
  def hasLeftCommonFactor(prods: List[Production]): Boolean = {
    prods.groupBy(_.head).values.exists { prod =>
      val firstSymbols = prod.map(_.body.head)
      firstSymbols.distinct.size != firstSymbols.size  // has duplicate symbols
    }
  }

  def eliminateLeftCommonFactor(prods: List[Production]): List[Production] = {
    prods.groupBy(_.head).values.flatMap(splitRecursive(_, primes2Append = 1)).toList
  }

  protected [analyzer] def splitRecursive(prods: List[Production], primes2Append: Int): List[Production] = {
    val tp = longestCommonPrefixProductions(prods)
    val len = tp._1.size
    val lcpp = tp._2
    if (lcpp.isEmpty) prods
    else {
      val sample = lcpp.head
      val newNonTerm = NonTerm(sample.head.str + "\'" * primes2Append)
      splitRecursive(
        Production(sample.head, sample.body.take(len) ++ List(newNonTerm)) ::
          prods.filterNot(lcpp.contains)
        , primes2Append + 1) ::: lcpp.map(p => Production(newNonTerm, p.body.drop(len))).toList
    }
  }

  protected [analyzer] def longestCommonPrefixProductions(prods: List[Production]): (List[Symbol], Set[Production]) = {
    val sortedProds = prods.sortBy(p => p.body.map(_.str).mkString)
    sortedProds.zip(sortedProds.tail).foldLeft((List.empty[Symbol], Set.empty[Production])) {
      case ((ml, res), (l, r)) =>
        val len = l.body.zip(r.body).takeWhile(z => z._1 == z._2).size
        if (len > ml.size)
          (l.body.take(len), Set.empty + l + r)
        else if (len == ml.size && l.body.take(len) == ml && len != 0)
          (ml, res + l + r)
        else (ml, res)
    }
  }

}

