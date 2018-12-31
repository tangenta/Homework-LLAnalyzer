package analyzer

import scala.collection.mutable

trait LeftRecursiveDetector extends Algorithm {
  override def first(prods: List[Production], symbol: Symbol): Set[Term] = {
    if (!detecting) super.first(prods, symbol)
    else {
      if (route.contains(symbol)) {
        recursiveFlag = true
        Set.empty  // prevent stack overflow
      }
      else {
        if (symbol.isInstanceOf[NonTerm]) route += symbol
        super.first(prods, symbol)
      }
    }
  }

  private val route = mutable.Set.empty[Symbol]
  private var recursiveFlag = false
  private var detecting = false

  def isLeftRecursive(prods: List[Production]): Boolean = {
    detecting = true
    val result = prods.exists { prod =>
      route.clear()
      recursiveFlag = false
      first(prods, prod.head)
      recursiveFlag
    }
    detecting = false
    result
  }

  def eliminateLeftRecursion(prods: List[Production]): List[Production] = {
    type ProdGroup = List[Production]
    type PGroupList = List[ProdGroup]
    def helper(result: PGroupList, rest: PGroupList): (PGroupList, PGroupList) = {
      if (rest.isEmpty) (result, rest)
      else {
        val tmp = eliminate(rest.head)
        helper(tmp :: result, rest.tail.map(rest => substitute(tmp.toSet, rest.toSet)))   // fixme: need optimize
      }
    }
    helper(List.empty, orderedGrouping(prods))._1.reverse.flatten
  }

  /**
    * Group a list of productions by their heads. Return a list of production sets.
    *
    * Guarantee that the order of left recursion elimination correspond to
    * the order of appearance of nonterminal in original productions.
    * @param prods a list of productions
    * @return a list of production sets
    */
  protected [analyzer] def orderedGrouping(prods: List[Production]): List[List[Production]] = {
    val prodGroups = prods.groupBy(_.head).values.toList
    val sortedKeys = prods.map(_.head.str).distinct.zipWithIndex.toMap
    prodGroups.sortBy(ps => sortedKeys(ps.head.head.str))
  }

  /**
    * Eliminate direct left recursion in a set of productions.
    *
    * Assumed that the heads of productions are the same.
    * @param prods a set of productions
    * @return another set of productions
    */
  private def eliminate(prods: List[Production]): List[Production] = {
    val isRecursive = (p: Production) => p.head == p.body.head
    val partition = prods.partition(isRecursive)
    if (partition._1.isEmpty) partition._2
    else {
      val newNonTerm = NonTerm(prods.head.head.str + '\'')
       partition._2.map(p =>
        Production(p.head, p.body ++ List(newNonTerm))
      ) ++ partition._1.map(p =>
         Production(newNonTerm, p.body.tail ++ List(newNonTerm))
       ) ++ List(
        Production(newNonTerm, List(Empty.value))
      )
    }
  }

  /**
    * Substitute each production which has the form of {{{S -> A b}}}
    * with {{{A -> B}}} into {{{S -> B b}}}
    * @param source a set of productions to substitute
    * @param dest a set of productions to be substituted
    * @return
    */
  private def substitute(source: Set[Production], dest: Set[Production]): List[Production] = {
    dest.flatMap { prod =>
      source.map(srcProd =>
        if (prod.body.head == srcProd.head)
          Production(prod.head, srcProd.body ++ prod.body.tail)
        else prod
      )
    }.toList
  }
}
