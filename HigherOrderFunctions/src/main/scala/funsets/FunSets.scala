package funsets

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): FunSet = (x: Int) => x == elem


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet = (x: Int) => s(x) || t(x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = (x: Int) => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet = (x: Int) => s(x) && !t(x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = (x: Int) => s(x) && p(x)


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   *
   * The idea is as follows: if there does not exist an element
   * of `s` that satisfies `p` then `forall(s, !p)` will return true
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = {
    !forall(s, (x: Int) => !p(x))
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   *
   * The idea is the characteristic function of the mapped set returns true
   * for a value `elem` if there is a value `x` in the original set such that
   * `f(x) = elem` that is `x` maps to `elem` under `f`
   */
  def map(s: FunSet, f: Int => Int): FunSet = {
    (elem: Int) => exists(s, (x: Int) => f(x) == elem)
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit = {
    println(toString(s))
  }
}

object FunSets extends FunSets
