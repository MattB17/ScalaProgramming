package patmat

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t0 = Leaf('a', 3)
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  test("weight of a leaf") {
    new TestTrees {
      assertEquals(weight(t0), 3)
    }
  }

  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
      assertEquals(weight(t2), 9)
  }

  test ("chars of a leaf") {
    new TestTrees {
      assertEquals(chars(t0), List('a'))
    }
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t1), List('a', 'b'))
      assertEquals(chars(t2), List('a','b','d'))
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times on an empty list") {
    assertEquals(times(List()), List())
  }

  test("times on a 1 element list") {
    assertEquals(times(List('a')), List(('a', 1)))
  }

  test("times on multiple element list") {
    assertEquals(times(List('a', 'b', 'c', 'a', 'a', 'b')).sortBy((x, y) => x),
                 List(('a', 3), ('b', 2), ('c', 1)))
  }

  test("makeOrderedLeafList from no frequencies") {
    val emptyFreqs: List[(Char, Int)] = List()
    val emptyLeaves: List[Leaf] = List()
    assertEquals(makeOrderedLeafList(emptyFreqs), emptyLeaves)
  }

  test("makeOrderedLeafList from 1 frequency") {
    val oneFreq: List[(Char, Int)] = List(('a', 5))
    val oneLeaf: List[Leaf] = List(Leaf('a', 5))
    assertEquals(makeOrderedLeafList(oneFreq), oneLeaf)
  }

  test("makeOrderedLeafList multiple frequencies") {
    val multiFreqs: List[(Char, Int)] = List(('b', 2), ('a', 3), ('c', 1))
    val multiLeaf: List[Leaf] = List(Leaf('c', 1), Leaf('b', 2), Leaf('a', 3))
    assertEquals(makeOrderedLeafList(multiFreqs), multiLeaf)
  }


  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton with empty list") {
    assert(!singleton(List()))
  }

  test("singleton with single leaf") {
    new TestTrees {
      assert(singleton(List(t0)))
    }
  }

  test("singleton with single Fork") {
    new TestTrees {
      assert(singleton(List(t1)))
    }
  }

  test("singleton with list of multiple CodeTrees") {
    new TestTrees {
      val treeList: List[CodeTree] = List(t0, t1, t2)
      assert(!singleton(treeList))
    }
  }

  test("combine on empty list") {
    val treeList: List[CodeTree] = List()
    assertEquals(combine(treeList), treeList)
  }

  test("combine on 1 tree") {
    new TestTrees {
      val treeList: List[CodeTree] = List(t1)
      assertEquals(combine(treeList), treeList)
    }
  }

  test("combine on fork and leaf") {
    new TestTrees {
      val treeList: List[CodeTree] = List(Leaf('d', 4), t1)
      val expectedList: List[CodeTree] = List(Fork(Leaf('d', 4), t1, List('d', 'a', 'b'), 9))
      assertEquals(combine(treeList), expectedList)
    }
  }

  test("combine inserts in sorted order") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('y', 7))
    assertEquals(combine(leaflist), List(Leaf('x', 4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'), 5), Leaf('y', 7)))
  }

  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until on 1 tree") {
    new TestTrees {
      val treeList: List[CodeTree] = List(t1)
      assertEquals(until(singleton, combine)(treeList), treeList)
    }
  }

  test("until on fork and leaf") {
    new TestTrees {
      val treeList: List[CodeTree] = List(Leaf('d', 4), t1)
      val expectedList: List[CodeTree] = List(Fork(Leaf('d', 4), t1, List('d', 'a', 'b'), 9))
      assertEquals(until(singleton, combine)(treeList), expectedList)
    }
  }

  test("until inserts in sorted order") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('y', 7))
    val result = List(
      Fork(Leaf('y', 7),
           Fork(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5), List('x', 'e', 't'), 9),
           List('y', 'x', 'e', 't'),
           16))
    assertEquals(until(singleton, combine)(leaflist), result)
  }


  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
