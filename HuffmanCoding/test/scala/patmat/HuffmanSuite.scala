package patmat

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t0 = Leaf('a', 3)
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = Fork(Leaf('y', 7),
                  Fork(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5), List('x', 'e', 't'), 9),
                  List('y', 'x', 'e', 't'),
                  16)
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
    new TestTrees {
      val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('y', 7))
      val result = List(t3)
      assertEquals(until(singleton, combine)(leaflist), result)
    }
  }

  test("createCodeTree with one char one time") {
    assertEquals(createCodeTree(List('a')), Leaf('a', 1))
  }

  test("createCodeTree with one char multiple times") {
    assertEquals(createCodeTree(List('b', 'b', 'b')), Leaf('b', 3))
  }

  test("createCodeTree with two chars") {
    new TestTrees {
      val charList = List('a', 'b', 'b', 'a', 'b')
      assertEquals(createCodeTree(charList), t1)
    }
  }

  test("createCodeTree complicated") {
    new TestTrees {
      val charList = List('y', 'e', 't', 't', 'x', 'e', 'y', 'y', 't', 'x', 'y', 'y', 'x', 'y', 'x', 'y')
      assertEquals(createCodeTree(charList), t3)
    }
  }

  test("decode on leaf gives empty list") {
    new TestTrees {
      assertEquals(decode(t0, List(0, 1, 0, 0)), List())
    }
  }

  test("decode on empty bit list") {
    new TestTrees {
      assertEquals(decode(t3, List()), List())
    }
  }

  test("decode two letter alphabet") {
    new TestTrees {
      val bitList = List(0, 1, 1, 0, 0, 0, 1)
      assertEquals(decode(t1, bitList), List('a', 'b', 'b', 'a', 'a', 'a', 'b'))
    }
  }

  test("decode three letter alphabet") {
    new TestTrees {
      val bitList = List(0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1)
      assertEquals(decode(t2, bitList), List('a', 'a', 'd', 'b', 'a', 'd', 'b'))
    }
  }

  test("decode complicated") {
    new TestTrees {
      val bitList = List(1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0)
      assertEquals(decode(t3, bitList), List('t', 'y', 'x', 'x', 'e', 't', 'y', 'x'))
    }
  }

  test("inList on empty list") {
    assert(!inList('a', List()))
  }

  test("inList on one element list") {
    val theList = List('a')
    assert(inList('a', theList))
    assert(!inList('b', theList))
  }

  test("inList on multiple elements") {
    val theList = List('a', 'b', 'a', 'c')
    assert(inList('a', theList))
    assert(inList('c', theList))
    assert(!inList('d', theList))
  }

  test("encodeChar on leaf") {
    new TestTrees {
      assertEquals(encodeChar('a', t0), List())
    }
  }

  test("encodeChar on simple fork") {
    new TestTrees {
      assertEquals(encodeChar('a', t1), List(0))
      assertEquals(encodeChar('b', t1), List(1))
    }
  }

  test("encodeChar on 2 level tree") {
    new TestTrees {
      assertEquals(encodeChar('b', t2), List(0, 1))
      assertEquals(encodeChar('d', t2), List(1))
    }
  }

  test("encodeChar on complex tree") {
    new TestTrees {
      assertEquals(encodeChar('y', t3), List(0))
      assertEquals(encodeChar('x', t3), List(1, 0))
      assertEquals(encodeChar('e', t3), List(1, 1, 0))
      assertEquals(encodeChar('t', t3), List(1, 1, 1))
    }
  }

  test("encode on leaf gives empty list") {
    new TestTrees {
      assertEquals(encode(t0)(List('a', 'b', 'c', 'a')), List())
    }
  }

  test("encode on empty char list") {
    new TestTrees {
      assertEquals(encode(t3)(List()), List())
    }
  }

  test("encode two letter alphabet") {
    new TestTrees {
      val charList = List('a', 'b', 'b', 'a', 'a', 'a', 'b')
      val bitList = List(0, 1, 1, 0, 0, 0, 1)
      assertEquals(encode(t1)(charList), bitList)
    }
  }

  test("encode three letter alphabet") {
    new TestTrees {
      val charList = List('a', 'a', 'd', 'b', 'a', 'd', 'b')
      val bitList = List(0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1)
      assertEquals(encode(t2)(charList), bitList)
    }
  }

  test("encode complicated") {
    new TestTrees {
      val charList = List('t', 'y', 'x', 'x', 'e', 't', 'y', 'x')
      val bitList = List(1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0)
      assertEquals(encode(t3)(charList), bitList)
    }
  }


  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
