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


  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
