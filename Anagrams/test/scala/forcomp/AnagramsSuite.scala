package forcomp

class AnagramsSuite extends munit.FunSuite:
  import Anagrams.*

  test("wordOccurrences: empty string") {
    assertEquals(wordOccurrences(""), List())
  }

  test("wordOccurrences: abcd (3pts)") {
    assertEquals(wordOccurrences("abcd"), List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: ZyXxXwW") {
    assertEquals(wordOccurrences("ZyXxXwW"), List(('w', 2), ('x', 3), ('y', 1), ('z', 1)))
  }

  test("wordOccurrences: Robert (3pts)") {
    assertEquals(wordOccurrences("Robert"), List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: empty sentence") {
    assertEquals(sentenceOccurrences(List()), List())
  }

  test("sentenceOccurrences: Matt") {
    assertEquals(sentenceOccurrences(List("Matt")), List(('a', 1), ('m', 1), ('t', 2)))
  }

  test("sentenceOccurrences: abcd e (5pts)") {
    assertEquals(sentenceOccurrences(List("abcd", "e")), List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: ZyXxXwW abcd") {
    assertEquals(sentenceOccurrences(List("ZyXxXwW", "abcd")),
      List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('w', 2), ('x', 3), ('y', 1), ('z', 1)))
  }


  test("dictionaryByOccurrences.get: eat (10pts)") {
    assertEquals(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet), Some(Set("ate", "eat", "tea")))
  }

  test("dictionaryByOccurrences.get: a") {
    assertEquals(dictionaryByOccurrences.get(List(('a', 1))).map(_.toSet), Some(Set("a")))
  }

  test("dictionaryByOccurrences.get: married") {
    assertEquals(dictionaryByOccurrences.get(List(('a', 1), ('d', 1), ('e', 1), ('i', 1), ('m', 1), ('r', 2))).map(_.toSet),
      Some(Set("married", "admirer")))
  }

  test("dictionaryByOccurrences.get: player") {
    assertEquals(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('l', 1), ('p', 1), ('r', 1), ('y', 1))).map(_.toSet),
      Some(Set("parley", "pearly", "player", "replay")))
  }

  test("wordAnagrams xxyyzz") {
    assertEquals(wordAnagrams("xxyyzz").toSet, Set())
  }

  test("wordAnagrams a") {
    assertEquals(wordAnagrams("a").toSet, Set("a"))
  }

  test("wordAnagrams married (2pts)") {
    assertEquals(wordAnagrams("married").toSet, Set("married", "admirer"))
  }

  test("wordAnagrams player (2pts)") {
    assertEquals(wordAnagrams("player").toSet, Set("parley", "pearly", "player", "replay"))
  }



  test("subtract: lard - r (10pts)") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assertEquals(subtract(lard, r), lad)
  }

  test("subtract: assessment - assess") {
    val assessment = List(('a', 1), ('e', 2), ('m', 1), ('n', 1), ('s', 4), ('t', 1))
    val assess = List(('a', 1), ('e', 1), ('s', 4))
    val ment = List(('e', 1), ('m', 1), ('n', 1), ('t', 1))
    assertEquals(subtract(assessment, assess), ment)
  }

  test("subtract with two empty lists") {
    assertEquals(subtract(List(), List()), List())
  }

  test("subtract: art - List()") {
    val art = List(('a', 1), ('r', 1), ('t', 1))
    assertEquals(subtract(art, List()), art)
  }

  test("subtractSlow: lard - r (10pts)") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assertEquals(subtractSlow(lard, r), lad)
  }

  test("subtractSlow: assessment - assess") {
    val assessment = List(('a', 1), ('e', 2), ('m', 1), ('n', 1), ('s', 4), ('t', 1))
    val assess = List(('a', 1), ('e', 1), ('s', 4))
    val ment = List(('e', 1), ('m', 1), ('n', 1), ('t', 1))
    assertEquals(subtractSlow(assessment, assess), ment)
  }

  test("subtractSlow with two empty lists") {
    assertEquals(subtractSlow(List(), List()), List())
  }

  test("subtractSlow: art - List()") {
    val art = List(('a', 1), ('r', 1), ('t', 1))
    assertEquals(subtractSlow(art, List()), art)
  }


  test("combinations: [] (8pts)") {
    assertEquals(combinations(Nil), List(Nil))
  }

  test("combinations: aaa") {
    val aaa = List(('a', 3))
    val aaacomb = List(List(), List(('a', 1)), List(('a', 2)), List(('a', 3)))
    assertEquals(combinations(aaa).toSet, aaacomb.toSet)
  }

  test("combinations: abba (8pts)") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assertEquals(combinations(abba).toSet, abbacomb.toSet)
  }


  test("sentence anagrams: [] (10pts)") {
    val sentence = List()
    assertEquals(sentenceAnagrams(sentence), List(Nil))
  }

  test("sentence anagrams: Linux rulez (10pts)") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assertEquals(sentenceAnagrams(sentence).toSet, anas.toSet)
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
