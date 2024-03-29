package objsets

class TweetSetSuite extends munit.FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6 = set1.incl(c).incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assertEquals(size(set1.filter(tw => tw.user == "a")), 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assertEquals(size(set5.filter(tw => tw.user == "a")), 1)
    }
  }

  test("filter: twenty on set5") {
    new TestSets {
      assertEquals(size(set5.filter(tw => tw.retweets == 20)), 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assertEquals(size(set4c.union(set4d)), 4)
    }
  }

  test("union: with empty set1") {
    new TestSets {
      assertEquals(size(set5.union(set1)), 4)
    }
  }

  test("union: with empty set5") {
    new TestSets {
      assertEquals(size(set1.union(set5)), 4)
    }
  }

  test("union: with two empty sets") {
    new TestSets {
      assertEquals(size(set1.union(set1)), 0)
    }
  }

  test("union: non-empty set with itself") {
    new TestSets {
      assertEquals(size(set2.union(set2)), size(set2))
    }
  }

  test("mostRetweeted: set2") {
    new TestSets {
      assert(set2.mostRetweeted.retweets == 20)
    }
  }

  test("mostRetweeted: set5") {
    new TestSets {
      assert(set5.mostRetweeted.retweets == 20)
    }
  }

  test("mostRetweeted: set6") {
    new TestSets {
      assert(set6.mostRetweeted.retweets == 9)
    }
  }

  test("descending: empty set1") {
    new TestSets {
      val result = set1.descendingByRetweet
      assert(result.isEmpty)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert((trends.head.user == "a" && trends.tail.head.user == "b") ||
        (trends.head.user == "b" && trends.tail.head.user == "a"))
    }
  }

  test("descending: set6") {
    new TestSets {
      val result = set6.descendingByRetweet
      assert(!result.isEmpty)
      assert(result.head.user == "d")
    }
  }

  import scala.concurrent.duration._
  override val munitTimeout = 10.seconds
}
