package stackoverflow

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext.*
import org.apache.spark.rdd.RDD
import java.io.File
import scala.io.{ Codec, Source }
import scala.util.Properties.isWin

object StackOverflowSuite:
  val conf: SparkConf = new SparkConf().setMaster("local[2]").setAppName("StackOverflow")
  val sc: SparkContext = new SparkContext(conf)

class StackOverflowSuite extends munit.FunSuite:
  import StackOverflowSuite.*


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  trait StackPostings {
    val q0 = Posting(1, 0, None, None, 7, Some("python,testing"))
    val q1 = Posting(1, 1, Some(5), None, 15, Some("scala"))
    val q2 = Posting(1, 2, None, None, 0, Some("coding"))

    val a3 = Posting(2, 3, None, Some(0), 3, Some("mockito"))
    val a4 = Posting(2, 4, None, Some(1), 2, Some("scala,recursion"))
    val a5 = Posting(2, 5, None, Some(1), 17, Some("scala,dynamic programming"))
    val a6 = Posting(2, 6, None, Some(1), 105, Some("scala,bit manipulation"))
    val a7 = Posting(2, 7, None, Some(10), 0, None)

    val emptyPosts: RDD[Posting] = sc.parallelize(List())
    val questions = sc.parallelize(List(q0, q1, q2))
    val answers = sc.parallelize(List(a3, a4, a5, a6, a7))

    val posts = List(q0, q1, q2, a3, a4, a5, a6, a7)
    val postsRdd = sc.parallelize(posts)
  }

  test("groupedPostings with no postings") {
    new StackPostings {
      assert(testObject.groupedPostings(emptyPosts).isEmpty())
    }
  }

  test("groupedPostings with only questions") {
    new StackPostings {
      assert(testObject.groupedPostings(questions).isEmpty())
    }
  }

  test("groupedPostings with only answers") {
    new StackPostings {
      assert(testObject.groupedPostings(answers).isEmpty())
    }
  }

  test("groupedPosting questions and answers") {
    new StackPostings {
      val result = testObject.groupedPostings(postsRdd).collect()
      assert(result.length == 2)
      assert(result(0)._2.toList.length == 1)
      assert(result(1)._2.toList.length == 3)
    }
  }


  import scala.concurrent.duration.given
  override val munitTimeout = 300.seconds
