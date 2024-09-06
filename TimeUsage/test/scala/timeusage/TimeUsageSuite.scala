package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row}
import scala.util.Random
import scala.util.Properties.isWin
import org.apache.spark.sql.functions.col

class TimeUsageSuite extends munit.FunSuite:
  import TimeUsage.*

  test("test row method") {
    val result = row(List("hello", "1", "7.5"))
    assertEquals(result, Row.fromSeq(List("hello", 1.0, 7.5)))
  }

  test("row with 1 element") {
    val result = row(List("test"))
    assertEquals(result, Row.fromSeq(List("test")))
  }

  test("classifiedColumns test t18") {
    val colList = List("t18", "t181", "t1801", "t1803", "t1805", "t1807")
    val expectedPrimary = List(col("t1801"), col("t1803"))
    val expectedWorking = List(col("t1805"))
    val expectedOther = List(col("t18"), col("t181"), col("t1807"))
    val result = classifiedColumns(colList)
    assertEquals(result._1, expectedPrimary)
    assertEquals(result._2, expectedWorking)
    assertEquals(result._3, expectedOther)
  }

  test("classifiedColumns long list") {
    val colList = List("t180145", "t13", "t25", "t01", "t0756", "t0456", "t1111", "t0513")
    val expectedPrimary = List(col("t180145"), col("t01"), col("t1111"))
    val expectedWorking = List(col("t0513"))
    val expectedOther = List(col("t13"), col("t0756"), col("t0456"))
    val result = classifiedColumns(colList)
    assertEquals(result._1, expectedPrimary)
    assertEquals(result._2, expectedWorking)
    assertEquals(result._3, expectedOther)
  }
