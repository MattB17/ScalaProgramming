package timeusage

import org.apache.spark.sql.*
import org.apache.spark.sql.types.*
import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.expressions.Aggregator

import scala.util.Properties.isWin

/** Main class */
object TimeUsage extends TimeUsageInterface:
  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions.*

  // Reduce Spark logging verbosity
  Logger.getLogger("org.apache.spark").setLevel(Level.ERROR)
  if isWin then System.setProperty("hadoop.home.dir", System.getProperty("user.dir") + "\\winutils\\hadoop-3.3.1")

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Time Usage")
      .master("local")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits.{StringToColumn, localSeqToDatasetHolder}
  import scala3encoders.given

  /** Main function */
  def main(args: Array[String]): Unit =
    timeUsageByLifePeriod()
    spark.close()

  def timeUsageByLifePeriod(): Unit =
    val (columns, initDf) = read("src/main/resources/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    val finalDf = timeUsageGroupedTyped(timeUsageSummaryTyped(summaryDf))
    finalDf.show()

  /** @return The read DataFrame along with its column names. */
  def read(path: String): (List[String], DataFrame) =
    val df = spark.read.options(Map("header" -> "true", "inferSchema" -> "true")).csv(path)
    (df.schema.fields.map(_.name).toList, df)

  /** @return An RDD Row compatible with the schema produced by `dfSchema`
    * @param line Raw fields
    */
  def row(line: List[String]): Row =
    Row.fromSeq(line.head :: line.tail.map(field => field.toDouble))

  /** @return The initial data frame columns partitioned in three groups: primary needs (sleeping, eating, etc.),
    *         work and other (leisure activities)
    *
    * @see https://www.kaggle.com/bls/american-time-use-survey
    *
    * The dataset contains the daily time (in minutes) people spent in various activities. For instance, the column
    * “t010101” contains the time spent sleeping, the column “t110101” contains the time spent eating and drinking, etc.
    *
    * This method groups related columns together:
    * 1. “primary needs” activities (sleeping, eating, etc.). These are the columns starting with “t01”, “t03”, “t11”,
    *    “t1801” and “t1803”.
    * 2. working activities. These are the columns starting with “t05” and “t1805”.
    * 3. other activities (leisure). These are the columns starting with “t02”, “t04”, “t06”, “t07”, “t08”, “t09”,
    *    “t10”, “t12”, “t13”, “t14”, “t15”, “t16” and “t18” (those which are not part of the previous groups only).
    */
  def classifiedColumns(columnNames: List[String]): (List[Column], List[Column], List[Column]) = {
    val primaryListSimple = List("t01", "t03", "t11")
    val primaryListLong = List("t1801", "t1803")
    val workingListSimple = List("t05")
    val workingListLong = List("t1805")
    val otherListSimple = List("t02", "t04", "t06", "t07", "t08", "t09", "t10", "t12", "t13", "t14", "t15", "t16")
    val otherListLong = List("t18")


    val primaryNeedsCols = columnNames
      .filter(c => (primaryListSimple ::: primaryListLong).exists(colPrefix => c.startsWith(colPrefix)))
      .map(c => functions.col(c))
    val workingCols = columnNames
      .filter(c => (workingListSimple ::: workingListLong).exists(colPrefix => c.startsWith(colPrefix)))
      .map(c => functions.col(c))
    val otherCols = columnNames
      .filter(c => otherListSimple.exists(colPrefix => c.startsWith(colPrefix)) ||
        (otherListLong.exists(colPrefix => c.startsWith(colPrefix)) &&
          !(primaryListLong ::: workingListLong).exists(colPrefix => c.startsWith(colPrefix))))
      .map(c => functions.col(c))
    (primaryNeedsCols, workingCols, otherCols)
  }


  /** @return a projection of the initial DataFrame such that all columns containing hours spent on primary needs
    *         are summed together in a single column (and same for work and leisure). The “teage” column is also
    *         projected to three values: "young", "active", "elder".
    *
    * @param primaryNeedsColumns List of columns containing time spent on “primary needs”
    * @param workColumns List of columns containing time spent working
    * @param otherColumns List of columns containing time spent doing other activities
    * @param df DataFrame whose schema matches the given column lists
    *
    * This methods builds an intermediate DataFrame that sums up all the columns of each group of activity into
    * a single column.
    *
    * The resulting DataFrame should have the following columns:
    * - working: value computed from the “telfs” column of the given DataFrame:
    *   - "working" if 1 <= telfs < 3
    *   - "not working" otherwise
    * - sex: value computed from the “tesex” column of the given DataFrame:
    *   - "male" if tesex = 1, "female" otherwise
    * - age: value computed from the “teage” column of the given DataFrame:
    *   - "young" if 15 <= teage <= 22,
    *   - "active" if 23 <= teage <= 55,
    *   - "elder" otherwise
    * - primaryNeeds: sum of all the `primaryNeedsColumns`, in hours
    * - work: sum of all the `workColumns`, in hours
    * - other: sum of all the `otherColumns`, in hours
    *
    * Finally, the resulting DataFrame should exclude people that are not employable (ie telfs = 5).
    *
    * Note that the initial DataFrame contains time in ''minutes''. You have to convert it into ''hours''.
    */
  def timeUsageSummary(
    primaryNeedsColumns: List[Column],
    workColumns: List[Column],
    otherColumns: List[Column],
    df: DataFrame
  ): DataFrame = {
    val workingStatusProjection: Column = functions.when(
        col("telfs").geq(1) && col("telfs").lt(3), "working")
      .otherwise("not working")
      .as("working")
    val sexProjection: Column = functions.when(col("tesex").equalTo(1), "male")
      .otherwise("female")
      .as("sex")
    val ageProjection: Column = functions.when(
        col("teage").geq(15) && col("teage").leq(22), "young")
      .when(col("teage").geq(23) && col("teage").leq(55), "active")
      .otherwise("elder")
      .as("age")

    val primaryNeedsProjection: Column = (primaryNeedsColumns.reduce(_ + _) / 60).as("primaryNeeds")
    val workProjection: Column = (workColumns.reduce(_ + _) / 60).as("work")
    val otherProjection: Column = (otherColumns.reduce(_ + _) / 60).as("other")
    df
      .select(workingStatusProjection, sexProjection, ageProjection, primaryNeedsProjection, workProjection, otherProjection)
      .where($"telfs" <= 4)
  }

  /** @return the average daily time (in hours) spent in primary needs, working or leisure, grouped by the different
    *         ages of life (young, active or elder), sex and working status.
    * @param summed DataFrame returned by `timeUsageSumByClass`
    *
    * The resulting DataFrame should have the following columns:
    * - working: the “working” column of the `summed` DataFrame,
    * - sex: the “sex” column of the `summed` DataFrame,
    * - age: the “age” column of the `summed` DataFrame,
    * - primaryNeeds: the average value of the “primaryNeeds” columns of all the people that have the same working
    *   status, sex and age, rounded with a scale of 1 (using the `round` function),
    * - work: the average value of the “work” columns of all the people that have the same working status, sex
    *   and age, rounded with a scale of 1 (using the `round` function),
    * - other: the average value of the “other” columns all the people that have the same working status, sex and
    *   age, rounded with a scale of 1 (using the `round` function).
    *
    * Finally, the resulting DataFrame should be sorted by working status, sex and age.
    */
  def timeUsageGrouped(summed: DataFrame): DataFrame = {
    val primaryCol = "primaryNeeds"
    val workCol = "work"
    val otherCol = "other"

    summed
      .groupBy("working", "sex", "age")
      .agg(
        functions.round(avg(primaryCol), 1).as(primaryCol),
        functions.round(avg(workCol), 1).as(workCol),
        functions.round(avg(otherCol), 1).as(otherCol))
      .orderBy("working", "sex", "age")
  }

  /**
    * @return Same as `timeUsageGrouped`, but using a plain SQL query instead
    * @param summed DataFrame returned by `timeUsageSumByClass`
    */
  def timeUsageGroupedSql(summed: DataFrame): DataFrame =
    val viewName = s"summed"
    summed.createOrReplaceTempView(viewName)
    spark.sql(timeUsageGroupedSqlQuery(viewName))

  /** @return SQL query equivalent to the transformation implemented in `timeUsageGrouped`
    * @param viewName Name of the SQL view to use
    */
  def timeUsageGroupedSqlQuery(viewName: String): String = {
    """
      |SELECT
      |  working,
      |  sex,
      |  age,
      |  ROUND(AVG(primaryNeeds), 1) AS primaryNeeds,
      |  ROUND(AVG(work), 1) AS work,
      |  ROUND(AVG(other), 1) AS other
      |FROM """.stripMargin + viewName +
      """
        |GROUP BY working, age, sex
        |ORDER BY working, age, sex""".stripMargin
  }

  /**
    * @return A `Dataset[TimeUsageRow]` from the “untyped” `DataFrame`
    * @param timeUsageSummaryDf `DataFrame` returned by the `timeUsageSummary` method
    *
    * Hint: you should use the `getAs` method of `Row` to look up columns and
    * cast them at the same time.
    */
  def timeUsageSummaryTyped(timeUsageSummaryDf: DataFrame): Dataset[TimeUsageRow] = {
    timeUsageSummaryDf.map(r => TimeUsageRow(
      r.getAs[String]("working"),
      r.getAs[String]("sex"),
      r.getAs[String]("age"),
      r.getAs[Double]("primaryNeeds"),
      r.getAs[Double]("work"),
      r.getAs[Double]("other")
    ))
  }

  /**
    * @return Same as `timeUsageGrouped`, but using the typed API when possible
    * @param summed Dataset returned by the `timeUsageSummaryTyped` method
    *
    * Note that, though they have the same type (`Dataset[TimeUsageRow]`), the input
    * dataset contains one element per respondent, whereas the resulting dataset
    * contains one element per group (whose time spent on each activity kind has
    * been aggregated).
    *
    * Hint: you should use the `groupByKey` and `avg` methods.
    */
  def timeUsageGroupedTyped(summed: Dataset[TimeUsageRow]): Dataset[TimeUsageRow] = {
    val primaryNeedsAggregator = new Aggregator[TimeUsageRow, (Double, Int), Double] {
      def zero: (Double, Int) = (0.0, 0)

      def reduce(curr: (Double, Int), record: TimeUsageRow): (Double, Int) = {
        (curr._1 + record.primaryNeeds, curr._2 + 1)
      }

      def merge(curr1: (Double, Int), curr2: (Double, Int)): (Double, Int) = {
        (curr1._1 + curr2._1, curr1._2 + curr2._2)
      }

      def finish(r: (Double, Int)): Double = "%.1f".format(r._1 / r._2).toDouble

      override def bufferEncoder: Encoder[(Double, Int)] = Encoders.tuple(Encoders.scalaDouble, Encoders.scalaInt)

      override def outputEncoder: Encoder[Double] = Encoders.scalaDouble
    }.toColumn
    val workAggregator = new Aggregator[TimeUsageRow, (Double, Int), Double] {
      def zero: (Double, Int) = (0.0, 0)

      def reduce(curr: (Double, Int), record: TimeUsageRow): (Double, Int) = {
        (curr._1 + record.work, curr._2 + 1)
      }

      def merge(curr1: (Double, Int), curr2: (Double, Int)): (Double, Int) = {
        (curr1._1 + curr2._1, curr1._2 + curr2._2)
      }

      def finish(r: (Double, Int)): Double = "%.1f".format(r._1 / r._2).toDouble

      override def bufferEncoder: Encoder[(Double, Int)] = Encoders.tuple(Encoders.scalaDouble, Encoders.scalaInt)

      override def outputEncoder: Encoder[Double] = Encoders.scalaDouble
    }.toColumn

    val otherAggregator = new Aggregator[TimeUsageRow, (Double, Int), Double] {
      def zero: (Double, Int) = (0.0, 0)

      def reduce(curr: (Double, Int), record: TimeUsageRow): (Double, Int) = {
        (curr._1 + record.other, curr._2 + 1)
      }

      def merge(curr1: (Double, Int), curr2: (Double, Int)): (Double, Int) = {
        (curr1._1 + curr2._1, curr1._2 + curr2._2)
      }

      def finish(r: (Double, Int)): Double = "%.1f".format(r._1 / r._2).toDouble

      override def bufferEncoder: Encoder[(Double, Int)] = Encoders.tuple(Encoders.scalaDouble, Encoders.scalaInt)

      override def outputEncoder: Encoder[Double] = Encoders.scalaDouble
    }.toColumn

    summed
      .groupByKey(tur => (tur.working, tur.sex, tur.age))
      .agg(
        primaryNeedsAggregator.as[Double],
        workAggregator.as[Double],
        otherAggregator.as[Double])
      .map(r => TimeUsageRow(r._1._1, r._1._2, r._1._3, r._2, r._3, r._4))
      .orderBy("working", "age", "sex")
  }

/**
  * Models a row of the summarized data set
  * @param working Working status (either "working" or "not working")
  * @param sex Sex (either "male" or "female")
  * @param age Age (either "young", "active" or "elder")
  * @param primaryNeeds Number of daily hours spent on primary needs
  * @param work Number of daily hours spent on work
  * @param other Number of daily hours spent on other activities
  */
case class TimeUsageRow(
  working: String,
  sex: String,
  age: String,
  primaryNeeds: Double,
  work: Double,
  other: Double
)
