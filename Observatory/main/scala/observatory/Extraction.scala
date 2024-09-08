package observatory

import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.types.*
import org.apache.spark.sql.functions.*

import java.time.LocalDate
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface:
  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .master("local")
      .getOrCreate()

  private val stnIdCol = "STN identifier"
  private val wbanIdCol = "WBAN identifier"
  private val latCol = "Latitude"
  private val longCol = "Longitude"
  private val monthCol = "Month"
  private val dayCol = "Day"
  private val tempCol = "Temperature"

  import spark.implicits._
  import scala3encoders.given

  /**
   * 
   * @param stationsFile   Path of the stations resource file to use
   * @return A DataFrame representing the data in stationsFile
   */
  private def loadStations(stationsFile: String): DataFrame = {
    val stationsSchema: StructType = StructType(Seq(
      StructField(stnIdCol, StringType, true),
      StructField(wbanIdCol, StringType, true),
      StructField(latCol, DoubleType, true),
      StructField(longCol, DoubleType, true)
    ))

    val stations = Source.fromInputStream(Extraction.getClass.getResourceAsStream(stationsFile), "utf-8")
    val stationsData = stations.getLines().toList
    stations.close()
    spark.read
      .schema(stationsSchema)
      .csv(spark.sparkContext.parallelize(stationsData).toDS)
      .filter(col(latCol).isNotNull && col(longCol).isNotNull)
  }

  /**
   * 
   * @param temperaturesFile   Path of the temperatures resource file to use
   * @return A DataFrame representing the data in temperaturesFile
   */
  private def loadTemperatures(temperaturesFile: String): DataFrame = {
    val temperatureSchema: StructType = StructType(Seq(
      StructField(stnIdCol, StringType, true),
      StructField(wbanIdCol, StringType, true),
      StructField(monthCol, IntegerType, true),
      StructField(dayCol, IntegerType, true),
      StructField(tempCol, DoubleType, true)
    ))

    val temperatures = Source.fromInputStream(Extraction.getClass.getResourceAsStream(temperaturesFile), "utf-8")
    val tempData = temperatures.getLines().toList
    temperatures.close()
    spark.read
      .schema(temperatureSchema)
      .csv(spark.sparkContext.parallelize(tempData).toDS)
      .withColumn(tempCol, (col(tempCol).cast("double") - lit(32)) * lit(5.toDouble / 9))
  }
  
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year,
                         stationsFile: String,
                         temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val yearCol = "Year"
    
    val stationDF = loadStations(stationsFile)
    val temperaturesDF = loadTemperatures(temperaturesFile)

    val joinedDF = stationDF
      .join(
        temperaturesDF,
        stationDF.col(stnIdCol).eqNullSafe(temperaturesDF.col(stnIdCol)) &&
          stationDF.col(wbanIdCol).eqNullSafe(temperaturesDF.col(wbanIdCol)),
        "inner")
      .withColumn(yearCol, lit(year).cast("integer"))
      .select(yearCol, monthCol, dayCol, latCol, longCol, tempCol)

    joinedDF
      .rdd
      .map(r => (
        LocalDate.of(r.getAs[Int](yearCol), r.getAs[Int](monthCol), r.getAs[Int](dayCol)),
        Location(r.getAs[Double](latCol), r.getAs[Double](longCol)),
        r.getAs[Double](tempCol)))
      .collect()
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    spark.sparkContext.parallelize(records.toSeq)
      .map((lDate, loc, temp) => (loc, temp))
      .groupByKey()
      .mapValues(tempIter => tempIter.sum / tempIter.size)
      .collect()
  }