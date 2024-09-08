package observatory

import com.sksamuel.scrimage.implicits.*
import org.apache.spark.sql.SparkSession

import org.apache.log4j.{Level, Logger}

import scala.util.Properties.isWin
import Extraction.*

object Main extends App:

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
  if (isWin) System.setProperty("hadoop.home.dir", System.getProperty("user.dir") + "\\winutils\\hadoop-3.3.1")

  locationYearlyAverageRecords(locateTemperatures(1975, "/stations.csv", "/1975.csv"))
end Main

