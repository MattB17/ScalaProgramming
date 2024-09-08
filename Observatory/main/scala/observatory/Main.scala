package observatory

import com.sksamuel.scrimage.implicits.*
import org.apache.spark.sql.SparkSession

import scala.util.Properties.isWin
import Extraction.*

object Main extends App:

  if (isWin) System.setProperty("hadoop.home.dir", System.getProperty("user.dir") + "\\winutils\\hadoop-3.3.1")

  locateTemperatures(1975, "/stations.csv", "/1975.csv")
end Main

