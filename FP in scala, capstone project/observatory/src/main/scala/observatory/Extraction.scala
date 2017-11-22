package observatory

import java.time.LocalDate

import scala.io.Source
import java.nio.file._

import scala.collection.JavaConversions._
import org.apache.spark.sql.{DataFrame, Row, SparkSession}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.functions.udf
import org.apache.spark.sql.catalyst.expressions.Literal

/**
  * 1st milestone: data extraction
  */
object Extraction {
    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  /**
    * Real-life example is likely to launch session when the app is instantiated
    * val spark=Main.getSparkSession()
    * When application is closed - Session is closed in Main class
    */
  val spark: SparkSession =
      SparkSession
        .builder()
        .appName("Time Usage")
        .config("spark.master", "local")
        .getOrCreate()

//    def withSparkSession[T](f:SparkSession=>T):T=
//    {
//      val spark: SparkSession =
//        SparkSession
//          .builder()
//          .appName("Time Usage")
//          .config("spark.master", "local")
//          .getOrCreate()
//      try(
//        f(spark)
//      )
//      finally( spark.close())
//    }

  /**
    * ignore data coming from stations that have no GPS coordinates
    */
  def fsPath(file:String):String=Paths.get(ClassLoader.getSystemResource(file).toURI).toString

  def dataETL(year:Year,stationsFile: String, temperaturesFile: String)={
//      withSparkSession {spark=>

      val stations=readStations(fsPath(stationsFile))
      val temperatures=readTemperature(fsPath(temperaturesFile))

//      def localDateOf(y:Int,m:Int,d:Int):LocalDate=LocalDate.of(y,m,d)
//      val udfLocalDateOf=udf(localDateOf(_:Int,_:Int,_:Int):LocalDate)

      stations.join(temperatures,stations.col("STN")===temperatures.col("STN") &&
       stations.col("WBAN")===temperatures.col("WBAN"))
//        .select(udfLocalDateOf(lit(year),col("Month"),col("Day")).alias("LocalDate"),col("Latitude"),col("Longitude"),expr("(Temperature-32)*5/9").alias("Temparature"))
   .select(lit(year).alias("year"),col("Month"),col("Day"),col("Latitude"),col("Longitude"),expr("(Temperature-32)*5/9").alias("Temparature"))


//      }
   }

  def readTemperature(fsPath:String):DataFrame={
    val schema = StructType(
      StructField("STN", StringType, true)
        :: StructField("WBAN", StringType, true)
        :: StructField("Month",IntegerType,true)
        :: StructField("Day",IntegerType,true)
        :: StructField("Temperature",DoubleType,false)
        ::Nil)

        spark
          .read
          .schema(schema)
          .csv(fsPath)
  }

  def readStations(fsPath:String):DataFrame={
    val schema = StructType(
    StructField("STN", StringType, true)
    :: StructField("WBAN", StringType, true)
    :: StructField("Latitude",DoubleType,true)
    :: StructField("Longitude",DoubleType,true)
    ::Nil)

      val path = Paths.get(ClassLoader.getSystemResource("stations.csv").toURI()).toString()
      spark.read
        .schema(schema)
        .csv(fsPath)
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {

//    implicit val localDate:(Int,Int,Int)=>LocalDate=LocalDate.of(_,_,_)

//   не поможет: When U is a class, fields for the class will be mapped to columns of the same name (case sensitivity is determined by spark.sql.caseSensitive).
   dataETL(year,stationsFile, temperaturesFile).as[(Int,Int,Int, Location, Temperature)].collect()
     .map {case (y,m,d,l,t)=>(LocalDate.of(y,m,d),l,t)}
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    ???
  }

}
