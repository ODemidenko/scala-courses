package observatory

import java.io.InputStream
import java.time.LocalDate

import scala.io.Source
import java.nio.file._

import scala.collection.JavaConversions._
import org.apache.spark.sql.{DataFrame, Row, SparkSession}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.log4j.{Level, Logger}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.functions.udf
import org.apache.spark.sql.catalyst.expressions.Literal

/**
  * 1st milestone: data extraction
  */

// todo: (third priority)
object Extraction {
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  //todo: bring spark instantiation in a single place
  /**
    * Real-life example is likely to launch session when the app is instantiated
    * val spark=Main.getSparkSession()
    * When application is closed - Session is closed in Main class
    */
  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
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

  //  Работает у меня, локально.
    def fsPath(file:String):String=Paths.get(ClassLoader.getSystemResource(file).toURI).toString
//  def fsPath(file: String): String = Paths.get(getClass.getResource(file).toURI).toString

  // не работает с scala worksheet
  def resStream(file: String)= {
    val resStream = getClass.getResourceAsStream(file)
//    for (line <- resStream) println(line)
  }

  def dataETL(year:Year,stationsFile: String, temperaturesFile: String):DataFrame={
//      withSparkSession {spark=>

      val stations=readStations(fsPath(stationsFile))
      val temperatures=readTemperature(fsPath(temperaturesFile))

      stations.join(temperatures,stations.col("STN")===temperatures.col("STN") &&
       stations.col("WBAN")===temperatures.col("WBAN"))

       .select(lit(year).alias("Year"),col("Month"),col("Day"),col("Latitude"),col("Longitude"),expr("(Temperature-32)*5/9").alias("Temperature"))
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
          .where(col("Temperature")<9999)
  }

  def readStations(fsPath:String):DataFrame={
    val schema = StructType(
    StructField("STN", StringType, true)
    :: StructField("WBAN", StringType, true)
    :: StructField("Latitude",DoubleType,true)
    :: StructField("Longitude",DoubleType,true)
    ::Nil)

      spark.read
        .schema(schema)
        .csv(fsPath)
          .na.drop(List("STN","WBAN","Latitude","Longitude"))
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {

      //попробовать, получение как tuple должно-таки работать!
     dataETL(year,stationsFile, temperaturesFile).rdd.map (row=>{
       val temperature: Double = row.getAs[Double]("Temperature")
       val location: Location = Location(row.getAs[Double]("Latitude"), row.getAs[Double]("Longitude"))
       val localDate: LocalDate = LocalDate.of(row.getAs[Int]("Year"), row.getAs[Int]("Month"), row.getAs[Int]("Day"))
       (localDate, location, temperature)
     }).collect()

  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    sparkAverageRecords(records).collect()
  }

  def sparkAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): RDD[(Location, Temperature)] ={
      spark.sparkContext.parallelize(records.toSeq)
      .map {case(_,loc,temp)=> (loc,temp)}
      .aggregateByKey((0,0d))(
        (u,v)=>(u._1+1,u._2+v),
        (v1,v2)=>(v1._1+v2._1,v1._2+v2._2)
      ).mapValues {case (count,sum)=>sum/count}
  }

//  def locationYearlyAverageRecordsInSpark(year: Year): DataFrame = {
//    val rawYearDF=dataETL(year,"\\stationsFile.csv", s"\\year.csv")
//      rawYearDF.groupBy("Year","Latitude","Longitude").avg("Temperature").as("Temperature")
//  }
}
